function(DEQ@SlopeField):
	call	DEQ@LoadStatusAddress
	bit	SlopeFldBit,(hl)
	ret	z
	bit	RealEquBit,(hl)
	ret	z
	inc	hl
	ld	d,(hl)
	call	DEQ@CountEquations
	or	a
	ret	z			;no equations selected
	dec	a
	jp	nz,@Invalid		;more than one equation used during graphing
	call	DEQ@LoadFldres
	bcall	_OP1ToOP6
	bcall	_ConvOP1
	or	d
	ret	z			;fieldres=0
	;E=Fldres_X

	push	de
	AppOnErr(@XUndefinedError)
	call	DEQ@RclT
	AppOffErr
@ContinueRclT:
	rst	18h ;rPUSHREALO1
	;FPS=[X,...]

	;Setup simple cache
	pop	ix
	AppOnErr(@ErrorHandler)
	push	ix

	bcall	_PushRealO6
	ld	a,XMINt
	bcall	_RclSysTok
	rst	08h ;rOP1TOOP2
	ld	a,XMAXt
	bcall	_RclSysTok
	bcall	_FPSub
	bcall	_CpyTo2FPST
	bcall	_FPDiv
	;X_StepSize = (Xmax-Xmin)/Fieldres_X
	bcall	_PopRealO2
	rst	18h ;rPUSHREALO1
	;FPS=[X_StepSize,X,...]

	bcall	_OP2ToOP1
	ld	hl,$757F		;.75 screen proportion is 3:4
	call	DEQ@LoadOP2
	bcall	_FPMult
	bcall	_Int
	;int(Fieldres_X*.75)
	ld	a,(OP1+2)
	or	a
	jr	nz,@ZDSf1
	bcall	_Plus1 ;should not be zero
@ZDSf1:
	rst	18h ;rPUSHREALO1
	bcall	_ConvOP1
	pop	de
	ld	d,a			;D=Fldres_Y E=Fldres_X
	push	de
	;FPS=[Fldres_Y,X_StepSize,X,...]

	ld	a,YMINt
	bcall	_RclSysTok
	rst	08h ;rOP1TOOP2
	ld	a,YMAXt
	bcall	_RclSysTok
	bcall	_FPSub
	bcall	_PopRealO2
	bcall	_FPDiv		;Y_StepSize = (Ymax-Ymin)/Fieldres_Y
	rst	18h ;rPUSHREALO1
	;FPS=[Y_StepSize,X_StepSize,X,...]

	bcall	_CpyTo1FPS1		;X_StepSize
	bcall	_TimesPt5
	rst	08h ;rOP1TOOP2
	ld	a,XMINt
	bcall	_RclSysTok
	rst	30h ;rFPADD
	call	DEQ@StoT 			;X = Xmin+X_StepSize/2

	pop	de
@XLoop:
	push	de
	bcall	_CpyTo1FPST;Y_StepSize
	bcall	_TimesPt5
	rst	08h ;rOP1TOOP2
	ld	a,YMAXt
	bcall	_RclSysTok
	bcall	_FPSub
	;Y = YMax-Y_StepSize/2
	call	@StoreY
	;FPS=[Y,Y_StepSize,X_StepSize,X,...]

	call	DEQ@LoadRKEvalAddress
	ld	b,(hl)
	xor	a
@EquNrLoop:
	sra	b
	jr	c,@EquNrLoopSkip
	inc	a
	jr	@EquNrLoop
@EquNrLoopSkip:
	pop	de
	push	de
	ld	e,a
@YLoop:
	push	de
	call	DEQ@LoadEquation
	AppOnErr(@ParserErrorHandler)
	bcall	_ParseInp
	AppOffErr
	bcall	_CkOP1Real
	jr	nz,@ContinueParser
	rst	18h ;rPUSHREALO1
	;FPS=[Derivative,Y,Y_StepSize,X_StepSize,X,...]

	bcall	_CpyTo1FPS3		;X_StepSize
	rst	18h ;rPUSHREALO1	;FPS=[PlotLength,Derivative,Y,Y_StepSize,X_StepSize,X,...]
	bcall	_CpyTo2FPS1		;Derivative
	bcall	_FPMult
	xor	a
	ld	(OP1),a		;Abs(PlotLength*Derivative)
	bcall	_CpyTo2FPS3		;Y_StepSize
	bcall	_CpOP1OP2
	jr	c,@ZDSf2
	;Abs(X_StepSize*Derivative) > Y_StepSize
	bcall	_CpyTo1FPS3		;Y_StepSize
	bcall	_CpyTo2FPS1		;Derivative
	bcall	_FPDiv
	bcall	_CpyO1ToFPST
	; PlotLength = Y_StepSize/Derivative
@ZDSf2:
	bcall	_PopRealO1
	ld	hl,$257F		;.25
	call	DEQ@LoadOP2
	bcall	_FPMult
	bcall	_OP1ToOP6		;OP6=PlotLength = PlotLength/4
	bcall	_CpyTo2FPST
	bcall	_FPMult
	bcall	_OP1ToOP5		;OP5=PlotLength*Derivative

	call	DEQ@RclT
	bcall	_OP1ToOP4		;OP4=X
	bcall	_OP6ToOP2		;PlotLength
	bcall	_FPSub
	bcall	_OP1ToOP3		;OP3=X-PlotLength

	bcall	_OP4ToOP1		;X
	bcall	_OP6ToOP2		;PlotLength
	rst	30h ;rFPADD
	bcall	_OP1ToOP6		;OP6=X+PlotLength

	bcall	_CpyTo1FPS1		;Y
	bcall	_OP5ToOP2		;PlotLength*Derivative
	bcall	_FPSub
	bcall	_OP1ToOP4		;OP4=Y-PlotLength

	bcall	_CpyTo1FPS1		;Y
	bcall	_OP5ToOP2		;PlotLength*Derivative
	rst	30h ;rFPADD
	rst	08h ;rOP1TOOP2	;OP2=Y+PlotLength

	bcall	_OP6ToOP1		;X+PlotLength

	bcall	_CLine
;Line(Y-PlotLength*Derivative,
;     X+PlotLength,Y+PlotLength*Derivative)

	ld	de,9
	bcall	_DeallocFPS1	;FPS=[Y,Y_StepSize,X_StepSize,X,...]

@ContinueParser:
	bcall	_PopRealO1
	bcall	_CpyTo2FPST
	bcall	_FPSub
	call	@StoreY		;Y -= Y_StepSize
	pop	de
	dec	d
	jp	nz,@YLoop

	ld	de,9
	bcall	_DeallocFPS1

	call	DEQ@RclT
	bcall	_CpyTo2FPS1
	rst	30h ;rFPADD
	call	DEQ@StoT			;X += X_StepSize
	pop	de
	dec	e
	jp	nz,@XLoop

	ld	de,9*2
	bcall	_DeallocFPS1
	AppOffErr
@Continue:
	call	DEQ@LoadSimpleCacheAddress
	res	cacheSimpleValidBit,(hl)
	bcall	_PopRealO1
	call	DEQ@StoT
	ret

@Invalid:
	bjump	_ErrInvalid

@XUndefinedError:
	bcall	_OP1Set0
	jp	@ContinueRclT

@StoreY:
	rst	18h ;rPUSHREALO1
	call	DEQ@RclT
	bcall	_OP1ToOP6
	call	DEQ@LoadRKEvalAddress
	ld	d,(hl)
	call	DEQ@Runge@SaveYCaches
	ret

@ErrorHandler:
	ld	b,a
	and	$7F
	cp	8			;only ignore the first errors
	jr	c,@Continue
@JumpError:
	push	bc
	call	DEQ@LoadSimpleCacheAddress
	res	cacheSimpleValidBit,(hl)
	ld	de,StatusOffset-2
	add	hl,de
	pop	bc
	jp	DisplayOriginalError

@ParserErrorHandler:
	ld	b,a
	and	$7F
	cp	6			;ON:BREAK
	jr	z,@JumpError
	cp	8			;only ignore the first errors
	jr	c,@ContinueParser
	jr	@JumpError
