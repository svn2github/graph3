SlopeField:
	call load_status_address
	bit SlopeFldBit,(hl)
	ret z
	bit RealEquBit,(hl)
	ret z
	inc hl
	ld d,(hl)
	call CountEquations
	or a
	ret z;no equations selected
	dec a
	jr nz,SlopeField_Invalid;more than one equation used during graphing
	call load_fldres
	B_CALL OP1ToOP6
	B_CALL ConvOP1
	or d
	ret z;fieldres=0
	;E=Fldres_X

	push de
	AppOnErr SlopeField_XUndefinedError
	call RclT
	AppOffErr
SlopeField_ContinueRclT:
	rst rPUSHREALO1
	;FPS=[X,...]

	;Setup simple cache
	pop ix
	AppOnErr SlopeField_ErrorHandler
	push ix

	B_CALL PushRealO6
	ld a,XMINt
	B_CALL RclSysTok
	rst rOP1TOOP2
	ld a,XMAXt
	B_CALL RclSysTok
	B_CALL FPSub
	B_CALL CpyTo2FPST
	B_CALL FPDiv
	;X_StepSize = (Xmax-Xmin)/Fieldres_X
	B_CALL PopRealO2
	rst rPUSHREALO1
	;FPS=[X_StepSize,X,...]

	B_CALL OP2ToOP1
	ld hl,75h*256+7Fh;.75 screen proportion is 3:4
	call LoadOP2
	B_CALL FPMult
	B_CALL Int
	;int(Fieldres_X*.75)
	ld a,(OP1+2)
	or a
	jr nz,$f
	B_CALL Plus1 ;should not be zero
$$:
	rst rPUSHREALO1
	B_CALL ConvOP1
	pop de
	ld d,a;D=Fldres_Y E=Fldres_X
	push de
	;FPS=[Fldres_Y,X_StepSize,X,...]

	ld a,YMINt
	B_CALL RclSysTok
	rst rOP1TOOP2
	ld a,YMAXt
	B_CALL RclSysTok
	B_CALL FPSub
	B_CALL PopRealO2
	B_CALL FPDiv ;Y_StepSize = (Ymax-Ymin)/Fieldres_Y
	rst rPUSHREALO1
	;FPS=[Y_StepSize,X_StepSize,X,...]

	B_CALL CpyTo1FPS1;X_StepSize
	B_CALL TimesPt5
	rst rOP1TOOP2
	ld a,XMINt
	B_CALL RclSysTok
	rst rFPADD
	call StoT ;X = Xmin+X_StepSize/2

	pop de
SlopeField_XLoop:
	push de
	B_CALL CpyTo1FPST;Y_StepSize
	B_CALL TimesPt5
	rst rOP1TOOP2
	ld a,YMAXt
	B_CALL RclSysTok
	B_CALL FPSub
	;Y = YMax-Y_StepSize/2
	call SlopeField_StoreY
	;FPS=[Y,Y_StepSize,X_StepSize,X,...]

	call load_RKEval_address
	ld b,(hl)
	xor a
SlopeField_EquNrLoop:
	sra b
	jr c,SlopeField_EquNrLoopSkip
	inc a
	jr SlopeField_EquNrLoop
SlopeField_EquNrLoopSkip:
	pop de
	push de
	ld e,a
SlopeField_YLoop:
	push de
	call load_equation
	apponerr SlopeField_ParserErrorHandler
	B_CALL ParseInp
	appofferr
	B_CALL CkOP1Real
	jr nz,SlopeField_ContinueParser
	rst rPUSHREALO1
	;FPS=[Derivative,Y,Y_StepSize,X_StepSize,X,...]

	B_CALL CpyTo1FPS3 ;X_StepSize
	rst rPUSHREALO1 ;FPS=[PlotLength,Derivative,Y,Y_StepSize,X_StepSize,X,...]
	B_CALL CpyTo2FPS1 ;Derivative
	B_CALL FPMult
	xor a
	ld (OP1),a;Abs(PlotLength*Derivative)
	B_CALL CpyTo2FPS3 ;Y_StepSize
	B_CALL CpOP1OP2
	jr c,$f
	;Abs(X_StepSize*Derivative) > Y_StepSize
	B_CALL CpyTo1FPS3 ;Y_StepSize
	B_CALL CpyTo2FPS1 ;Derivative
	B_CALL FPDiv
	B_CALL CpyO1ToFPST
	; PlotLength = Y_StepSize/Derivative
$$:
	B_CALL PopRealO1
	ld hl,25h*256+7Fh;.25
	call LoadOP2
	B_CALL FPMult 
	B_CALL OP1ToOP6 ;OP6=PlotLength = PlotLength/4
	B_CALL CpyTo2FPST
	B_CALL FPMult 
	B_CALL OP1ToOP5 ;OP5=PlotLength*Derivative

	call RclT
	B_CALL OP1ToOP4 ;OP4=X
	B_CALL OP6ToOP2 ;PlotLength
	B_CALL FPSub
	B_CALL OP1ToOP3 ;OP3=X-PlotLength
	
	B_CALL OP4ToOP1 ;X
	B_CALL OP6ToOP2 ;PlotLength
	rst rFPADD
	B_CALL OP1ToOP6 ;OP6=X+PlotLength

	B_CALL CpyTo1FPS1 ;Y
	B_CALL OP5ToOP2 ;PlotLength*Derivative
	B_CALL FPSub
	B_CALL OP1ToOP4 ;OP4=Y-PlotLength

	B_CALL CpyTo1FPS1 ;Y
	B_CALL OP5ToOP2 ;PlotLength*Derivative
	rst rFPADD
	rst rOP1TOOP2 ;OP2=Y+PlotLength

	B_CALL OP6ToOP1 ;X+PlotLength

	B_CALL CLine	
;Line(Y-PlotLength*Derivative,
;     X+PlotLength,Y+PlotLength*Derivative)

	ld de,9
	B_CALL DeallocFPS1 ;FPS=[Y,Y_StepSize,X_StepSize,X,...]

SlopeField_ContinueParser:	
	B_CALL PopRealO1
	B_CALL CpyTo2FPST
	B_CALL FPSub
	call SlopeField_StoreY;Y -= Y_StepSize
	pop de
	dec d
	jr nz,SlopeField_YLoop

	ld de,9
	B_CALL DeallocFPS1

	call RclT
	B_CALL CpyTo2FPS1
	rst rFPADD
	call StoT;X += X_StepSize
	pop de
	dec e
	jr nz,SlopeField_XLoop

	ld de,9*2
	B_CALL DeallocFPS1
	AppOffErr
SlopeField_Continue:
	call load_simple_cache_address
	res cacheSimpleValidBit,(hl)
	B_CALL PopRealO1
	call StoT
	ret

SlopeField_Invalid:
	B_JUMP ErrInvalid

SlopeField_XUndefinedError:
	B_CALL OP1Set0
	jr SlopeField_ContinueRclT

SlopeField_StoreY:
	rst rPUSHREALO1
	call RclT
	B_CALL OP1ToOP6
	call load_RKEval_address
	ld d,(hl)
	call runge_save_y_caches
	ret

SlopeField_ErrorHandler:
	ld b,a
	and 7Fh
	cp 8 ;only ignore the first errors
	jr c,SlopeField_Continue
SlopeField_JumpError:
	push bc
	call load_simple_cache_address
	res cacheSimpleValidBit,(hl)
	ld de,StatusOffset-2
	add hl,de
	pop bc
	jp DisplayOriginalError

SlopeField_ParserErrorHandler:
	ld b,a
	and 7Fh
	cp 6;ON:BREAK
	jr z,SlopeField_JumpError
	cp 8;only ignore the first errors
	jr c,SlopeField_ContinueParser
	jr SlopeField_JumpError
