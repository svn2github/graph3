function(Runge):
	push	de
	call	LookupAppVar
	ld	hl,RKEvalOffset
	add	hl,de
	pop	de
	ld	d,(hl)
	push	de
	call	LoadSimpleCacheAddress
	bit	cacheSimpleValidBit,(hl)
	jr	z,@SkipSimpleCache
	inc	hl
	push	hl
	bcall	_CpyTo1FPST
	pop	hl
	push	hl
	bcall	_Mov9OP2Cp
	pop	hl
	jr	nz,@SkipSimpleCache
	pop	de
	dec	e			;skip X
	call	@HLPlusCacheOffset
	rst	20h ;rMOV9TOOP1
	rst	08h ;rOP1TOOP2
	ret
@SkipSimpleCache:
	call	@LookupEndpointCache
	ld	a,cache1ValidMask | cache2ValidMask
	and	(hl)
	cp	cache1ValidMask | cache2ValidMask
	jr	nz,@SkipEndpointCache
	;both are valid
	inc	hl
	push	hl
	bcall	_CpyTo1FPST
	pop	hl
	push	hl
	bcall	_Mov9OP2Cp
	pop	hl
	jp	z,@ReturnInterpolate
	sbc	a,a
	and	1			;A=1 if X_Cache1>X_target ;A=0 if X_Cache1<X_target
	ld	bc,endpointCacheBlockSize
	add	hl,bc
	push	af
	bcall	_Mov9OP2Cp
	pop	bc
	jp	z,@ReturnInterpolate
	sbc	a,a
	and	1			;A=1 if X_Cache2>X_target ;A=0 if X_Cache2<X_target
	xor	b			;check if they're different
	jp	nz,@ReturnInterpolate
@SkipEndpointCache:
	;try to save some steps by using cache
	bcall	_CpyTo2FPST
	ld	a,X0
	bcall	_RclSysTok
	bcall	_OP1ExOP2
	bcall	_FPSub		;X_target-X0
	ld	hl,OP1
	ld	a,(hl)
	and	$80
	pop	de
	or	d
	ld	d,a
	push	de

	ld	e,0
	push	de			;for use in cache checking with destroyed e value
	ld	(hl),0
	rst	18h ;rPUSHREALO1	;FPS=[abs(X_target-X0),X_target,...]
	call	@LookupEndpointCache
	bit	cache1ValidBit,(hl)
	jr	z,@SkipEndpointCache1
	pop	de
	push	de
	push	hl
	inc	hl
	call	@CheckCache
	pop	hl
	jr	nz,@SkipEndpointCache1
	pop	de
	inc	e
	push	de
@SkipEndpointCache1:
	bit	cache2ValidBit,(hl)
	jr	z,@SkipEndpointCache2
	ld	bc,1 + endpointCacheBlockSize
	add	hl,bc
	pop	de
	push	de
	call	@CheckCache
	jr	nz,@SkipEndpointCache2
	pop	de
	ld	e,2
	push	de
@SkipEndpointCache2:
	bcall	_PopRealO1
	pop	de
	ld	a,e			;E=0 use X0, E=1 use Cache1, E=2 use Cache2
	or	a
	jr	z,@LoadFromInitialValues
	push	af
	call	@LookupEndpointCache
	inc	hl
	pop	af
	cp	2
	jr	nz,@ZDSf1
	ld	bc,endpointCacheBlockSize
	add	hl,bc
@ZDSf1:
	;HL=starting point of endpointCacheBlock
	push	hl
	ld	hl,2*9
	bcall	_AllocFPS1
	pop	de

	bcall	_CpyToFPS1		;X
	ex	de,hl
	bcall	_CpyToFPST		;step
	push	de
	pop	ix			;save (FPS)

	ex	de,hl
	ld	bc,-9
	add	hl,bc
	pop	af
	push	af
	and	80h
	ld	(hl),a		;correct stepsize direction

	;load Y*
	pop	bc
	push	bc
	ex	de,hl
	call	@LoadCache
	;load f1*
	pop	bc
	push	bc
	call	@LoadCache

	jr	@LoopF1Known
@LoadFromInitialValues:
	ld	a,X0
	bcall	_RclSysTok
	rst	18h ;rPUSHREALO1
	bcall	_OP1ToOP6
	call	StoT			;starting point
	ld	a,Xstep
	bcall	_RclSysTok
	pop	af
	push	af
	and	$80
	ld	(OP1),a
	rst	18h ;rPUSHREALO1;stepsize

	ld	hl,(fps)
	push	hl
	pop	ix			;save (FPS)

	pop	de
	push	de
	call	@LoadY0

	;start RK
	;FPS=[Y*,step,X,X_target,...]
	pop	de
	push	de
	call	@SaveYCaches

	pop	de
	push	de
	call	@ExecuteEqu		;FPS=[f1*,Y*,step,X,X_target,...]

	pop	de
	push	de
	call	@SaveEndpointCache;save initial values in cache for easy interpolation

@LoopF1Known:
	;Check if stepsize is large enough
	;IX=[Y*,step,X,X_target,...]
	push	ix
	pop	hl
	ld	de,-9
	add	hl,de
	bcall	_Mov9ToOP2
	push	ix
	pop	hl
	ld	de,-9*2
	add	hl,de
	rst	20h ;rMOV9TOOP1
	bcall	_OP1ToOP3
	rst	30h ;rFPADD		;X+step
	bcall	_OP3ToOP2
	bcall	_FPSub		;(X+step)-X
	ld	a,(OP1+1)
	cp	$80-14		;E-14
	jr	nc,@ZDSf2
	bjump	_ErrTolTooSmall
@ZDSf2:

	pop	de
	push	de
	ld	a,$50
	call	@UpdateXY

	pop	de
	push	de
	call	@ExecuteEqu		;FPS=[f2*,f1*,Y*,step,X,X_target,...]

	pop	de
	push	de
	ld	a,$75
	call	@UpdateXY

	pop	de
	push	de
	call	@ExecuteEqu		;FPS=[f3*,f2*,f1*,Y*,step,X,X_target,...]

	pop	de
	push	de
	call	@UpdateFinalXY	;FPS=[Yn+1*,f3*,f2*,f1*,Y*,step,X,X_target,...]

	pop	de
	push	de
	call	@ExecuteEqu		;FPS=[f4*,Yn+1*,f3*,f2*,f1*,Y*,step,X,X_target,...]

	pop	de
	push	de
	call	@CalcErrest		;FPS=[errest,f4*,Yn+1*,f1*,Y*,step,X,X_target,...]

	ld	a,(OP1M)
	or	a
	jr	nz,@NoDouble
	;zero error, double stepsize
	push	ix
	pop	hl
	ld	bc,-9
	add	hl,bc
	rst	20h ;rMOV9TOOP1
	bcall	_OP2Set2
	bcall	_FPMult
	jr	@GuessSkip
@NoDouble:
	rst	08h ;rOP1TOOP2
	call	LoadDiftol
	bcall	_FPDiv
	ld	hl,$257F		;.25
	call	LoadOP2
	bcall	_YToX
	ld	hl,$907F		;.9
	call	LoadOP2
	bcall	_FPMult
	push	ix
	pop	hl
	ld	bc,-9
	add	hl,bc
	bcall	_Mov9ToOP2
	bcall	_FPMult
	;.9*step*(tol/abs(errest))^(1/4)
@GuessSkip:
	bcall	_PopRealO2
	rst	18h ;rPUSHREALO1	;FPS=[step_guess,f4*,Yn+1*,f1*,Y*,step,X,X_target,...]
	;OP2=errest
	
	call	LoadDiftol
	bcall	_CpOP1OP2
	push	af
	ld	hl,$2080		;2
	jr	nc,@ErrestSkip
	ld	hl,$507F		;.5
@ErrestSkip:
	call	LoadOP2
	push	ix
	pop	hl
	ld	bc,-9
	add	hl,bc
	rst	20h ;rMOV9TOOP1	;step
	bcall	_FPMult
	bcall	_PopRealO2		;step_guess
	xor	a
	ld	(OP1),a		;absolute values
	ld	(OP2),a
	bcall	_Min
	;min(.5h,guess) or min(2h,guess)

	push	ix
	pop	hl
	ld	bc,-9
	add	hl,bc
	bcall	_Mov9ToOP2		;step
	pop	bc
	pop	af
	push	af
	push	bc
	and	$80
	ld	(OP1),a
	push	ix
	pop	hl
	ld	bc,9
	or	a
	sbc	hl,bc
	ld	de,OP1
	ex	de,hl
	ldir				;update step
	
	pop	af
	jr	c,@Retry		;error too big,retry step
	;step succeeded
	;FPS=[f4*,Yn+1*,f1*,Y*,step,X,X_target,...]
	push	ix
	pop	hl
	ld	bc,-9*2
	add	hl,bc
	push	hl
	rst	20h ;rMOV9TOOP1	;X
	rst	30h ;rFPADD
	pop	de
	ld	hl,OP1
	call	Mov9			;update X
	push	ix
	pop	hl
	ld	bc,-9*3
	add	hl,bc
	ld	de,OP2
	call	Mov9			;X_target
	pop	af
	push	af
	and	$80
	jr	z,@ZDSf3
	bcall	_OP1ExOP2		;correct for negative stepsize
@ZDSf3:
	bcall	_CpOP1OP2
	pop	de
	push	de
	push	af
	;FPS=[f4*,Yn+1*,f1*,Y*,step,X,X_target,...]
	ld	bc,2*256+2
	call	@DeallocMiddleFPS
	;FPS=[f4*,Yn+1*,step_next,X+step,X_target,...]
	pop	af
	pop	de
	push	de
	push	af
	call	@SaveEndpointCache
	pop	af
	jr	z,@ExitLoop		;X+step=X_target
	jp	c,@LoopF1Known	;X+step<X_target
@ExitLoop:	
	;end RK
	;invalidate cache after RK is finished
	call	LoadSimpleCacheAddress
	res	cacheSimpleValidBit,(hl)

	pop	de
	push	de
	call	CountEquations
	add	a,a
	add	a,2			;*2+2
	call	DeallocFPSA
	;FPS=[X_target,...]

@ReturnInterpolate:
	pop	de
	jp	@Interpolate
	;ret

@Retry:
	;FPS=[f4*,Yn+1*,f1*,Y*,step_updated,X,X_target,...]
	pop	de
	push	de
	call	CountEquations
	add	a,a			;remove both f4* and Yn+1*
	call	DeallocFPSA
	;FPS=[f1*,Y*,step_updated,X,X_target,...]
	jp	@LoopF1Known

@MultAdd@DE:
	push	de
	bcall	_Mov9ToOP2
	pop	hl
	jr	@MultAdd
@MultAdd@Pop:
	push	hl
	bcall	_PopRealO2
	pop	hl
function(Runge@MultAdd):
	rst	20h ;rMOV9TOOP1
	bcall	_FPMult
	bcall	_OP4ToOP2
	rst	30h ;rFPADD
	ret	

function(Runge@ExecuteEqu):
	rlc	d			;skip first two bits
	rlc	d
	ld	e,5
@Loop:
	rlc	d
	jr	nc,@SkipExec
	push	de
	call	LoadEquation
	push	ix
	bcall	_ParseInp
	pop	ix
	bcall	_CkOP1Real
	jp	nz,ParserHook@ArgumentError
	rst	18h ;rPUSHREALO1	;fx
	pop	de
@SkipExec:
	dec	e
	ld	a,-1			;0..5 are the equations
	cp	e
	jr	nz,@Loop
	ret

function(Runge@CalcErrest):
	;FPS=[f4*,Yn+1*,f3*,f2*,f1*,Y*,step,X,X_target,...]
	;IX=[Y*,step,X]
	push	de

	bcall	_OP5Set0

	ld	hl,(FPS)
	pop	de
	ld	e,0
@Loop:
	push	de
	ld	b,e
	inc	b
@ZDSb1:
	rrc	d
	djnz	@ZDSb1
	pop	de
	jr	nc,@Skip

	ld	bc,-9
	add	hl,bc
	push	hl
	push	de
	push	hl			;stack=[HL,DE,HL,...]
	bcall	_Mov9ToOP2		;f4
	ld	hl,Runge@F4Err
	rst	20h ;rMOV9TOOP1
	bcall	_FPMult
	bcall	_OP1ToOP4	

	pop	hl
	pop	de
	push	de
	call	Runge@HLMinusEquNrTimes9
	pop	de
	push	de
	call	Runge@HLMinusEquNrTimes9
	push	hl
	ld	de,Runge@F3Err
	call	Runge@MultAdd@DE
	bcall	_OP1ToOP4

	pop	hl
	pop	de
	push	de
	call	Runge@HLMinusEquNrTimes9
	push	hl
	ld	de,Runge@F2Err
	call	Runge@MultAdd@DE
	bcall	_OP1ToOP4

	pop	hl
	pop	de
	push	de
	call	Runge@HLMinusEquNrTimes9
	ld	de,Runge@F1Err
	call	Runge@MultAdd@DE
	
	push	ix
	pop	hl
	ld	de,-9
	add	hl,de
	bcall	_Mov9ToOP2		;step
	bcall	_FPMult
	xor	a
	ld	(OP1),a		;abs
	;OP1=abs(h(5/72*f1-1/12*f2-1/9*f3+1/8*f4))  the error estimate
	;FPS=[f4*,Yn+1*,f3*,f2*,f1*,Y*,step,X,X_target,...]

	bcall	_OP5ToOP2
	bcall	_Max
	bcall	_OP1ToOP6
	pop	de
	pop	hl
@Skip:
	inc	e
	ld	a,6
	cp	e
	jr	nz,@Loop

	ld	bc,2*256+2
	call	Runge@DeallocMiddleFPS	;delete f2* and f3* keep Yn+1* and f4*
	
	rst	18h ;rPUSHREALO1
	;FPS=[errest,f4*,Yn+1*,f1*,Y*,step,X,X_target,...]
	ret

function(Runge@UpdateXY):
	push	de
	push	af
	
	;calculate h*a*0.1 here
	push	ix
	pop	hl
	ld	de,-9
	add	hl,de
	rst	20h ;rMOV9TOOP1
	pop	hl
	ld	l,$7F
	call	LoadOP2
	;OP2=a*0.1
	bcall	_FPMult
	bcall	_OP1ToOP3		;h*a*0.1

	rst	08h ;rOP1TOOP2
	push	ix
	pop	hl
	ld	de,-9*2
	add	hl,de
	rst	20h ;rMOV9TOOP1
	rst	30h ;rFPADD
	bcall	_OP1ToOP6
	call	StoT			;X=X+h*a*0.1
	bcall	_OP3ToOP4		;h*a*0.1

	pop	de
	push	de
	call	CountEquations
	call	MultABy9
	ld	hl,(FPS)
	ld	b,0
	ld	c,a
	or	a
	sbc	hl,bc
	pop	de
	ld	e,5
	res	6,d
	res	7,d
	rlc	d
	rlc	d			;skip first two bits
@Loop:
	;D contains eval bits
	;E contains equ nr
	;HL contains address of fx
	rlc	d
	jr	nc,@Skip

	push	hl
	push	de

	;IX=[Y*,step,X]
	rst	20h ;rMOV9TOOP1	;fx
	bcall	_OP4ToOP2		;h*a*0.1
	bcall	_FPMult
	rst	08h ;rOP1TOOP2

	push	ix
	pop	hl
	pop	de
	push	de

	ld	a,6
	sub	e
	ld	b,a			;5-e
	xor	a
@InnerLoop:
	rrc	d
	adc	a,0
	djnz	@InnerLoop
	dec	a
	call	Runge@HLPlusATimes9
	
	rst	20h ;rMOV9TOOP1
	rst	30h ;rFPADD		;Y+h*a*0.1*fx
	rst	18h ;rPUSHREALO1

	pop	de
	pop	hl
	ld	bc,9
	add	hl,bc
@Skip:
	dec	e
	ld	a,-1			;0..5 are the equations
	cp	e
	jr	nz,@Loop
	push	de
	call	Runge@SaveYCaches
	pop	de
	call	CountEquations
DeallocFPSA:
	ld	l,a
	ld	h,0
	bcall	_DeallocFPS
	ret

function(Runge@UpdateFinalXY):
	;FPS=[f3*,f2*,f1*,Y*,step,X,X_target,...]
	;IX=[Y*,step,X]
	push	de

	push	ix
	pop	hl
	ld	de,-9
	add	hl,de
	rst	20h ;rMOV9TOOP1	;step
	push	ix
	pop	hl
	ld	de,-9*2
	add	hl,de
	bcall	_Mov9ToOP2		;X
	rst	30h ;rFPADD		;X+h
	bcall	_OP1ToOP6
	call	StoT

	pop	de
	push	de
	ld	hl,(FPS)
	call	Runge@HLMinusEquNrTimes9
	pop	de
	ld	e,5

@Loop:
	push	hl
	push	de
	ld	b,e
	inc	b
@ZDSb2:
	rrc	d
	djnz	@ZDSb2
	pop	de
	pop	hl
	jr	nc,@Skip

	push	hl
	push	de
	push	hl			;stack=[HL,DE,HL,...]
	
	bcall	_Mov9ToOP2
	ld	hl,Runge@F3Inc
	rst	20h ;rMOV9TOOP1	
	bcall	_FPMult
	bcall	_OP1ToOP4

	pop	hl
	pop	de
	push	de
	call	Runge@HLMinusEquNrTimes9
	push	hl
	ld	de,Runge@F2Inc
	call	Runge@MultAdd@DE
	bcall	_OP1ToOP4

	pop	hl
	pop	de
	push	de
	call	Runge@HLMinusEquNrTimes9
	push	hl
	ld	de,Runge@F1Inc
	call	Runge@MultAdd@DE

	push	ix
	pop	hl
	ld	de,-9
	add	hl,de
	bcall	_Mov9ToOP2		;step
	bcall	_FPMult

	pop	hl
	pop	de
	push	de
	call	Runge@HLMinusEquNrTimes9
	bcall	_Mov9ToOP2		;Y
	rst	30h ;rFPADD		;Y+h(2/9*f1+1/3*f2+4/9*f3)
	rst	18h ;rPUSHREALO1	;FPS=[Yn+1*,f3*,f2*,f1*,Y*,step,X,X_target,...]
	pop	de
	pop	hl
	ld	bc,9
	add	hl,bc
@Skip:
	dec	e
	ld	a,-1
	cp	e
	jr	nz,@Loop
	call	Runge@SaveYCaches
	ret

;def interpolate(xa,ya,fa,xb,yb,fb,x):
;    #cubic hermite interpolation using divided differences
;    #see: http://www.esm.psu.edu/courses/emch407/njs/notes02/ch3_2.doc
;    divider=xb-xa*1.0
;    row3   =(yb -ya   )/divider
;    row4   =(fb  -row3)/divider
;    row3   =(row3-fa  )/divider
;    row4   =(row4-row3)/divider
;    result =ya+(x-xa)*(fa+(x-xa)*(row3+(x-xb)*row4))
;    return result
function(Runge@Interpolate):
	;FPS=[X_target,...]
	push	de
	call	Runge@LookupEndpointCache
	inc	hl			;assume both caches are valid
	pop	de
	push	de
	push	hl
	call	Runge@InterpolateLoadCache
	;FPS=[Xa,Ya,Fa,X_target,...]
	pop	hl
	pop	de
	ld	bc,endpointCacheBlockSize
	add	hl,bc
	call	Runge@InterpolateLoadCache
	;FPS=[Xb,Yb,Fb,Xa,Ya,Fa,X_target,...]

	bcall	_CpyTo1FPST		;Xb
	bcall	_CpyTo2FPS3		;Xa
	bcall	_FPSub
	ld	hl,(FPS)
	push	hl
	rst	18h ;rPUSHREALO1	;divider=Xb-Xa
	;FPS=[divider,Xb,Yb,Fb,Xa,Ya,Fa,X_target,...]

	bcall	_CpyTo1FPS2		;Yb
	bcall	_CpyTo2FPS5		;Ya
	pop	hl
	push	hl
	call	@InterpolateCalc
	rst	18h ;rPUSHREALO1	;row3=(yb-ya)/divider
	;FPS=[row3,divider,Xb,Yb,Fb,Xa,Ya,Fa,X_target,...]

	rst	08h ;rOP1TOOP2	;row3
	bcall	_CpyTo1FPS4		;Fb
	pop	hl
	push	hl
	call	@InterpolateCalc
	rst	18h ;rPUSHREALO1	;row4=(fb-row3)/divider
	;FPS=[row4,row3,divider,Xb,Yb,Fb,Xa,Ya,Fa,X_target,...]

	bcall	_CpyTo1FPS1		;row3
	bcall	_CpyTo2FPS8		;Fa
	pop	hl
	push	hl
	call	@InterpolateCalc
	bcall	_CpyO1ToFPS1	;row3=(row3-fa)/divider

	rst	08h ;rOP1TOOP2	;row3
	bcall	_CpyTo1FPST		;row4
	pop	hl
	call	@InterpolateCalc
	bcall	_CpyO1ToFPST	;row4=(row4-row3)/divider

	bcall	_CpyTo1FPS9		;X_target
	bcall	_CpyTo2FPS3		;Xb
	bcall	_FPSub
	bcall	_PopRealO2		;row4
	bcall	_FPMult
	bcall	_PopRealO2		;row3
	rst	30h ;rFPADD
	rst	18h ;rPUSHREALO1	;row3+(X_target-Xb)*row4
	;FPS=[row3+(X_target-Xb)*row4,divider,Xb,Yb,Fb,Xa,Ya,Fa,X_target,...]

	bcall	_CpyTo1FPS8		;X_target
	bcall	_CpyTo2FPS5		;Xa
	bcall	_FPSub
	bcall	_PopRealO2		;row3+(X_target-Xb)*row4
	rst	18h ;rPUSHREALO1	;X_target-Xa
	bcall	_FPMult
	bcall	_CpyTo2FPS7		;Fa
	rst	30h ;rFPADD
	;OP1=Fa+(X-Xa)*(row3+(X-Xb)*row4)
	;FPS=[X_target-Xa,divider,Xb,Yb,Fb,Xa,Ya,Fa,X_target,...]
	
	bcall	_PopRealO2		;X_target-Xa
	bcall	_FPMult
	bcall	_CpyTo2FPS5		;Ya
	rst	30h ;rFPADD
	rst	08h ;rOP1TOOP2
	;OP2=Ya+(X_target-Xa)*(Fa+(X_target-Xa)*(row3+(X_target-Xb)*row4))

	ld	a,7
	jp	DeallocFPSA		;FPS=[X_target,...]
	;DeallocFPSA will return to caller
	
@InterpolateCalc:
	push	hl
	bcall	_FPSub
	pop	hl
	bcall	_Mov9ToOP2
	bcall	_FPDiv
	ret

function(Runge@InterpolateLoadCache):
	push	de
	push	hl
	ld	hl,3*9
	bcall	_AllocFPS1
	pop	de
	bcall	_CpyToFPST
	pop	de
	dec	e			;skip step
	call	Runge@HLPlusCacheOffset
	ex	de,hl
	bcall	_CpyToFPS1
	ld	bc,5*9		;skip 5 floats
	add	hl,bc
	ex	de,hl
	bcall	_CpyToFPS2
	ret

endpointCacheBlockSize	= (2+6+6)*9
;ENDPOINT CACHE BLOCK:
;0..8		X
;9..17	step
;18..71	Y6..Y1 (reverse order!)
;72..125	f1* (for Y6..Y1) (reverse order!)

endpointCacheSize		= 1+endpointCacheBlockSize*2 ;X and Y1..Y6
;ENDPOINT CACHE:
;0				statusbits
;1..126			cache block 1
;127..252			cache block 1


function(Runge@LookupEndpointCache):
	call LookupAppVar
	ex de,hl
	ld bc,2+simpleCacheSize
	add hl,bc
	ret

function(Runge@SaveEndpointCache):
	;FPS=[f4*,Yn+1*,step,X,X_target,...]
	push	de
	call	Runge@LookupEndpointCache

	ld	a,(hl)
	xor	cacheSwitchMask
	bit	cacheSwitchBit,a
	jr	z,@1
	or	cache2ValidMask
	ld	(hl),a
	ld	de,endpointCacheBlockSize+1
	add	hl,de			;skip first cache
	jr	@2
@1:
	or	cache1ValidMask
	ld	(hl),a
	inc	hl
@2:

	ex	de,hl
	push	ix
	pop	hl
	ld	bc,-9*2
	add	hl,bc
	call	Mov9			;copy X
	call	Mov9			;copy step

	ex	de,hl
	pop	de
	push	hl
	push	de
	ld	hl,(FPS)
	call	Runge@HLMinusEquNrTimes9
	pop	de
	push	de
	call	Runge@HLMinusEquNrTimes9
	pop	bc
	pop	de
	push	bc
	call	Runge@SaveInAppvar;Save Y*

	ex	de,hl
	pop	de
	push	hl
	push	de
	ld	hl,(FPS)
	call	Runge@HLMinusEquNrTimes9
	pop	bc
	pop	de
	call	Runge@SaveInAppvar;Save Y*
	ret

function(Runge@SaveYCaches):
	push	de
	call	LoadSimpleCacheAddress
	ld	a,cacheSimpleValidMask
	or	(hl)
	ld	(hl),a
	inc	hl
	
	ex	de,hl
	ld	hl,OP6
	call	Mov9			;copy X	

	ex	de,hl
	pop	de
	push	hl
	push	de
	ld	hl,(FPS)
	call	Runge@HLMinusEquNrTimes9
	pop	bc
	pop	de
	;fallthrough
function(Runge@SaveInAppvar):
	;DE contains address in appvar
	;HL contains address of first value on fps
	;B  contains equation bits
	ld	c,5
	rlc	b
	rlc	b			;skip first two bits
@Loop:
	rlc	b
	push	bc
	jr	nc,@DontSave
	call	Mov9
	jr	@Skip
@DontSave:
	ld	bc,9
	ex	de,hl
	add	hl,bc
	ex	de,hl
@Skip:
	pop	bc
	dec	c
	ld	a,$FF
	cp	c
	jr	nz,@Loop
	;DE is start+6*9
	;HL is address of last value copied
	ret

function(Runge@CheckCache):	;check cache at HL, Z if cache is useful
	push	de
	bcall	_Mov9ToOP2		;X_Cache
	bcall	_CpyTo1FPS1		;X_target
	bcall	_FPSub
	pop	de
	ld	a,(OP1)
	xor	d
	and	$80
	jr	nz,@Skip
	xor	a
	ld	(OP1),a		;abs(X_target-X_Cache)
	bcall	_CpyTo2FPST
	bcall	_CpOP1OP2
	jr	z,@ZDSf4
	jr	nc,@Skip
@ZDSf4:
	bcall	_CpyO1ToFPST	;FPS=[min(FPST,abs(X_target,X_Cache)),X_target,...]
	xor	a
	ret
@Skip:
	xor	a
	inc	a
	ret

function(Runge@LoadCache):	;B contains equation bits, HL contains address in cache
	ld	c,5
	rlc	b
	rlc	b			;skip first two bits
@Loop:
	rlc	b
	push	bc
	jr	nc,@DontLoad
	push	hl
	push	bc
	rst	20h ;rMOV9TOOP1
	rst	18h ;rPUSHREALO1
	pop	bc
	pop	hl
@DontLoad:
	ld	bc,9
	add	hl,bc
	pop	bc
	dec	c
	ld	a,$FF
	cp	c
	jr	nz,@Loop
	;HL is start+6*9
	ret

function(Runge@DeallocMiddleFPS):	;keeps the first B floats on the FPS and deletes the following C floats from the FPS
	;WARNING: B and C shouldn't be bigger than 4 because 5*6*9=270>255
	ld	hl,(FPS)
	push	bc
	call	CountEquations
	pop	bc
	ld	e,a
	xor	a
@ZDSb3:
	add	a,e
	djnz	@ZDSb3
	call	MultABy9
	ld	d,a
	ld	b,c
	xor	a
@ZDSb4:
	add	a,e
	djnz	@ZDSb4
	call	MultABy9
	ld	e,a
	push	de			;D=keep bytes E=delete bytes
	ld	b,0
	ld	c,d
	or	a
	sbc	hl,bc			;starting point of floats to keep

	push	hl
	ld	d,0
	or	a
	sbc	hl,de			;starting point of floats to delete

	ex	de,hl
	pop	hl
	pop	bc
	push	bc
	ld	c,b
	ld	b,0
	ldir

	pop	de	
	ld	d,0
	bcall	_DeallocFPS1
	ret	

function(Runge@HLMinusEquNrTimes9):
	call	CountEquations
	or	a
	ret	z
	ld	b,a
	ld	de,-9
@ZDSb5:
	add	hl,de
	djnz	@ZDSb5
	ret

function(Runge@HLPlusCacheOffset):
	ld a,5
	sub e
Runge@HLPlusATimes9:
	or a
	ret z
	ld b,a
	ld de,9
@ZDSb6:
	add hl,de
	djnz @ZDSb6
	ret

function(Runge@LoadY0):
	rlc	d			;skip first two bits
	rlc	d
	ld	e,5
@Loop:
	rlc	d
	jr	nc,@Skip
	push	de
	call	LoadYi0
	rst	18h ;rPUSHREALO1
	pop	de
@Skip:
	dec	e
	ld	a,-1			;0..5 are the equations
	cp	e
	jr	nz,@Loop
	ret

Runge@F1Inc:			;2/9 in floating point
	.db $00,$7F,$22,$22,$22,$22,$22,$22,$22
Runge@F2Inc:			;3/9 in floating point
	.db $00,$7F,$33,$33,$33,$33,$33,$33,$33
Runge@F3Inc:			;4/9 in floating point
	.db $00,$7F,$44,$44,$44,$44,$44,$44,$44

Runge@F1Err:			;5/72 in floating point
	.db $00,$7E,$69,$44,$44,$44,$44,$44,$44
Runge@F2Err:			;-1/12 in floating point
	.db $80,$7E,$83,$33,$33,$33,$33,$33,$33
Runge@F3Err:			;-1/9 in floating point
	.db $80,$7F,$11,$11,$11,$11,$11,$11,$11
Runge@F4Err:			;1/8 in floating point
	.db $00,$7F,$12,$50,$00,$00,$00,$00,$00

;--------------------------------MAYBE LATER--------------------------------
;CLEANUP: don't loop when x0 is requested
;FIX: Improve checks to use cache 
;		* both caches have to be valid now
;		* error handler simple invalidates all caches
;		* has to know whether the cache contains begin and endpoint of a rk step
