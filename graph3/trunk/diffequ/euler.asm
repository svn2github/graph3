;calculate value of equation (register) E at time (variable) X
;parameters for euler
;X0		<-> Tmin
;Xmax		<-> Tmax
;Xstep	<-> Tstep
;y*		<-> X*T
;yi*		<-> Y*T

function(DEQ@Euler):
	push	de
	rst	08h ;rOP1TOOP2
	ld	a,X0
	bcall	_RclSysTok
	bcall	_OP1ExOP2
	bcall	_FPSub
	rst	18h ;rPUSHREALO1
	;calculated X-X0
	pop	de
	ld	d,1
	push	de
	call	@CheckCache
	jr	z,@InitCheckCache	;cache useful
	bcall	_PopRealO1		;get rid of cache-X0
	pop	de
	ld	d,2*9+1
	push	de
	call	@CheckCache
	jr	nz,@InitX0
	jr	@InitCache

@InitCheckCache:
	bcall	_PopRealO1		;get cache-X0
	bcall	_PopRealO2		;get X-X0
	rst	18h ;rPUSHREALO1
	pop	de
	ld	d,2*9+1
	push	de
	call	@CheckCache
	jr	z,@InitCache
	pop	de
	ld	d,1
	push	de
	bcall	_PopRealO1
	bcall	_PopRealO1
	rst	18h ;rPUSHREALO1
	rst	18h ;rPUSHREALO1	;make the top of the stack the correct cache-X0 value
	;use cache as startingpoint
@InitCache:
	;set (variable) X and FPST
	pop	de
	push	de
	call	@LookupCache
	pop	de
	push	de
	ld	e,d
	ld	d,0
	add	hl,de
	rst	20h ;rMOV9TOOP1
	push	hl
	call	DEQ@StoT
	bcall	_PopRealO2
	bcall	_PopRealO1
	pop	hl
	rst	20h ;rMOV9TOOP1
	rst	18h ;rPUSHREALO1
	jr	@InitRegisters
@InitX0:
	;set (variable) X and FPST
	ld	a,X0
	bcall	_RclSysTok
	call	DEQ@StoT
	pop	de
	push	de
	call	DEQ@LoadYi0
	bcall	_PopRealO2
	bcall	_PopRealO2
	rst	18h ;rPUSHREALO1
@InitRegisters:			;doesn't depend on X0/cache choice
	ld	a,Xstep
	bcall	_RclSysTok
	bcall	_OP1ExOP2
	bcall	_FPDiv
	ld	hl,OP1
	ld	a,(hl)
	and	80h
	pop	de
	ld	d,a			;store sign next to equ nr
	push	de
	res	7,(hl)
	bcall	_ConvOP1
	;DE=(X-?)/Xstep -> aantal stappen (?=X0 or ?=cache
	pop	bc
	push	de
	push	bc
	pop	de
	pop	bc
	ld	a,b
	or	c
	jr	z,@LoopSkip
	push	bc
	push	de

@LoopStart:
	;FPS=FPS(+-)Y*(X)*Xstep (forward or reverse euler)
	pop	de
	push	de
	call	DEQ@LoadEquation
	bcall	_ParseInp
	bcall	_CkOP1Real
	jp	nz,DEQ@ParserHook@ArgumentError
	rst	08h ;rOP1TOOP2
	ld	a,Xstep
	bcall	_RclSysTok
	bcall	_FPMult
	rst	08h ;rOP1TOOP2
	bcall	_PopRealO1
	pop	af
	push	af
	or	a
	jr	z,@LoopAdd1
	bcall	_FPSub
	jr	@LoopSub1
@LoopAdd1:
	rst	30h ;rFPADD
@LoopSub1:
	rst	18h ;rPUSHREALO1
	;X=X(+-)Xstep (forward or reverse euler)
	ld	a,Xstep
	bcall	_RclSysTok
	rst	08h ;rOP1TOOP2
	call	DEQ@RclT
	pop	af
	push	af
	or	a
	jr	z,@LoopAdd2
	bcall	_FPSub
	jr	@LoopSub2
@LoopAdd2:
	rst	30h ;rFPADD
@LoopSub2:
	call	DEQ@StoT

	;save calculated value in cache
	pop	de
	push	de
	call	@LookupCache
	ld	a,(hl)
	xor	cacheSwitchMask
	bit	cacheSwitchBit,a
	jr	z,@LoopCache1
	or	cache2ValidMask
	ld	(hl),a
	ld	de,2*9+1
	add	hl,de			;skip first cache
	jr	@LoopCache2
@LoopCache1:
	or	cache1ValidMask
	ld	(hl),a
	inc	hl
@LoopCache2:
	push	hl
	call	DEQ@RclT
	bcall	_CpyTo2FPST
	pop	de
	ld	hl,OP1
	call	Mov9
	ld	hl,OP2
	call	Mov9

	pop	de
	pop	bc
	dec	bc
	ld	a,b
	or	c
	push	bc
	push	de
	jr	nz,@LoopStart
	;done final loop

	pop	de
	pop	bc
@LoopSkip:
	push	de
	;check if final iteration is needed
	call	DEQ@RclT
	rst	08h ;rOP1TOOP2
	bcall	_CpyTo1FPS1
	bcall	_FPSub
	ld	a,(OP1+2)
	or	a
	jr	z,@SkipFinal

	rst	18h ;rPUSHREALO1
	pop	de
	push	de
	call	DEQ@LoadEquation
	bcall	_ParseInp
	bcall	_CkOP1Real
	jp	nz,DEQ@ParserHook@ArgumentError
	bcall	_PopRealO2
	bcall	_FPMult
	bcall	_PopRealO2
	rst	30h ;rFPADD
	rst	18h ;rPUSHREALO1

@SkipFinal:
	bcall	_PopRealO2
	pop	de
	ret

function(DEQ@Euler@CheckCache):	;checks whether using the cache is usefull
	;E contains equ number
	;D contains nr of bytes to skip 1=first cache , 19=2nd cache
	push	de
	call	DEQ@Euler@LookupCache
	pop	de
	ld	a,d
	dec	a
	ld	a,(hl)
	jr	z,@ZDSf
	rra				;shift cache 2 bit to place of cache 1
@ZDSf:
	and	cache1ValidMask
	xor	cache1ValidMask
	push	af			;save for after push on FPS
	ld	e,d
	ld	d,0
	add	hl,de
	rst	20h ;rMOV9TOOP1
	rst	08h ;rOP1TOOP2
	bcall	_CpyTo1FPS1		;X0
	bcall	_FPSub
	rst	18h ;rPUSHREALO1
	pop	af
	ret	nz			;cache was not valid
	;calculated Cache-X0
	ld	a,(OP1+2)
	or	a
	ret	z			;result is in cache
	bcall	_CpyTo2FPS1
	call	DEQ@SameSign
	ret	nz			;only going farther from X0 using cache
	bcall	_FPSub
	bcall	_CpyTo2FPST
	call	DEQ@SameSign
	xor	80h
	;NZ if X0 is closer
	ret

eulerCacheBlockSize equ 9*4+1
;EULER CACHE BLOCK:
;0			statusbits
;9			X1
;9			Y(X1)
;9			X2
;9			Y(X2)

eulerCacheSize equ eulerCacheBlockSize*6 ;Y1..Y6
;EULER CACHE:
;64		SimpleCache (Used by SlopeField)
;37		Cache Block for Y1
;37		Cache Block for Y2
;37		Cache Block for Y3
;37		Cache Block for Y4
;37		Cache Block for Y5
;37		Cache Block for Y6

function(DEQ@Euler@LookupCache):
;E contains equation number
	push	de
	call	DEQ@LookupAppVar
	ex	de,hl
	ld	de,2-(eulerCacheBlockSize)+simpleCacheSize	;leave simple cache available for slopefield use
	add	hl,de
	ld	de,eulerCacheBlockSize
	pop	bc
	ld	b,c
	inc	b
@ZDSb:
	add	hl,de
	djnz	@ZDSb
	ret
