;calculate value of equation (register) E at time (variable) X
;parameters for euler
;X0		<-> Tmin
;Xmax		<-> Tmax
;Xstep	<-> Tstep
;y*		<-> X*T
;yi*		<-> Y*T

euler:
	push de
	rst rOP1TOOP2
	ld a,X0
	B_CALL RclSysTok
	B_CALL OP1ExOP2
	B_CALL FPSub
	rst rPUSHREALO1
	;calculated X-X0
	pop de
	ld d,1
	push de
	call euler_check_cache
	jr z,euler_init_check_cache;cache usefull
	B_CALL PopRealO1 ;get rid of cache-X0
	pop de
	ld d,2*9+1
	push de
	call euler_check_cache
	jr nz,euler_init_X0
	jr euler_init_cache

euler_init_check_cache:
	B_CALL PopRealO1 ;get cache-X0
	B_CALL PopRealO2 ;get X-X0
	rst rPUSHREALO1
	pop de
	ld d,2*9+1
	push de
	call euler_check_cache
	jr z,euler_init_cache
	pop de
	ld d,1
	push de
	B_CALL PopRealO1
	B_CALL PopRealO1
	rst rPUSHREALO1
	rst rPUSHREALO1;make the top of the stack the correct cache-X0 value
	;use cache as startingpoint
euler_init_cache:
	;set (variable) X and FPST
	pop de
	push de
	call euler_lookup_cache
	pop de
	push de
	ld e,d
	ld d,0
	add hl,de
	rst rMOV9TOOP1
	push hl
	call StoT
	B_CALL PopRealO2
	B_CALL PopRealO1
	pop hl
	rst rMOV9TOOP1
	rst rPUSHREALO1
	jr euler_init_registers
euler_init_X0:
	;set (variable) X and FPST
	ld a,X0
	B_CALL RclSysTok
	call StoT
	pop de
	push de
	call load_yi0
	B_CALL PopRealO2
	B_CALL PopRealO2
	rst rPUSHREALO1
euler_init_registers:;doesn't depend on X0/cache choice
	ld a,Xstep
	B_CALL RclSysTok
	B_CALL OP1ExOP2
	B_CALL FPDiv
	ld hl,OP1
	ld a,(hl)
	and 80h
	pop de
	ld d,a;store sign next to equ nr
	push de
	res 7,(hl)
	B_CALL ConvOP1
	;DE=(X-?)/Xstep -> aantal stappen (?=X0 or ?=cache
	pop bc
	push de
	push bc
	pop de
	pop bc
	ld a,b
	or c
	jr z,euler_loop_skip
	push bc
	push de


euler_loop_start:
	;FPS=FPS(+-)Y*(X)*Xstep (forward or reverse euler)
	pop de
	push de
	call load_equation
	B_CALL ParseInp
	B_CALL CkOP1Real
	jr nz,parser_hook_argument_error
	rst rOP1TOOP2
	ld a,Xstep
	B_CALL RclSysTok
	B_CALL FPMult
	rst rOP1TOOP2
	B_CALL PopRealO1
	pop af
	push af
	or a
	jr z,euler_loop_add1
	B_CALL FPSub
	jr euler_loop_sub1
euler_loop_add1:
	rst rFPADD
euler_loop_sub1:
	rst rPUSHREALO1
	;X=X(+-)Xstep (forward or reverse euler)
	ld a,Xstep
	B_CALL RclSysTok
	rst rOP1TOOP2
	call RclT
	pop af
	push af
	or a
	jr z,euler_loop_add2
	B_CALL FPSub
	jr euler_loop_sub2
euler_loop_add2:
	rst rFPADD
euler_loop_sub2:
	call StoT

	;save calculated value in cache
	pop de
	push de
	call euler_lookup_cache
	ld a,(hl)
	xor cacheSwitchMask
	bit cacheSwitchBit,a
	jr z,euler_loop_cache1
	or cache2ValidMask
	ld (hl),a
	ld de,2*9+1
	add hl,de;skip first cache
	jr euler_loop_cache2
euler_loop_cache1:
	or cache1ValidMask
	ld (hl),a
	inc hl
euler_loop_cache2:
	push hl
	call RclT
	B_CALL CpyTo2FPST
	pop de
	ld hl,OP1
	call Mov9
	ld hl,OP2
	call Mov9

	pop de
	pop bc
	dec bc
	ld a,b
	or c
	push bc
	push de
	jr nz,euler_loop_start
	;done final loop
	
	pop de
	pop bc
euler_loop_skip:
	push de
	;check if final iteration is needed
	call RclT
	rst rOP1TOOP2
	B_CALL CpyTo1FPS1
	B_CALL FPSub
	ld a,(OP1+2)
	or a
	jr z,euler_skip_final

	rst rPUSHREALO1
	pop de
	push de
	call load_equation
	B_CALL ParseInp
	B_CALL CkOP1Real
	jr nz,parser_hook_argument_error
	B_CALL PopRealO2
	B_CALL FPMult
	B_CALL PopRealO2
	rst rFPADD
	rst rPUSHREALO1

euler_skip_final:
	B_CALL PopRealO2
	pop de
	ret

euler_check_cache:;checks whether using the cache is usefull
	;E contains equ number
	;D contains nr of bytes to skip 1=first cache , 19=2nd cache
	push de
	call euler_lookup_cache
	pop de
	ld a,d
	dec a
	ld a,(hl)
	jr z,$f
	rra ;shift cache 2 bit to place of cache 1
$$:
	and cache1ValidMask
	xor cache1ValidMask
	push af;save for after push on FPS
	ld e,d
	ld d,0
	add hl,de
	rst rMOV9TOOP1
	rst rOP1TOOP2
	B_CALL CpyTo1FPS1;X0
	B_CALL FPSub
	rst rPUSHREALO1
	pop af
	ret nz;cache was not valid
	;calculated Cache-X0
	ld a,(OP1+2)
	or a
	ret z;result is in cache
	B_CALL CpyTo2FPS1
	call same_sign
	ret nz;only going farther from X0 using cache
	B_CALL FPSub
	B_CALL CpyTo2FPST
	call same_sign
	xor 80h
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

euler_lookup_cache:
;E contains equation number
	push de
	call LookupAppVar
	ex de,hl
	ld de,2-(eulerCacheBlockSize)+simpleCacheSize;leave simple cache available for slopefield use
	add hl,de
	ld de,eulerCacheBlockSize
	pop bc
	ld b,c
	inc b
$$:
	add hl,de
	djnz $b
	ret
