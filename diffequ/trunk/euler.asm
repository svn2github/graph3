;calculate value of equation (register) E at time (variable) X
;parameters for euler
;X0		<-> Tmin
;Xmax		<-> Tmax
;Xstep	<-> Tstep
;Estep	<-> -			(new variable, equals Xres now) 
;y*		<-> y*		(y7..y0 now for 1..4)
;yi*		<-> -			(new variable, equals 1 now)

X0 equ TMINt
Xstep equ TSTEPt

euler:
	push de
	B_CALL RclX
	rst rPUSHREALO1;save X
	rst rOP1TOOP2
	ld a,X0
	B_CALL RclSysTok
	B_CALL OP1ExOP2
	B_CALL FPSub
	rst rPUSHREALO1
	;calculated X-X0
	;FIX: cache not always on a Xstep, can also be on a Estep
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
	B_CALL StoX
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
	B_CALL StoX
	pop de
	push de
	call euler_load_yi0
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
	call euler_load_estep
	B_CALL ConvOP1
	pop bc
	push de
	push bc
euler_loop2_start:;Estep loop
	;FPS=FPS(+-)Y*(X)*Xstep/Estep (forward or reverse euler)
	pop de
	push de
	call euler_load_equation
	B_CALL ParseInp
	B_CALL CkOP1Real
	jr nz,parser_hook_argument_error
	rst rOP1TOOP2
	ld a,Xstep
	B_CALL RclSysTok
	B_CALL FPMult
	rst rOP1TOOP2
	call euler_load_estep
	B_CALL OP1ExOP2
	B_CALL FPDiv
	rst rOP1TOOP2
	B_CALL PopRealO1
	pop af
	push af
	or a
	jr z,euler_loop2_add1
	B_CALL FPSub
	jr euler_loop2_sub1
euler_loop2_add1:
	rst rFPADD
euler_loop2_sub1:
	rst rPUSHREALO1
	;X=X(+-)Xstep/Estep (forward or reverse euler)
	call euler_load_estep
	rst rOP1TOOP2
	ld a,Xstep
	B_CALL RclSysTok
	B_CALL FPDiv
	rst rOP1TOOP2
	B_CALL RclX
	pop af
	push af
	or a
	jr z,euler_loop2_add2
	B_CALL FPSub
	jr euler_loop2_sub2
euler_loop2_add2:
	rst rFPADD
euler_loop2_sub2:
	B_CALL StoX

	;save calculated value in cache
	pop de
	push de
	call euler_lookup_cache
	ld a,(hl)
	xor cacheSwitchMask
	bit cacheSwitchBit,a
	jr z,euler_loop2_cache1
	or cache2ValidMask
	ld (hl),a
	ld de,2*9+1
	add hl,de;skip first cache
	jr euler_loop2_cache2
euler_loop2_cache1:
	or cache1ValidMask
	ld (hl),a
	inc hl
euler_loop2_cache2:
	push hl
	B_CALL RclX
	B_CALL CpyTo2FPST
	pop de
	ld hl,OP1
	ld bc,9
	ldir
	ld bc,9
	ld hl,OP2
	ldir

	pop de
	pop bc
	dec bc
	ld a,c
	or b
	push bc
	push de
	jr nz,euler_loop2_start

	pop de
	pop bc
euler_loop2_end:
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
	B_CALL RclX
	rst rOP1TOOP2
	B_CALL CpyTo1FPS1
	B_CALL FPSub
	ld a,(OP1+2)
	or a
	jr z,euler_skip_final

	rst rPUSHREALO1
	pop de
	call euler_load_equation
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
	B_CALL PopRealO1
	B_CALL StoX
	B_CALL OP2ToOP1
	pop de
	or 0FFh
	ret

euler_load_equation:
	ld a,e
	ld hl,euler_equ
	rst rMOV9TOOP1
	ld hl,OP1+2
	add (hl)
	ld (hl),a
	ret
euler_equ:
	db EquObj, tVarEqu, tY7,0

euler_lookup_cache:
;E contains equation number
	push de
	call LookupAppVar
	ex de,hl
	ld de,2-(18*2+1)
	add hl,de
	ld de,9*4+1
	pop bc
	ld b,c
	inc b
$$:
	add hl,de
	djnz $b
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

euler_load_estep:;FIX: use real estep value
	ld a,XRESt
	B_CALL RclSysTok
	ret

euler_load_yi0:
	ld hl,euler_load_yi0_value
	rst rMOV9TOOP1
	ret
euler_load_yi0_value:
	db 0,80h,10h,0,0,0,0,0,0
