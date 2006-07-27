runge:
	push de
	call LookupAppVar
	ld hl,RKEvalOffset
	add hl,de
	pop de
	ld d,(hl)
	push de
	call RclT
	rst rPUSHREALO1;save X
	pop ix
	AppOnErr runge_errorhandler
	push ix
	call load_simple_cache_address
	bit cacheSimpleValidBit,(hl)
	jr z,runge_skip_simple_cache
	inc hl
	push hl
	B_CALL CpyTo1FPST
	pop hl
	push hl
	B_CALL Mov9OP2Cp
	pop hl
	jr nz,runge_skip_simple_cache
	pop de
	dec e;skip X
	call runge_HL_plus_cache_offset
	rst rMOV9TOOP1
	rst rOP1TOOP2
	jr runge_return_cache
runge_skip_simple_cache:
	call runge_lookup_endpoint_cache
	ld a,cache1ValidMask|cache2ValidMask
	and (hl)
	cp cache1ValidMask|cache2ValidMask
	jr nz,runge_skip_endpoint_cache
	;both are valid
	inc hl
	push hl
	B_CALL CpyTo1FPST
	pop hl
	push hl
	B_CALL Mov9OP2Cp
	pop hl
	jr z,runge_return_interpolate
	ld a,0
	adc a,0;A=1 if X_Cache1>X_target ;A=0 if X_Cache1<X_target
	ld bc,endpointCacheBlockSize
	add hl,bc
	push af
	B_CALL Mov9OP2Cp
	pop bc
	jr z,runge_return_interpolate
	ld a,0
	adc a,0;A=1 if X_Cache2>X_target ;A=0 if X_Cache2<X_target
	xor b;check if they're different
	jr nz,runge_return_interpolate
runge_skip_endpoint_cache:
	;try to save some steps by using cache
	B_CALL CpyTo2FPST
	ld a,X0
	B_CALL RclSysTok
	B_CALL OP1ExOP2
	B_CALL FPSub;X_target-X0
	ld hl,OP1
	ld a,(hl)
	and 80h 
	pop de
	or d
	ld d,a
	push de

	ld e,0
	push de;for use in cache checking with destroyed e value
	ld (hl),0
	rst rPUSHREALO1	;FPS=[abs(X_target-X0),X_target,...]
	call runge_lookup_endpoint_cache
	bit cache1ValidBit,(hl)
	jr z,runge_skip_endpoint_cache1
	pop de
	push de
	push hl
	inc hl
	call runge_check_cache
	pop hl
	jr nz,runge_skip_endpoint_cache1
	pop de
	inc e
	push de
runge_skip_endpoint_cache1:
	bit cache2ValidBit,(hl)
	jr z,runge_skip_endpoint_cache2
	ld bc,1+endpointCacheBlockSize
	add hl,bc
	pop de
	push de
	call runge_check_cache
	jr nz,runge_skip_endpoint_cache2
	pop de
	ld e,2
	push de
runge_skip_endpoint_cache2:
	B_CALL PopRealO1
	pop de
	ld a,e;E=0 use X0, E=1 use Cache1, E=2 use Cache2
	or a
	jr z,runge_load_from_initial_values
	push af
	call runge_lookup_endpoint_cache
	inc hl
	pop af
	cp 2
	jr nz,$f
	ld bc,endpointCacheBlockSize
	add hl,bc
$$:
	;HL=starting point of endpointCacheBlock
	push hl
	ld hl,2*9
	B_CALL AllocFPS1
	pop de

	B_CALL CpyToFPS1;X
	ex de,hl
	B_CALL CpyToFPST;step
	push de
	pop ix;save (FPS)

	ex de,hl
	ld bc,-9
	add hl,bc
	pop af
	push af
	and 80h
	ld (hl),a;correct stepsize direction

	;load Y*
	pop bc
	push bc
	ex de,hl
	call runge_load_cache
	;load f1*
	pop bc
	push bc
	call runge_load_cache

	jr runge_loop_f1_known
runge_load_from_initial_values:
	ld a,X0
	B_CALL RclSysTok
	rst rPUSHREALO1
	B_CALL OP1ToOP6
	call StoT ;starting point
	ld a,Xstep
	B_CALL RclSysTok
	pop af
	push af
	and 80h
	ld (OP1),a
	rst rPUSHREALO1;stepsize

	ld hl,(fps)
	push hl
	pop ix;save (FPS)

	pop de
	push de
	call runge_load_y0

	;start RK
	;FPS=[Y*,step,X,X_target,...] 
	pop de
	push de
	call runge_save_y_caches

	pop de
	push de
	call runge_execute_equ ;FPS=[f1*,Y*,step,X,X_target,...]

	pop de
	push de
	call runge_save_endpoint_cache;save initial values in cache for easy interpolation

runge_loop_f1_known:
	;Check if stepsize is large enough
	;IX=[Y*,step,X,X_target,...]
	push ix
	pop hl
	ld de,-9
	add hl,de
	B_CALL Mov9ToOP2
	push ix
	pop hl
	ld de,-9*2
	add hl,de
	rst rMOV9TOOP1
	B_CALL OP1ToOP3
	rst rFPADD;X+step
	B_CALL OP3ToOP2
	B_CALL FPSub ;(X+step)-X
	ld a,(OP1+1)
	cp 80h-14;E-14
	jr nc,$f
	B_JUMP ErrTolTooSmall
$$:


	pop de
	push de
	ld a,50h
	call runge_update_x_y

	pop de
	push de
	call runge_execute_equ ;FPS=[f2*,f1*,Y*,step,X,X_target,...]

	pop de
	push de
	ld a,75h
	call runge_update_x_y

	pop de
	push de
	call runge_execute_equ ;FPS=[f3*,f2*,f1*,Y*,step,X,X_target,...]

	pop de
	push de
	call runge_update_final_x_y;FPS=[Yn+1*,f3*,f2*,f1*,Y*,step,X,X_target,...]

	pop de
	push de
	call runge_execute_equ ;FPS=[f4*,Yn+1*,f3*,f2*,f1*,Y*,step,X,X_target,...]

	pop de
	push de
	call runge_calc_errest ;FPS=[errest,f4*,Yn+1*,f1*,Y*,step,X,X_target,...]

	ld a,(OP1M)
	or a
	jr nz,runge_nodouble
	;zero error, double stepsize
	push ix
	pop hl
	ld bc,-9
	add hl,bc
	rst rMOV9TOOP1
	ld hl,20h*256+80h;2
	call LoadOP2
	B_CALL FPMult
	jr runge_guess_skip
runge_nodouble:
	rst rOP1TOOP2
	call load_diftol
	B_CALL FPDiv
	ld hl,25h*256+7Fh;.25
	call LoadOP2
	B_CALL YToX
	ld hl,90h*256+7Fh;.9
	call LoadOP2
	B_CALL FPMult
	push ix
	pop hl
	ld bc,-9
	add hl,bc
	B_CALL Mov9ToOP2
	B_CALL FPMult
	;.9*step*(tol/abs(errest))^(1/4)
runge_guess_skip:
	B_CALL PopRealO2
	rst rPUSHREALO1;FPS=[step_guess,f4*,Yn+1*,f1*,Y*,step,X,X_target,...]
	;OP2=errest
	
	call load_diftol
	B_CALL CpOP1OP2
	push af
	ld hl,20h*256+80h;2
	jr nc,runge_errest_skip
	ld hl,50h*256+7Fh;.5
runge_errest_skip:
	call LoadOP2
	push ix
	pop hl
	ld bc,-9
	add hl,bc
	rst rMOV9TOOP1 ;step
	B_CALL FPMult
	B_CALL PopRealO2;step_guess
	xor a
	ld (OP1),a;absolute values
	ld (OP2),a
	B_CALL Min
	;min(.5h,guess) or min(2h,guess)

	push ix
	pop hl
	ld bc,-9
	add hl,bc
	B_CALL Mov9ToOP2 ;step
	pop bc
	pop af
	push af
	push bc
	and 80h
	ld (OP1),a
	push ix
	pop hl
	ld bc,9
	or a
	sbc hl,bc
	ld de,OP1
	ex de,hl
	ldir ;update step
	
	pop af
	jr c,runge_retry;error too big,retry step
	;step succeeded
	;FPS=[f4*,Yn+1*,f1*,Y*,step,X,X_target,...]
	push ix
	pop hl
	ld bc,-9*2
	add hl,bc
	push hl
	rst rMOV9TOOP1 ;X
	rst rFPADD
	pop de
	ld bc,9
	ld hl,OP1
	ldir ;update X
	push ix
	pop hl
	ld bc,-9*3
	add hl,bc
	ld de,OP2
	ld bc,9
	ldir ;X_target
	pop af
	push af
	and 80h
	jr z,$f
	B_CALL OP1ExOP2 ;correct for negative stepsize
$$:
	B_CALL CpOP1OP2
	pop de
	push de
	push af
	;FPS=[f4*,Yn+1*,f1*,Y*,step,X,X_target,...]
	ld bc,2*256+2
	call runge_dealloc_middle_FPS
	;FPS=[f4*,Yn+1*,step_next,X+step,X_target,...]
	pop af
	pop de
	push de
	push af
	call runge_save_endpoint_cache
	pop af
	jr z,runge_exit_loop;X+step=X_target
	jr c,runge_loop_f1_known;X+step<X_target
runge_exit_loop:	
	;end RK
	;invalidate cache after RK is finished
	call load_simple_cache_address
	res cacheSimpleValidBit,(hl)

	pop de
	push de
	call CountEquations
	add a
	add 2;*2+2
	call runge_mult_A_by_9
	ld e,a
	ld d,0
	B_CALL DeallocFPS1
	;FPS=[X_target,...]

runge_return_interpolate:
	pop de
	call runge_interpolate

runge_return_cache:
	AppOffErr
	B_CALL PopRealO1
	call StoT
	B_CALL OP2ToOP1
	ret

runge_errorhandler:
	push af
	call load_simple_cache_address
	res cacheSimpleValidBit,(hl)
	ld bc,simpleCacheSize
	add hl,bc
	res cache1ValidBit,(hl)
	res cache2ValidBit,(hl)
	B_CALL PopRealO1
	call StoT
	pop af
	B_JUMP JError

runge_retry:
	;FPS=[f4*,Yn+1*,f1*,Y*,step_updated,X,X_target,...]
	pop de
	push de
	call CountEquations
	add a;remove both f4* and Yn+1*
	call runge_mult_A_by_9
	ld e,a
	ld d,0
	B_CALL DeallocFPS1
	;FPS=[f1*,Y*,step_updated,X,X_target,...]
	jr runge_loop_f1_known

runge_mult_add_de:
	push de
	B_CALL Mov9ToOP2
	pop hl
	jr runge_mult_add
runge_mult_add_pop:
	push hl
	B_CALL PopRealO2
s	pop hl
runge_mult_add:
	rst rMOV9TOOP1
	B_CALL FPMult
	B_CALL OP4ToOP2
	rst rFPADD
	ret	

runge_execute_equ:
	rlc d;skip first two bits
	rlc d
	ld e,5
runge_execute_equs_loop:
	rlc d
	jr nc,runge_execute_equs_skip_exec
	push de
	call load_equation
	push ix
	B_CALL ParseInp
	pop ix
	B_CALL CkOP1Real
	jr nz,parser_hook_argument_error
	rst rPUSHREALO1;fx
	pop de
runge_execute_equs_skip_exec:
	dec e
	ld a,-1;0..5 are the equations
	cp e
	jr nz,runge_execute_equs_loop
	ret

runge_calc_errest:
	;FPS=[f4*,Yn+1*,f3*,f2*,f1*,Y*,step,X,X_target,...]
	;IX=[Y*,step,X]
	push de
	ld bc,8
	ld de,OP6+1
	push de
	ld hl,OP6
	ld (hl),0
	ldir
	pop hl
	ld (hl),80h;OP6=0.0

	ld hl,(FPS)
	pop de
	ld e,0
runge_calc_errest_loop:
	push de
	ld b,e
	inc b
$$:
	rrc d
	djnz $b
	pop de
	jr nc,runge_calc_errest_skip

	ld bc,-9
	add hl,bc
	push hl
	push de
	push hl;stack=[HL,DE,HL,...]
	B_CALL Mov9ToOP2 ;f4
	ld hl,runge_f4_err
	rst rMOV9TOOP1
	B_CALL FPMult
	B_CALL OP1ToOP4	

	pop hl
	pop de
	push de
	call runge_HL_minus_equNr_times_9
	pop de
	push de
	call runge_HL_minus_equNr_times_9
	push hl
	ld de,runge_f3_err
	call runge_mult_add_de
	B_CALL OP1ToOP4

	pop hl
	pop de
	push de
	call runge_HL_minus_equNr_times_9
	push hl
	ld de,runge_f2_err
	call runge_mult_add_de
	B_CALL OP1ToOP4

	pop hl
	pop de
	push de
	call runge_HL_minus_equNr_times_9
	ld de,runge_f1_err
	call runge_mult_add_de
	
	push ix
	pop hl
	ld de,-9
	add hl,de
	B_CALL Mov9ToOP2 ;step
	B_CALL FPMult
	xor a
	ld (OP1),a;abs
	;OP1=abs(h(5/72*f1-1/12*f2-1/9*f3+1/8*f4))  the error estimate
	;FPS=[f4*,Yn+1*,f3*,f2*,f1*,Y*,step,X,X_target,...]

	B_CALL OP6ToOP2
	B_CALL Max
	B_CALL OP1ToOP6
	pop de
	pop hl
runge_calc_errest_skip:
	inc e
	ld a,6
	cp e
	jr nz,runge_calc_errest_loop

	ld bc,2*256+2
	call runge_dealloc_middle_FPS ;delete f2* and f3* keep Yn+1* and f4*
	
	rst rPUSHREALO1
	;FPS=[errest,f4*,Yn+1*,f1*,Y*,step,X,X_target,...]
	ret

runge_update_x_y:
	push de
	push af
	
	;calculate h*a*0.1 here
	push ix
	pop hl
	ld de,-9
	add hl,de
	rst rMOV9TOOP1
	pop hl
	ld l,7Fh
	call LoadOP2
	;OP2=a*0.1
	B_CALL FPMult
	B_CALL OP1ToOP3;h*a*0.1

	rst rOP1TOOP2
	push ix
	pop hl
	ld de,-9*2
	add hl,de
	rst rMOV9TOOP1
	rst rFPADD
	B_CALL OP1ToOP6
	call StoT;X=X+h*a*0.1
	B_CALL OP3ToOP4;h*a*0.1

	pop de
	push de
	call CountEquations
	call runge_mult_A_by_9
	ld hl,(FPS)
	ld b,0
	ld c,a
	or a
	sbc hl,bc
	pop de
	ld e,5
	res 6,d
	res 7,d
	rlc d
	rlc d;skip first two bits
runge_update_x_y_loop:
	;D contains eval bits
	;E contains equ nr
	;HL contains address of fx
	rlc d
	jr nc,runge_update_x_y_skip

	push hl
	push de

	;IX=[Y*,step,X]
	rst rMOV9TOOP1;fx
	B_CALL OP4ToOP2;h*a*0.1
	B_CALL FPMult
   rst rOP1TOOP2

	push ix
	pop hl
	pop de
	push de

	ld a,6
	sub e
	ld b,a;5-e
	xor a
runge_update_x_y_inner_loop:
	rrc d
	adc a,0
	djnz runge_update_x_y_inner_loop
	dec a
	call runge_HL_plus_A_times_9
	
	rst rMOV9TOOP1
	rst rFPADD ;Y+h*a*0.1*fx
	rst rPUSHREALO1

	pop de
	pop hl
	ld bc,9
	add hl,bc
runge_update_x_y_skip:
	dec e
	ld a,0FFh;0..5 are the equations
	cp e
	jr nz,runge_update_x_y_loop
	push de
	call runge_save_y_caches
	pop de
	call CountEquations
	call runge_mult_A_by_9
	ld e,a
	ld d,0
	B_CALL DeallocFPS1
	ret

runge_update_final_x_y:
	;FPS=[f3*,f2*,f1*,Y*,step,X,X_target,...]
	;IX=[Y*,step,X]
	push de

	push ix
	pop hl
	ld de,-9
	add hl,de
	rst rMOV9TOOP1 ;step
	push ix
	pop hl
	ld de,-9*2
	add hl,de
	B_CALL Mov9ToOP2 ;X
	rst rFPADD ;X+h
	B_CALL OP1ToOP6
	call StoT

	pop de
	push de
	ld hl,(FPS)
	call runge_HL_minus_equNr_times_9
	pop de
	ld e,5

runge_update_final_x_y_loop:
	push hl
	push de
	ld b,e
	inc b
$$:
	rrc d
	djnz $b
	pop de
	pop hl
	jr nc,runge_update_final_x_y_skip

	push hl
	push de
	push hl ;stack=[HL,DE,HL,...]
	
	B_CALL Mov9ToOP2
	ld hl,runge_f3_inc
	rst rMOV9TOOP1	
	B_CALL FPMult
	B_CALL OP1ToOP4

	pop hl
	pop de
	push de
	call runge_HL_minus_equNr_times_9
	push hl
	ld de,runge_f2_inc
	call runge_mult_add_de
	B_CALL OP1ToOP4

	pop hl
	pop de
	push de
	call runge_HL_minus_equNr_times_9
	push hl
	ld de,runge_f1_inc
	call runge_mult_add_de

	push ix
	pop hl
	ld de,-9
	add hl,de
	B_CALL Mov9ToOP2 ;step
	B_CALL FPMult

	pop hl
	pop de
	push de
	call runge_HL_minus_equNr_times_9
	B_CALL Mov9ToOP2 ;Y
	rst rFPADD ;Y+h(2/9*f1+1/3*f2+4/9*f3)
	rst rPUSHREALO1 ;FPS=[Yn+1*,f3*,f2*,f1*,Y*,step,X,X_target,...]
	pop de
	pop hl
	ld bc,9
	add hl,bc
runge_update_final_x_y_skip:
	dec e
	ld a,-1
	cp e
	jr nz,runge_update_final_x_y_loop
	call runge_save_y_caches
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
runge_interpolate:
	;FPS=[X_target,...]
	push de
	call runge_lookup_endpoint_cache
	inc hl ;assume both caches are valid
	pop de
	push de
	push hl
	call runge_interpolate_load_cache
	;FPS=[Xa,Ya,Fa,X_target,...]
	pop hl
	pop de
	ld bc,endpointCacheBlockSize
	add hl,bc
	call runge_interpolate_load_cache
	;FPS=[Xb,Yb,Fb,Xa,Ya,Fa,X_target,...]

	B_CALL CpyTo1FPST;Xb
	B_CALL CpyTo2FPS3;Xa
	B_CALL FPSub
	ld hl,(FPS)
	push hl
	rst rPUSHREALO1;divider=Xb-Xa
	;FPS=[divider,Xb,Yb,Fb,Xa,Ya,Fa,X_target,...]

	B_CALL CpyTo1FPS2;Yb
	B_CALL CpyTo2FPS5;Ya
	pop hl
	push hl
	call runge_interpolate_calc
	rst rPUSHREALO1 ;row3=(yb-ya)/divider
	;FPS=[row3,divider,Xb,Yb,Fb,Xa,Ya,Fa,X_target,...]

	rst rOP1TOOP2;row3
	B_CALL CpyTo1FPS4;Fb
	pop hl
	push hl
	call runge_interpolate_calc
	rst rPUSHREALO1 ;row4=(fb-row3)/divider
	;FPS=[row4,row3,divider,Xb,Yb,Fb,Xa,Ya,Fa,X_target,...]

	B_CALL CpyTo1FPS1;row3
	B_CALL CpyTo2FPS8;Fa
	pop hl
	push hl
	call runge_interpolate_calc 
	B_CALL CpyO1ToFPS1;row3=(row3-fa)/divider

	rst rOP1TOOP2 ;row3
	B_CALL CpyTo1FPST;row4
	pop hl
	call runge_interpolate_calc
	B_CALL CpyO1ToFPST;row4=(row4-row3)/divider

	B_CALL CpyTo1FPS9;X_target
	B_CALL CpyTo2FPS3;Xb
	B_CALL FPSub
	B_CALL PopRealO2;row4
	B_CALL FPMult
	B_CALL PopRealO2;row3
	rst rFPADD
	rst rPUSHREALO1;row3+(X_target-Xb)*row4
	;FPS=[row3+(X_target-Xb)*row4,	divider,Xb,Yb,Fb,Xa,Ya,Fa,X_target,...]

	B_CALL CpyTo1FPS8;X_target
	B_CALL CpyTo2FPS5;Xa
	B_CALL FPSub
	B_CALL PopRealO2;row3+(X_target-Xb)*row4
	rst rPUSHREALO1;X_target-Xa
	B_CALL FPMult
	B_CALL CpyTo2FPS7;Fa
	rst rFPADD
	;OP1=Fa+(X-Xa)*(row3+(X-Xb)*row4)
	;FPS=[X_target-Xa,		divider,Xb,Yb,Fb,Xa,Ya,Fa,X_target,...]
	
	B_CALL PopRealO2;X_target-Xa
	B_CALL FPMult
	B_CALL CpyTo2FPS5;Ya
	rst rFPADD
	rst rOP1TOOP2
	;OP2=Ya+(X_target-Xa)*(Fa+(X_target-Xa)*(row3+(X_target-Xb)*row4))

	ld de,7*9
	B_CALL DeallocFPS1
	;FPS=[X_target,...]
	ret	
	
runge_interpolate_calc:
	push hl
	B_CALL FPSub
	pop hl
	B_CALL Mov9ToOP2
	B_CALL FPDiv
	ret

runge_interpolate_load_cache:
	push de
	push hl
	ld hl,3*9
	B_CALL AllocFPS1
	pop de
	B_CALL CpyToFPST
	pop de
	dec e;skip step
	call runge_HL_plus_cache_offset
	ex de,hl
	B_CALL CpyToFPS1
	ld bc,5*9;skip 5 floats
	add hl,bc
	ex de,hl
	B_CALL CpyToFPS2
	ret

endpointCacheBlockSize equ (2+6+6)*9
;ENDPOINT CACHE BLOCK:
;0..8		X
;9..17	step
;18..71	Y6..Y1 (reverse order!)
;72..125	f1* (for Y6..Y1) (reverse order!)

endpointCacheSize equ 1+endpointCacheBlockSize*2 ;X and Y1..Y6
;ENDPOINT CACHE:
;0				statusbits
;1..126		cache block 1
;127..252	cache block 1


runge_lookup_endpoint_cache:
	call LookupAppVar
	ex de,hl
	ld bc,2+simpleCacheSize
	add hl,bc
	ret

runge_save_endpoint_cache:
	;FPS=[f4*,Yn+1*,step,X,X_target,...]
	push de
	call runge_lookup_endpoint_cache

	ld a,(hl)
	xor cacheSwitchMask
	bit cacheSwitchBit,a
	jr z,runge_save_endpoint_cache1
	or cache2ValidMask
	ld (hl),a
	ld de,endpointCacheBlockSize+1
	add hl,de;skip first cache
	jr runge_save_endpoint_cache2
runge_save_endpoint_cache1:
	or cache1ValidMask
	ld (hl),a
	inc hl
runge_save_endpoint_cache2:

	ex de,hl
	push ix
	pop hl
	ld bc,-9*2
	add hl,bc
	ld bc,9
	ldir;copy X
	ld bc,9
	ldir;copy step

	ex de,hl
	pop de
	push hl
	push de
	ld hl,(FPS)
	call runge_HL_minus_equNr_times_9
	pop de
	push de
	call runge_HL_minus_equNr_times_9
	pop bc
	pop de
	push bc
	call runge_save_in_appvar;Save Y*

	ex de,hl
	pop de
	push hl
	push de
	ld hl,(FPS)
	call runge_HL_minus_equNr_times_9
	pop bc
	pop de
	call runge_save_in_appvar;Save Y*
	ret

runge_save_y_caches:
	push de
	call load_simple_cache_address
	ld a,cacheSimpleValidMask
	or (hl)
	ld (hl),a
	inc hl
	
	ex de,hl
	ld hl,OP6
	ld bc,9
	ldir;copy X	

	ex de,hl
	pop de
	push hl
	push de
	ld hl,(FPS)
	call runge_HL_minus_equNr_times_9
	pop bc
	pop de
	;fallthrough

runge_save_in_appvar:
	;DE contains address in appvar
	;HL contains address of first value on fps
	;B  contains equation bits
	ld c,5
	rlc b
	rlc b;skip first two bits
runge_save_in_appvar_loop:
	rlc b
	push bc
	jr nc,runge_save_in_appvar_dont_save
	ld bc,9
	ldir
	jr runge_save_in_appvar_skip
runge_save_in_appvar_dont_save:
	ld bc,9
	ex de,hl
	add hl,bc
	ex de,hl
runge_save_in_appvar_skip:
	pop bc
	dec c
	ld a,0FFh
	cp c
	jr nz,runge_save_in_appvar_loop
	;DE is start+6*9
	;HL is address of last value copied
	ret

runge_check_cache:;check cache at HL, Z if cache is useful 
	push de
	B_CALL Mov9ToOP2;X_Cache
	B_CALL CpyTo1FPS1;X_target
	B_CALL FPSub
	pop de
	ld a,(OP1)
	xor d
	and 80h
	jr nz,runge_check_cache_skip
	xor a
	ld (OP1),a;abs(X_target-X_Cache)
	B_CALL CpyTo2FPST
	B_CALL CpOP1OP2
	jr z,$f
	jr nc,runge_check_cache_skip
$$:
	B_CALL CpyO1ToFPST ;FPS=[min(FPST,abs(X_target,X_Cache)),X_target,...]
	xor a
	ret
runge_check_cache_skip:
	xor a
	inc a
	ret

runge_load_cache:;B contains equation bits, HL contains address in cache
	ld c,5
	rlc b
	rlc b;skip first two bits
runge_load_cache_loop:
	rlc b
	push bc
	jr nc,runge_load_cache_dont_load
	push hl
	push bc
	rst rMOV9TOOP1
	rst rPUSHREALO1
	pop bc
	pop hl
runge_load_cache_dont_load:
	ld bc,9
	add hl,bc
	pop bc
	dec c
	ld a,0FFh
	cp c
	jr nz,runge_load_cache_loop
	;HL is start+6*9
	ret

runge_dealloc_middle_FPS:;keeps the first B floats on the FPS and deletes the following C floats from the FPS
	;WARNING:B and C shouldn't be bigger than 4 because 5*6*9=270>255
	ld hl,(FPS)
	push bc
	call CountEquations
	pop bc
	ld e,a
	xor a
$$:
	add e
	djnz $b
	call runge_mult_A_by_9
	ld d,a
	ld b,c
	xor a
$$:
	add e
	djnz $b
	call runge_mult_A_by_9
	ld e,a
	push de;D=keep bytes E=delete bytes
	ld b,0
	ld c,d
	or a
	sbc hl,bc;starting point of floats to keep

	push hl
	ld d,0
	or a
	sbc hl,de;starting point of floats to delete

	ex de,hl
	pop hl
	pop bc
	push bc
	ld c,b
	ld b,0
	ldir

	pop de	
	ld d,0
	B_CALL DeallocFPS1
	ret	

runge_mult_A_by_9:;destroys B
	ld b,a
	add a
	add a
	add a
	add b
	ret

runge_HL_minus_equNr_times_9:
	call CountEquations
runge_HL_minus_A_times_9:
	or a
	ret z
	ld b,a
	ld de,-9
$$:
	add hl,de
	djnz $b
	ret

runge_HL_plus_cache_offset:
	ld a,5
	sub e
runge_HL_plus_A_times_9:
	or a
	ret z
	ld b,a
	ld de,9
$$:
	add hl,de
	djnz $b
	ret

runge_load_y0:
	rlc d;skip first two bits
	rlc d
	ld e,5
runge_load_y0_loop:
	rlc d
	jr nc,runge_load_y0_skip
	push de
	call load_yi0
	rst rPUSHREALO1
	pop de
runge_load_y0_skip:
	dec e
	ld a,-1;0..5 are the equations
	cp e
	jr nz,runge_load_y0_loop
	ret

runge_f1_inc: ;2/9 in floating point
	db 00h,7Fh,22h,22h,22h,22h,22h,22h,22h
runge_f2_inc: ;3/9 in floating point
	db 00h,7Fh,33h,33h,33h,33h,33h,33h,33h
runge_f3_inc: ;4/9 in floating point
	db 00h,7Fh,44h,44h,44h,44h,44h,44h,44h

runge_f1_err: ;5/72 in floating point
	db 00h,7Eh,69h,44h,44h,44h,44h,44h,44h
runge_f2_err: ;-1/12 in floating point
	db 80h,7Eh,83h,33h,33h,33h,33h,33h,33h
runge_f3_err: ;-1/9 in floating point
	db 80h,7Fh,11h,11h,11h,11h,11h,11h,11h
runge_f4_err: ;1/8 in floating point
	db 00h,7Fh,12h,50h,00h,00h,00h,00h,00h

;--------------------------------MAYBE LATER--------------------------------
;CLEANUP: don't loop when x0 is requested
;CLEANUP: don't multiply by 9 when deallocating FPS use DeallocFPS instead
;CLEANUP: use OP*Set* to optimize the code
;FIX: Improve checks to use cache 
;		* both caches have to be valid now
;		* error handler simple invalidates all caches
;		* has to know whether the cache contains begin and endpoint of a rk step
