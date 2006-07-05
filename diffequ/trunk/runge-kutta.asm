runge:
	ld d,2+1
	push de
	B_CALL RclX
	rst rPUSHREALO1;save X
	call runge_lookup_simple_cache
	bit cacheRungeSimpleValidBit,(hl)
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
	inc e
	call runge_HL_plus_E_times_9
	rst rMOV9TOOP1
	rst rOP1TOOP2
	jr runge_return_cache
runge_skip_simple_cache:
	B_CALL CpyTo2FPST
	ld a,X0
	B_CALL RclSysTok
	B_CALL OP1ExOP2
	B_CALL FPSub;X-X0
	ld a,(OP1)
	and 80h 
	pop de
	or d
	ld d,a
	push de
	ld a,X0
	B_CALL RclSysTok
	rst rPUSHREALO1
	B_CALL OP1ToOP6
	B_CALL StoX ;starting point
	ld a,Xstep
	B_CALL RclSysTok
	pop af
	push af
	and 80h
	ld (OP1),a
	rst rPUSHREALO1;stepsize

	ld hl,(fps)
	push hl
	pop ix

	pop de
	push de
	call runge_load_y0

runge_loop_start:
	;start RK
	;FPS=[Y*,step,X,X_target,...] 
	pop de
	push de
	call runge_save_y_caches

	pop de
	push de
	call runge_execute_equ ;FPS=[f1*,Y*,step,X,X_target,...]

runge_loop_f1_known:
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
	call runge_load_OP2
	B_CALL FPMult
	jr runge_guess_skip
runge_nodouble:
	rst rOP1TOOP2
	call runge_load_tolerance
	B_CALL FPDiv
	ld hl,25h*256+7Fh;.25
	call runge_load_OP2
	B_CALL YToX
	ld hl,90h*256+7Fh;.9
	call runge_load_OP2
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
	
	call runge_load_tolerance
	B_CALL CpOP1OP2
	push af
	ld hl,20h*256+80h;2
	jr nc,runge_errest_skip
	ld hl,50h*256+7Fh;.5
runge_errest_skip:
	call runge_load_OP2
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
	pop af
	push af
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
	push af
	;FPS=[f4*,Yn+1*,f1*,Y*,step,X,X_target,...]
	ld bc,2*256+2
	call runge_dealloc_middle_FPS
	;FPS=[f4*,Yn+1*,step_next,X+step,X_target,...]
	pop af
	jr z,runge_exit_loop;X+step=X_target
	jr c,runge_loop_f1_known;X+step<X_target
runge_exit_loop:	
	;end RK
	;FIX: find out which value to return (and interpolate)
	;invalidate cache after RK is finished
	pop de
	call runge_lookup_simple_cache
	res cacheRungeSimpleValidBit,(hl)

	B_CALL PopRealO1
	B_CALL PopRealO2;Yn+1
	B_CALL PopRealO1
	B_CALL PopRealO1
runge_return_cache:
	B_CALL PopRealO1
	B_CALL StoX
	B_CALL OP2ToOP1
	ret

runge_retry:
	;FPS=[f4*,Yn+1*,f1*,Y*,step_updated,X,X_target,...]
	pop de
	push de
	call runge_count_equations
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
	call runge_load_OP2
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
	B_CALL StoX;X=X+h*a*0.1
	B_CALL OP3ToOP4;h*a*0.1

	pop de
	push de
	call runge_count_equations
	call runge_mult_A_by_9
	ld hl,(FPS)
	ld b,0
	ld c,a
	or a
	sbc hl,bc
	pop de
	ld e,5
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
	pop hl;Y
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
	call runge_count_equations
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
	B_CALL StoX

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

runge_save_y_caches:
	push de
	call runge_lookup_simple_cache
	ld a,cacheRungeSimpleValidMask
	or (hl)
	ld (hl),a
	inc hl
	
	ex de,hl
	ld hl,OP6
	ld bc,9
	ldir;copy X	

	ld hl,(FPS)
	pop bc
	ld c,0
runge_save_y_caches_loop:
	rrc b
	push bc
	jr nc,runge_save_y_caches_dont_save
	ld bc,9
	or a
	sbc hl,bc
	push hl
	ldir
	pop hl
	jr runge_save_y_caches_skip
runge_save_y_caches_dont_save:
	ld bc,9
	ex de,hl
	add hl,bc
	ex de,hl
runge_save_y_caches_skip:
	pop bc
	inc c
	ld a,6
	cp c
	jr nz,runge_save_y_caches_loop
	ret

runge_dealloc_middle_FPS:;keeps the first B floats on the FPS and deletes the following C floats from the FPS
	;WARNING:B+C shouldn't be bigger than 4 because 5*6*9=270>255
	ld hl,(FPS)
	push bc
	call runge_count_equations
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

runge_load_OP2:;hl=M*256+E
	push hl
	B_CALL ZeroOP2
	pop hl
	ld (OP2+1),hl
	ret

simpleCacheSize equ 1+7*9 ;X and Y1..Y6
;SIMPLE CACHE:
;0			statusbits
;1..9		X
;10..63	Y1..Y6
;FOLLOWED BY ENDPOINT CACHE

endpointCacheBlockSize equ (2+6+6)*9
;ENDPOINT CACHE BLOCK:
;0..8		X
;9..17	step
;18..71	Y1..Y6
;72..125	f1* (for Y1..Y6)

endpointCacheSize equ 1+endpointCacheBlockSize*2 ;X and Y1..Y6
;ENDPOINT CACHE:
;0				statusbits
;1..126		cache block 1
;127..252	cache block 1


runge_lookup_simple_cache:
	call LookupAppVar
	ex de,hl
	inc hl
	inc hl
	ret

runge_lookup_endpoint_cache:
	call LookupAppVar
	ex de,hl
	ld bc,2+simpleCacheSize
	add hl,bc
	ret

runge_HL_minus_equNr_times_9:
	call runge_count_equations
runge_HL_minus_A_times_9:
	or a
	ret z
	ld b,a
	ld de,-9
$$:
	add hl,de
	djnz $b
	ret

runge_HL_plus_E_times_9:
	ld a,e
	or a
	ret z
	ld b,e
	ld de,9
$$:
	add hl,de
	djnz $b
	ret

runge_count_equations:;returns the nr of active equations in d
	xor a
	ld b,6
runge_count_equations_loop:
	rrc d
	adc a,0
	djnz runge_count_equations_loop
	ret

runge_load_y0:
	call runge_count_equations
	ld b,a
$$:
	push bc
	call load_yi0
	rst rPUSHREALO1
	pop bc
	djnz $b
	ret

runge_load_tolerance:
	ld hl,runge_load_tolerance_value
	rst rMOV9TOOP1
	ret
runge_load_tolerance_value:
	db 00h,80h,10h,00h,00h,00h,00h,00h,00h

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

;FIX: invalidate simple cache when raising error
;FIX: don't loop when x0 is requested

