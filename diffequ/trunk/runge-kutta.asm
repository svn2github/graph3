runge:
	ld d,2
	push de
	B_CALL RclX
	rst rPUSHREALO1;save X
	pop de
	push de
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
	B_CALL OP1ToOP5
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
	B_CALL OP1ToOP6

runge_loop_start:
	;start RK
	;FPS=[Y*,step,X,X_target,...] 
	pop de
	push de
	call runge_save_y_cache

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
	call runge_calc_errest ;FPS=[errest,f4*,Yn+1*,Y*,step,X,X_target,...]

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
	rst rPUSHREALO1;FPS=[step_guess,f4*,Yn+1*,Y*,step,X,X_target,...]
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
	pop af
	jr c,runge_retry;error too big,retry step
	;step succeeded
	;FPS=[f4*,Yn+1*,Y*,step,X,X_target,...]
	push ix
	pop hl
	ld bc,-9
	B_CALL Mov9ToOP2
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
	ldir ;update step
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
	;FIX: support multiple equations
	B_CALL PopRealO1;f4*
	B_CALL PopRealO2;Yn+1*
	B_CALL PopRealO3;Y*
	B_CALL PushRealO2
	rst rPUSHREALO1
	;FPS=[f4*,Yn+1*,step_next,X+step,X_target,...]
	pop af
	jr z,runge_exit_loop;X+step=X_target
	jr c,runge_loop_f1_known;X+step<X_target
runge_exit_loop:	
	;end RK
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
	;FPS=[f4*,Yn+1*,Y*,step,X,X_target,...]
	B_CALL PopRealO2
	B_CALL PopRealO2
	pop af
	push af
	and 80h
	ld (OP1),a
	B_CALL CpyO1ToFPS1
	;FPS=[Y*,step_updated,X,X_target,...]
	;update X
	B_CALL CpyTo1FPS2
	B_CALL OP1ToOP5
	B_CALL StoX
	;update Y
	B_CALL CpyTo6FPST
	jr runge_loop_start

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
	
	ld hl,(FPS)
	call runge_count_equations
	push af
	add a;double it
	call runge_HL_minus_A_times_9 ;f4 and Yn+1

	pop af
	push hl
	push af
	ld b,a
	add a
	add b;a=3*a
	call runge_HL_minus_A_times_9; f3*,f2*,f1*

	ex de,hl
	pop af
	pop hl
	push af
	add a
	ld b,a
	add a
	add a
	add a;*16
	add b;*18=2*9
	ld c,a
	ld b,0
	push bc
	ldir

	pop bc
	pop af
	ld b,a
	add a
	add a
	add a
	add b
	add c;*27=3*9
	ld e,a
	ld d,0
	B_CALL DeallocFPS1

	rst rPUSHREALO1
	;FPS=[errest,f4*,Yn+1*,Y*,step,X,X_target,...]
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
	B_CALL OP1ToOP5
	B_CALL StoX;X=X+h*a*0.1
	B_CALL OP3ToOP4;h*a*0.1

	pop de
	res 7,d
	ld e,0
	ld hl,(FPS)
runge_update_x_y_loop:
	;D contains eval bits
	;E contains equ nr
	;HL contains address of fx
	rrc d
	jr nc,runge_update_x_y_skip

	ld bc,-9
	add hl,bc

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
	B_CALL OP1ToOP6

	pop de
	push de

	call runge_save_y_cache

	pop de
	pop hl
runge_update_x_y_skip:
	inc e
	ld a,6;0..5 are the equations
	cp e
	jr nz,runge_update_x_y_loop
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
	B_CALL OP1ToOP5
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
	B_CALL OP1ToOP6
	pop de
	push de
	call runge_save_y_cache
	pop de
	pop hl
	ld bc,9
	add hl,bc
runge_update_final_x_y_skip:
	dec e
	ld a,-1
	cp e
	jr nz,runge_update_final_x_y_loop
	ret

runge_save_y_cache:;OP6=y, OP5=x, E=equation number
	;fix:modify to store all y values at once
	push de
	call runge_lookup_simple_cache
	;FIX:use proper equates
	ld a,cacheRungeSimpleValidMask
	or (hl)
	ld (hl),a
	inc hl
	ex de,hl
	ld hl,OP5
	ld bc,9
	ldir
	ex de,hl
	pop de
	call runge_HL_plus_E_times_9
	ex de,hl
	ld bc,9
	ld hl,OP6
	ldir
	ret

runge_load_OP2:;hl=M*256+E
	push hl
	B_CALL ZeroOP2
	pop hl
	ld (OP2+1),hl
	ret

runge_lookup_simple_cache:
	push de
	call LookupAppVar
	ex de,hl
	inc hl
	inc hl
	pop de
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

;CACHE:
;0			statusbits
;1..9		X
;10..63	Y1..Y6
;REST OF CACHE....
