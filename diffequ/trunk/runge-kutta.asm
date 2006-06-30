runge:
	push de
	B_CALL RclX
	rst rPUSHREALO1;save X
	pop de
	push de
	call lookup_cache
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
	ld de,9
	add hl,de
	rst rMOV9TOOP1
	rst rOP1TOOP2
	pop de
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
	ld (OP1),a
	rst rPUSHREALO1;stepsize
	call load_yi0
	rst rPUSHREALO1 
	B_CALL OP1ToOP6
	ld hl,(fps)
	ld de,-9
	add hl,de
	push hl
	pop ix
runge_loop_start:
	;start RK
	;FPS=[Y,step,X,X_target,...] 
	pop de
	push de
	call runge_save_y_cache

	pop de
	push de
	call runge_execute_equ ;FPS=[f1,Y,step,X,X_target,...]

runge_loop_f1_known:
	pop de
	push de
	ld a,50h
	call runge_update_x_y

	pop de
	push de
	call runge_execute_equ ;FPS=[f2,f1,Y,step,X,X_target,...]

	pop de
	push de
	ld a,75h
	call runge_update_x_y

	pop de
	push de
	call runge_execute_equ ;FPS=[f3,f2,f1,Y,step,X,X_target,...]

	ld hl,runge_f1_inc
	rst rMOV9TOOP1
	B_CALL CpyTo2FPS2;f1
	B_CALL FPMult
	B_CALL OP1ToOP4

	B_CALL CpyTo2FPS1;f2
	ld hl,runge_f2_inc
	call runge_mult_add
	B_CALL OP1ToOP4

	B_CALL CpyTo2FPST;f3
	ld hl,runge_f3_inc
	call runge_mult_add
	B_CALL CpyTo2FPS4 ;step
	B_CALL FPMult
	B_CALL CpyTo2FPS3 ;Y
	rst rFPADD ;Y+h(2/9*f1+1/3*f2+4/9*f3)
	rst rPUSHREALO1 ;FPS=[Yn+1,f3,f2,f1,Y,step,X,X_target,...]
	B_CALL OP1ToOP6

	B_CALL CpyTo2FPS5 ;step
	B_CALL CpyTo1FPS6 ;X
	rst rFPADD ;X+h
	B_CALL OP1ToOP5
	B_CALL StoX

	pop de
	push de
	call runge_save_y_cache

	pop de
	push de
	call runge_execute_equ ;FPS=[f4,Yn+1,f3,f2,f1,Y,step,X,X_target,...]

	B_CALL PopRealO2 ;f4
	B_CALL OP2ToOP5
	B_CALL PopRealO6 ;save Yn+1
	ld hl,runge_f4_err
	rst rMOV9TOOP1
	B_CALL FPMult
	B_CALL OP1ToOP4	

	ld hl,runge_f3_err
	call runge_mult_add_pop
	B_CALL OP1ToOP4

	ld hl,runge_f2_err
	call runge_mult_add_pop
	B_CALL OP1ToOP4

	ld hl,runge_f1_err
	call runge_mult_add_pop
	B_CALL CpyTo2FPS1 ;step
	B_CALL FPMult
	xor a
	ld (OP1),a;abs
	;OP1=abs(h(5/72*f1-1/12*f2-1/9*f3+1/8*f4))  the error estimate
	B_CALL PushRealO6 ;Yn+1
	B_CALL PushRealO5 ;f4 (for FSAL)
	rst rPUSHREALO1
	;FPS=[errest,f4,Yn+1,Y,step,X,X_target,...]
	ld a,(OP1M)
	or a
	jr nz,runge_guess_formula
	;zero error, double stepsize
	B_CALL CpyTo1FPS4
	ld hl,20h*256+80h;2
	call runge_load_OP2
	B_CALL FPMult
	jr runge_guess_skip
runge_guess_formula:
	rst rOP1TOOP2
	call runge_load_tolerance
	B_CALL FPDiv
	ld hl,25h*256+7Fh;.25
	call runge_load_OP2
	B_CALL YToX
	ld hl,90h*256+7Fh;.9
	call runge_load_OP2
	B_CALL FPMult
	B_CALL CpyTo2FPS4;h
	B_CALL FPMult
	;.9*h*(tol/abs(errest))^(1/4)
runge_guess_skip:
	B_CALL PopRealO2
	rst rPUSHREALO1;FPS=[step_guess,f4,Yn+1,Y,step,X,X_target,...]
	;OP2=errest
	
	call runge_load_tolerance
	B_CALL CpOP1OP2
	push af
	ld hl,20h*256+80h;2
	jr nc,runge_errest_skip
	ld hl,50h*256+7Fh;.5
runge_errest_skip:
	call runge_load_OP2
	B_CALL CpyTo1FPS4 ;step
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
	;FPS=[f4,Yn+1,Y,step,X,X_target,...]
	B_CALL CpyTo2FPS3 ;step
	pop af
	push af
	ld (OP1),a
	B_CALL CpyO1ToFPS3 ;update step
	B_CALL CpyTo1FPS4 ;X
	rst rFPADD
	B_CALL CpyO1ToFPS4 ;update X
	B_CALL CpyTo2FPS5 ;X_target
	pop af
	push af
	or a
	jr z,$f
	B_CALL OP1ExOP2 ;correct for negative stepsize
$$:
	B_CALL CpOP1OP2
	push af
	B_CALL PopRealO1;f4
	B_CALL PopRealO2;Yn+1
	B_CALL PopRealO3;Y
	B_CALL PushRealO2
	rst rPUSHREALO1
	;FPS=[f4,Yn+1,step_next,X+step,X_target,...]
	pop af
	jr z,runge_exit_loop;X+step=X_target
	jr c,runge_loop_f1_known;X+step<X_target
runge_exit_loop:	
	;end RK
	;invalidate cache after RK is finished
	pop de
	call lookup_cache
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
	;FPS=[f4,Yn+1,Y,step,X,X_target,...]
	B_CALL PopRealO2
	B_CALL PopRealO2
	pop af
	push af
	ld (OP1),a
	B_CALL CpyO1ToFPS1
	;FPS=[Y,step_updated,X,X_target,...]
	;update X
	B_CALL CpyTo1FPS2
	B_CALL OP1ToOP5
	B_CALL StoX
	;update Y
	B_CALL CpyTo6FPST
	jr runge_loop_start

runge_mult_add_pop:
	push hl
	B_CALL PopRealO2
	pop hl
runge_mult_add:
	rst rMOV9TOOP1
	B_CALL FPMult
	B_CALL OP4ToOP2
	rst rFPADD
	ret	

runge_execute_equ:
	call load_equation
	push ix
	B_CALL ParseInp
	pop ix
	B_CALL CkOP1Real
	jr nz,parser_hook_argument_error
	rst rPUSHREALO1;fx
	ret

runge_update_x_y:
	;IX=[Y,step,X]
	push de
	push af

	push ix
	pop hl
	ld de,-9
	add hl,de
	rst rMOV9TOOP1
	pop hl
	ld l,7Fh
	call runge_load_OP2
	;OP2=a*0.1
	B_CALL FPMult;h*a*0.1
	B_CALL OP1ToOP3

	push ix
	pop hl
	ld de,-9*2
	add hl,de
	rst rMOV9TOOP1
	B_CALL OP3ToOP2
	rst rFPADD
	B_CALL OP1ToOP5
	B_CALL StoX;X=X+h*a*0.1

	B_CALL CpyTo1FPST ;fx
	B_CALL OP3ToOP2
	B_CALL FPMult
   B_CALL OP1ToOP2
	push ix
	pop hl
	rst rMOV9TOOP1
	rst rFPADD ;Y+h*a*0.1*fx
	B_CALL OP1ToOP6
	pop de
	;fallthrough 
runge_save_y_cache:;OP6=y, OP5=x, E=equation number
	call lookup_cache
	;FIX:use proper equates
	ld a,cacheRungeSimpleValidMask
	or (hl)
	ld (hl),a
	inc hl
	ex de,hl
	ld hl,OP5
	ld bc,9
	ldir
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
