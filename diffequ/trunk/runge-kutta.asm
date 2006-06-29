runge:
	push de
	B_CALL RclX
	rst rPUSHREALO1;save X
	pop de
	push de
	call lookup_cache
	bit 7,(hl)
	jr z,$f
	inc hl
	push hl
	B_CALL CpyTo1FPST
	pop hl
	push hl
	B_CALL Mov9OP2Cp
	pop hl
	jr nz,$f
	ld de,9
	add hl,de
	rst rMOV9TOOP1
	rst rOP1TOOP2
	jr runge_return_cache
$$:
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
	rst rPUSHREALO1;stepsize
	call load_yi0
	rst rPUSHREALO1 
	B_CALL OP1ToOP6
	;start RK
	ld hl,(fps)
	ld de,-9
	add hl,de
	push hl
	pop ix
	;FPS=[Y,step,X,X_target,...] 
	pop de
	push de
	call runge_save_y_cache

	pop de
	push de
	call runge_execute_equ ;FPS=[f1,Y,step,X,...]

	pop de
	push de
	ld a,50h
	call runge_update_x_y

	pop de
	push de
	call runge_execute_equ ;FPS=[f2,f1,Y,step,X,...]

	pop de
	push de
	ld a,75h
	call runge_update_x_y

	pop de
	push de
	call runge_execute_equ ;FPS=[f3,f2,f1,Y,step,X,...]

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
	rst rPUSHREALO1 ;FPS=[Yn+1,f3,f2,f1,Y,step,X,...]
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
	call runge_execute_equ ;FPS=[f4,Yn+1,f3,f2,f1,Y,step,X,...]

	B_CALL PopRealO2
	B_CALL PopRealO6 ;save Yn+1
	ld hl,

	;end RK
	;FIX:invalidate cache after RK is finished
	B_CALL PopRealO1
	B_CALL PopRealO1
	B_CALL PopRealO1
	B_CALL PopRealO1
	B_CALL PopRealO1
	B_CALL PopRealO1
	B_CALL PopRealO1
	B_CALL PopRealO1
runge_return_cache:
	B_CALL PopRealO1
	B_CALL StoX
	B_CALL OP2ToOP1
	pop de
	ret

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
	push af
	push de
	push ix
	pop hl
	ld de,-9
	add hl,de
	rst rMOV9TOOP1
	B_CALL ZeroOP2
	pop de
	pop af
	push de
	ld hl,OP2
	ld (hl),d
	inc hl
	ld (hl),7Fh
	inc hl
	ld (hl),a
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
	ld a,80h 
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
