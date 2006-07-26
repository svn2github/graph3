SlopeField:
	call load_status_address
	bit SlopeFldBit,(hl)
	ret z
	inc hl
	ld d,(hl)
	call CountEquations
	or a
	ret z;no equations selected
	dec a
	jr nz,SlopeField_Invalid;more than one equation used during graphing
	call load_fldres
	rst rOP1TOOP2
	B_CALL ConvOP1
	or d
	ret z;fieldres=0

	;E=Fldres_X
	push de
	B_CALL PushRealO2
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
	;FPS=[X_StepSize,...]

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
	;FPS=[Fldres_Y,X_StepSize,...]

	ld a,YMINt
	B_CALL RclSysTok
	rst rOP1TOOP2
	ld a,YMAXt
	B_CALL RclSysTok
	B_CALL FPSub
	B_CALL PopRealO2
	B_CALL FPDiv ;Y_StepSize = (Ymax-Ymin)/Fieldres_Y
	rst rPUSHREALO1
	;FPS=[Y_StepSize,X_StepSize,...]

	pop de
	push de


	B_CALL PopRealO1
	B_CALL PopRealO1
	pop de
	ret

SlopeField_Invalid:
	B_JUMP ErrInvalid
