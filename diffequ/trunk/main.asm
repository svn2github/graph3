 nolist
 include "..\ti83plus.inc"
 globals on
;This is the application header definition area required for all apps.
 ;Field: Program length
	DB 080h,0Fh
;Length=0 (N/A for unsigned apps)
	DB 00h,00h,00h,00h 
;Field: Program type
	DB 080h,012h
	DB 01h,04h
;Field: App ID
	DB 080h,021h
;Id = 1
	DB 01h
;Field: App Build
	DB 080h,031h
	;Build = 1
	DB 01h
;Field: App Name
	DB 080h,048h
;App Name = APPNAME
	DB "Diffequ "
;Field: App Pages
	DB 080h,081h
;App Pages = 1
	DB 01h
;No default splash screen
	DB 080h,090h
;Field: Date stamp - 5/12/1999
	DB 003h,026h,009h,004h,004h,06fh,01bh,080h
;Dummy encrypted TI date stamp signature
	DB 002h ,00dh ,040h                             
	DB 0a1h ,06bh ,099h ,0f6h ,059h ,0bch ,067h 
	DB 0f5h ,085h ,09ch ,009h ,06ch ,00fh ,0b4h ,003h ,09bh ,0c9h 
	DB 003h ,032h ,02ch ,0e0h ,003h ,020h ,0e3h ,02ch ,0f4h ,02dh 
	DB 073h ,0b4h ,027h ,0c4h ,0a0h ,072h ,054h ,0b9h ,0eah ,07ch 
	DB 03bh ,0aah ,016h ,0f6h ,077h ,083h ,07ah ,0eeh ,01ah ,0d4h 
	DB 042h ,04ch ,06bh ,08bh ,013h ,01fh ,0bbh ,093h ,08bh ,0fch 
	DB 019h ,01ch ,03ch ,0ech ,04dh ,0e5h ,075h 
;Field: Program Image length
	DB 80h,7Fh
	DB 0,0,0,0    ;Length=0, N/A
	DB 0,0,0,0    ;Reserved
	DB 0,0,0,0    ;Reserved
	DB 0,0,0,0    ;Reserved
	DB 0,0,0,0	;Reserved
 list

_EnableParserHook equ 5026h

StartApp:
	in a,(6)
	ld hl,parser_hook
	B_CALL EnableParserHook

	B_JUMP JForceCmdNoChar

parser_hook:
	db 83h
	or a
	jr z,parser_hook_return;preparser
	push hl
	ld hl,8A8Ah
	or a
	sbc hl,bc
	pop hl
	jr nz,parser_hook_return
	ld a,l
	dec a
	dec a 
	or h;only Z for HL=2
	jr z,parser_hook_real
parser_hook_return:
	xor a
	ret

parser_hook_argument_error:
	B_JUMP ErrArgument

parser_hook_real:
	B_CALL CkPosInt
	jr nz,parser_hook_argument_error;second arg not posint
	B_CALL ConvOP1
	xor a
	or d
	jr nz,parser_hook_argument_error;second arg too big (>255)
	ld a,e
	cp 6
	jr nc,parser_hook_argument_error;second arg too big (>5)
	push de
	B_CALL PopRealO1
	B_CALL CkPosInt
	pop de
	jr nz,parser_hook_argument_error;first arg not posint
	push de
	B_CALL ConvOP1
	ld e,a
	sub 34 ;not yet used by omnicalc
	or d
	pop de
	jr nz,parser_hook_argument_error
	;all checks passed, E contains equation number
	call euler
	or 0FFh
	ret

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
	rst rPUSHREALO1
	rst rOP1TOOP2
	ld a,X0
	B_CALL RclSysTok
	B_CALL OP1ExOP2
	B_CALL FPSub
	rst rOP1TOOP2
	ld a,Xstep
	B_CALL RclSysTok
	B_CALL OP1ExOP2
	B_CALL FPDiv
	B_CALL ConvOP1
	;DE=(X-X0)/Xstep -> aantal stappen
	pop bc
	push de
	push bc
	ld a,X0
	B_CALL RclSysTok
	B_CALL StoX
	pop de
	push de
	call euler_load_yi0
	rst rPUSHREALO1
	pop de
	pop bc
	ld a,b
	or c
	jr z,euler_loop_end
	push bc
	push de


euler_loop_start:
	call euler_load_estep
	B_CALL ConvOP1
	pop bc
	push de
	push bc
euler_loop2_start:;Estep loop
	;FPS=FPS+Y*(X)*Xstep/Estep
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
	B_CALL PopRealO2
	rst rFPADD
	rst rPUSHREALO1
	;X=X+Xstep/Estep
	call euler_load_estep
	rst rOP1TOOP2
	ld a,Xstep
	B_CALL RclSysTok
	B_CALL FPDiv
	rst rOP1TOOP2
	B_CALL RclX
	rst rFPADD
	B_CALL StoX

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
	pop bc
	pop de
euler_loop_end:

	;do final euler iteration here (OP1=FPS+Y*(X)*(X-Xend))
	;OPTIMIZE: only use parseinp when not multiplied by zero
	call euler_load_equation
	B_CALL ParseInp
	B_CALL CkOP1Real
	jr nz,parser_hook_argument_error
	B_CALL OP1ToOP3
	B_CALL RclX
	rst rOP1TOOP2
	B_CALL CpyTo1FPS1
	B_CALL FPSub
	B_CALL OP3ToOP2
	B_CALL FPMult
	B_CALL PopRealO2
	rst rFPADD
	rst rOP1TOOP2
	;
	B_CALL PopRealO1
	B_CALL StoX
	B_CALL OP2ToOP1
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
	

;NOT IMPLEMENTED:function may contain y1(=real(34,0))
;NOT IMPLEMENTED:caching previous result for speedup
;NOT IMPLEMENTED:support Yi* parameter
