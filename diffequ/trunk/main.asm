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
	DB 0,0,0,0 ;Length=0, N/A
	DB 0,0,0,0 ;Reserved
	DB 0,0,0,0 ;Reserved
	DB 0,0,0,0 ;Reserved
	DB 0,0,0,0	;Reserved
 list

_EnableParserHook equ 5026h

StartApp:
	in a,(6)
	ld hl,parser_hook
	B_CALL EnableParserHook

	call create_appvar

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
	push de
	call runge
	pop de
	;call euler
	nop
	nop
	nop
	or 0FFh
	ret
	
create_appvar:
	ld hl,AppvarName
	rst rMOV9TOOP1
	B_CALL ChkFindSym
	jr nc,Exists
	ld hl,AppvarInitSize
	B_CALL CreateAppVar
	inc de
	inc de
	ex de,hl
	ld bc,AppvarInitSize
	B_CALL MemClear
Exists:
	ld a,b
	or a
	jr z,NotArchived
	B_CALL Arc_Unarc
NotArchived:
	ret

LookupAppVar:
	ld	hl,AppvarName
	rst	20h
	B_CALL	ChkFindSym
	ret	nc
	B_JUMP	ErrUndefined

AppvarName:
	db AppVarObj,"Diffequ",0

;	db 0 status bits
;	db 0,0,0,0,0,0,0,0,0 ;1;x1
;	db 0,0,0,0,0,0,0,0,0 ;1;y1(x1)
;	db 0,0,0,0,0,0,0,0,0 ;2;x1
;	db 0,0,0,0,0,0,0,0,0 ;2;y1(x1)

cacheSwitchBit					equ 0
cache1ValidBit					equ 1
cache2ValidBit					equ 2
cacheRungeSimpleValidBit	equ 7

cacheSwitchMask				equ 1<<cacheSwitchBit
cache1ValidMask				equ 1<<cache1ValidBit
cache2ValidMask				equ 1<<cache2ValidBit
cacheRungeSimpleValidMask	equ 1<<cacheRungeSimpleValidBit
cache_size_per_equ equ 9*4+1
AppvarInitSize equ cache_size_per_equ*6

same_sign: ;Z if same sign of OP1 and OP2
	ld a,(OP1)
	ld hl,OP2
	xor (hl)
	and 80h
	ret

load_yi0:
	ld hl,load_yi0_value
	rst rMOV9TOOP1
	ret
load_yi0_value:
	db 0,80h,10h,0,0,0,0,0,0

load_equation:
	ld a,e
	ld hl,equation
	rst rMOV9TOOP1
	ld hl,OP1+2
	add (hl)
	ld (hl),a
	ret
equation:
	db EquObj, tVarEqu, tY7,0


X0 equ TMINt
Xstep equ TSTEPt

	include "euler.asm"
	include "runge-kutta.asm"

;NOT IMPLEMENTED:support Yi* parameter
;NOT IMPLEMENTED:disallow y1(..) calls inside ODE's
