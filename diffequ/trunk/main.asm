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

_EnableParserHook 	equ 5026h
_EnableAppSwitchHook	equ 502Ch
_EnableReGraphHook	equ 4FEAh
_EnableRawKeyHook 	equ 4F66h
_EnableMenuHook		equ 5083h
_EnableWindowHook		equ 4FB1h
_WPutSEOL				equ 4522h
_SetNumWindow			equ 452Bh

StartApp:
	in a,(6)
	ld hl,parser_hook
	B_CALL EnableParserHook

	ld hl,app_switch_hook
	B_CALL EnableAppSwitchHook

	ld hl,regraph_hook
	B_CALL EnableReGraphHook

	ld hl,raw_key_hook
	B_CALL EnableRawKeyHook

	ld hl,MenuHook
	B_CALL EnableMenuHook

	ld hl,WindowHook
	B_CALL EnableWindowHook

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
	;fallthrough
execute_diffequ_algorithm:
	;call runge
	call euler
	or 0FFh
	ret
	
regraph_hook:
	db 83h
	cp 08h
	jr nz,regraph_hook_return
	
	ld a,(OP1+2)
	;X*t is even
	;Y*t is odd
	srl a
	jr nc,regraph_hook_X
	;process Y
	and 07h
	ld e,a
	call execute_diffequ_algorithm
	jr regraph_hook_return_NZ
regraph_hook_X:;just return T (already in OP1)
	call RclT
regraph_hook_return_NZ:
	or 0FFh
	ret

regraph_hook_return:
	xor a
	ret

set_graph_mode:
	ld a,(IY+grfModeFlags)
	and 0Fh;reset all graph modes
	or b ;set graph mode to parametric
	ld (IY+grfModeFlags),a
	set grfSimul,(IY+grfDBFlags)
	ret

app_switch_hook:
	db 83h
	ld b,1<<grfParamM
	push af
	call set_graph_mode
	pop af
	ld b,1<<grfFuncM
	cp cxTableEditor
	jr z,set_graph_mode;let set_graph_mode return
	ret

CurTableRow equ 91DCh
CurTableCol equ 91DDh

raw_key_hook:
	db 83h
	push af
	ld a,(cxCurApp)
	cp cxTableEditor
	jr nz,raw_key_hook_no_table
	ld hl,(CurTableRow)
	dec l
	jr nz,raw_key_hook_skip
	xor a
	cp h
	jr z,raw_key_hook_skip
	pop af
	cp kUp
	jr z,raw_key_hook_ignore_key
	push af
raw_key_hook_no_table:
raw_key_hook_skip:
	pop af
	or a
	ret

raw_key_hook_ignore_key:
	xor a
	ret

MenuHook:;PORTED
	db	83h
	or	a
	jr	nz,MenuHook_Not0
	ld	a,(MenuCurrent)
	cp	39h ;Calc Menu (parametric mode)
	jr	z,MenuHook_ReplaceMenu
	cp 11h
	jr nz,MenuHook_Allow
MenuHook_ReplaceMenu:
	ld	hl,CalcMenu
	ld	de,tempSwapArea
	push	de
	call	Mov9
	pop	hl
	or	0FFh
	ret
MenuHook_Not0:
MenuHook_Allow:
	xor	a
	ret

CalcMenu:;PORTED
	db 1		;how many headers?
	db 1 	;how many choices under 1st header?
	db 24h	;string table entry for "CALCULATE"
	db 0,kEval ;these are 2-byte keypresses of each entry

WindowHook:;PORTED
	db 83h
	push	af
	ld	a,(cxCurApp)
	cp	kWindow
	jr	z,WindowHook_Possible
	pop	af
	xor	a
	ret
WindowHook_Possible:
	pop	af
;	call	CheckGraphMode
;	ret	z
	or	a
	jr	nz,WindowHook_Not0
	or	0FFh
	ld	a,(ix)
	ret
WindowHook_Not0:
	dec	a
	jr	nz,WindowHook_Not1
	or	0FFh
	ld	a,(ix+3)
	ret
WindowHook_Not1:
	dec	a
	jr	nz,WindowHook_Not2
	or	0FFh
	ld	a,(ix-3)
	ret
WindowHook_Not2:
	dec	a
	jr	nz,WindowHook_Not3
	ld a,(ix)
	cp WindowHook_Identifier
	jr nc,WindowHook_3NoToken
	ld d,tVarSys
	ld e,a
	B_CALL PutTokString
	jr WindowHook_3Token
WindowHook_3NoToken:
	ld	l,(ix+1)
	ld	h,(ix+2)
	call	PutsApp
WindowHook_3Token
	ld	a,'='
	B_CALL PutC
	B_CALL SetNumWindow
	jr	WindowHook_DispNum
WindowHook_Not3:
	dec	a
	ret	z;A=04h return Z
	dec	a
	jr	nz,WindowHook_Not5
	ld a,(hl)
	call WindowHook_RclValue
	ld	hl,OP1
	or	0FFh
	ret
WindowHook_Not5:
	dec	a
	jr	nz,WindowHook_Not6
	set	graphDraw,(iy + graphFlags)
	ld a,(hl)
	cp WindowHook_Identifier
	jr nc,WindowHook_6NoToken
	B_CALL StoSysTok
	jr WindowHook_6Token
WindowHook_6NoToken:
	rst rPUSHREALO1
	call load_diftol_address
	ex de,hl
	B_CALL PopReal
WindowHook_6Token:
	ld	hl,OP1
	or	0FFh
	ret
WindowHook_Not6:
	dec	a
	jr	nz,WindowHook_Not7
	ld	hl,WindowHook_Table + 3
WindowHook_Disallow:
	or	0FFh
	ret
WindowHook_Not7:
	dec	a
	ret	z
	dec	a
	jr	nz,WindowHook_Not9
WindowHook_DispNum:
	ld a,(ix)
	call WindowHook_RclValue
	ld	a,15h
	B_CALL FormEReal
	ld	hl,OP3
	B_CALL WPutSEOL
	or	0FFh
	ret
WindowHook_Not9:
	dec	a
	jr	nz,WindowHook_Not10
	ld	a,b
	cp	kLastEnt
	jr	z,WindowHook_Disallow
WindowHook_Not10:
WindowHook_Allow:
	xor	a
	ret

WindowHook_RclValue:
	cp WindowHook_Identifier
	jr nc,WindowHook_RclValueNoToken
	B_CALL RclSysTok
	ret
WindowHook_RclValueNoToken:
	call load_diftol
	ret

WindowHook_Table:
	db 0FFh
	dw 0
	db TMINt
	dw 0
	db TMAXt
	dw 0
	db TSTEPt
	dw 0
	db XMINt
	dw 0
	db XMAXt
	dw 0
	db XSCLt
	dw 0
	db YMINt
	dw 0
	db YMAXt
	dw 0
	db YSCLt
	dw 0
	db 0FEh
	dw WindowHook_DifTolStr
	db 0FFh

WindowHook_DifTolStr:
	db "Diftol",0

WindowHook_Identifier equ 0FEh ;first identifier-1 that's not a system variable token

PutsApp:;PORTED
	rst	20h
	ld	hl,OP1
	B_CALL PutS
	ret

Mov9:;PORTED
	ldi 
 	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	ret

create_appvar:;PORTED
	ld hl,AppvarName
	rst rMOV9TOOP1
	B_CALL ChkFindSym
	jr nc,Exists
	ld hl,AppvarInitSize
	B_CALL CreateAppVar
	inc de
	inc de
	ex de,hl
	ld bc,rungeCacheSize
	push hl
	push bc
	B_CALL MemClear
	pop hl
	pop bc
	add hl,bc
	ex de,hl
	ld hl,appvarInitData
	ld bc,appvarInitDataLength
	ldir
Exists:
	ld a,b
	or a
	jr z,NotArchived
	B_CALL Arc_Unarc
NotArchived:
	ret

LookupAppVar:;PORTED
	ld	hl,AppvarName
	rst	20h
	B_CALL	ChkFindSym
	ret	nc
	B_JUMP	ErrUndefined

AppvarName:
	db AppVarObj,"Diffequ",0

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

load_diftol_address:
	call LookupAppVar
	ld hl,2+rungeCacheSize+1
	add hl,de
	ret

load_diftol:
	call load_diftol_address
	rst rMOV9TOOP1
	ret
	

RclT:
	ld a,(cxCurApp)
	cp cxTableEditor
	jr nz,RclT_T
	B_CALL RclX
	ret
RclT_T:
	B_CALL TName
	B_CALL RclVarSym
	ret

StoT:
	ld a,(cxCurApp)
	cp cxTableEditor
	jr nz,$f
	B_CALL StoX
$$:
	B_CALL StoT
	ret


X0 equ TMINt
Xstep equ TSTEPt

	include "euler.asm"
	include "runge-kutta.asm"

cacheSwitchBit					equ 0
cache1ValidBit					equ 1
cache2ValidBit					equ 2

cacheSwitchMask				equ 1<<cacheSwitchBit
cache1ValidMask				equ 1<<cache1ValidBit
cache2ValidMask				equ 1<<cache2ValidBit

appvarInitData:
	db 0;statusbits
appvarInitDataDiftol:
	db 00h,7Dh,10h,00h,00h,00h,00h,00h,00h ;.001 diftol
appvarInitDataEnd:
appvarInitDataLength equ appvarInitDataEnd-appvarInitData

;APPVAR
;0..316		Cache (size of runge cache)
;317			Statusbits
;318..326	Diftol

AppvarInitSize 	equ rungeCacheSize+appvarInitDataLength ;runge cache is larger than euler cache


end_of_app:
app_size equ end_of_app-4080h
;NOT IMPLEMENTED:support Yi* parameter
;NOT IMPLEMENTED:disallow y1(..) calls inside ODE's
;NOT IMPLEMENTED:reset diftol when zoom:Zstandard (and tstep,tmax to our own values?)
