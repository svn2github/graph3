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
_EnableGraphHook		equ 4FB7h
_EnableYEquHook		equ 4FCFh
_WPutSEOL				equ 4522h
_SetNumWindow			equ 452Bh

_SetCursorHook			equ 4F60h
_Mon						equ 401Eh
_newContext0			equ 4033h
_SendKPress				equ 4024h
_JForceCmd				equ 402Ah
_restoreTR				equ 458Bh
_saveTR					equ 4588h
_cxPutAway				equ 4036h
_POPCX					equ 49E1h
_SetEmptyEditPtr		equ 4969h
_OP1ToEdit				equ 49A5h
_BufClear				equ 4936h
_UpdateYEqu				equ 49CCh
_CursorToStart			equ 4939h;4945h
_OpenEditEqu			equ 49C3h
_CursorDown				equ 4948h
_IsA2ByteTok			equ 42A3h

GX1						equ 9780h
YEquHookState			equ 9B98h+3

StartApp:
	call create_appvar

	ld a,(YEquHookState)
	cp 42h
	jr z,Restore

	call SetupCalc

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

	ld hl,GraphHook
	B_CALL EnableGraphHook

	ld hl,YEquHook
	B_CALL EnableYEquHook

ExitApp:
	B_JUMP JForceCmdNoChar

Restore:
	call RestoreCalc
	jr ExitApp

parser_hook:
	db 83h
	call SetCalcSpeed
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
	push de
	call load_status_address
	pop de
	ld d,(hl)
	push de
	bit RealEquBit,d
	jr z,DataTypeError;bomb out when the equations aren't real

	call RclT
	rst rPUSHREALO1;save X
	
	
	call load_simple_cache_address
	push hl
	B_CALL CpyTo1FPST;X
	pop hl
	bit cacheSimpleValidBit,(hl)
	jr z,execute_diffequ_algorithm_skip_simple_cache
	inc hl
	push hl
	B_CALL Mov9OP2Cp
	pop hl
	jr nz,execute_diffequ_algorithm_skip_simple_cache
	push hl
	ld de,9
	B_CALL DeallocFPS1
	pop hl
	pop de
	dec e;skip X
	call runge_HL_plus_cache_offset
	rst rMOV9TOOP1
	or 0FFh
	ret
execute_diffequ_algorithm_skip_simple_cache:


	pop ix
	push ix
	apponerr execute_diffequ_algorithm_error
	push ix
	pop de
	bit EulerBit,d
	jr nz,execute_diffequ_algorithm_RK
	call euler
execute_diffequ_algorithm_finish:
	appofferr
	pop ix 
	B_CALL PopRealO1
	call StoT
	B_CALL OP2ToOP1
	or 0FFh
	ret

execute_diffequ_algorithm_RK:
	call runge
	jr execute_diffequ_algorithm_finish

DataTypeError:
	B_JUMP ErrDataType

execute_diffequ_algorithm_error:
	pop de
	push af
	bit EulerBit,d
	call nz,ClearCache;Simple erase cache in case of RK
	B_CALL PopRealO1
	call StoT
	pop af
	ld b,a
	and 7Fh
	cp 6;ON:BREAK
	jr z,DisplayError
DisplayOriginalError:
	ld a,b
DisplayError:
	B_JUMP JError

regraph_hook:
	db 83h
	call SetCalcSpeed
	cp 09h
	jr nz,regraph_hook_not9
	call SlopeField
	jr regraph_hook_return
regraph_hook_not9:
	cp 08h
	jr nz,regraph_hook_return

	AppOnErr regraph_hook_error
	ld a,(OP1+2)
	;X*t is even
	;Y*t is odd
	srl a
	jr nc,regraph_hook_X
	;process Y
	and 07h
	ld e,a
	call execute_diffequ_algorithm
	AppOffErr
	jr regraph_hook_return_NZ
regraph_hook_X:;just return T (already in OP1)
	set numOP1,(IY+ParsFlag2)
	call RclT
	AppOffErr
regraph_hook_return_NZ:
	or 0FFh
	ret

regraph_hook_return:
	xor a
	ret

regraph_hook_error:
	res write_on_graph,(IY+sGrFlags)
	res numOP1,(IY+ParsFlag2)
	ld b,a
	and 7Fh
	cp 8 ;only ignore the first errors
	jr nc,DisplayOriginalError
	ld hl,equation
	rst rMOV9TOOP1
	or 0FFh
	ret

set_graph_mode:
	ld a,(IY+grfModeFlags)
	and 0Fh;reset all graph modes
	or b ;set graph mode to parametric
	ld (IY+grfModeFlags),a
	set grfSimul,(IY+grfDBFlags)
	set 0,(IY+24);set ExprOff because we don't want to see the X*T and Y*T expressions
	ret

RemoveGoto_ErrorHandler:
	and 07Fh
	jp DisplayError

app_switch_hook:
	db 83h
	call SetCalcSpeed
	push af
	push hl
	ld a,b
	cp kYequ
	call z,app_switch_hook_ExitYEqu
	ld b,1<<grfParamM
	call set_graph_mode
	pop hl
	pop af
	ld b,1<<grfFuncM
	cp cxTableEditor
	jr z,set_graph_mode;let set_graph_mode return
	cp kFormat
	jr z,ModeHook
	ret

app_switch_hook_ExitYEqu:
	apponerr RemoveGoto_ErrorHandler
	;fill simple cache with T
	call load_simple_cache_address
	set cacheSimpleValidBit,(hl)
	inc hl
	push hl
	call RclT
	pop de
	ld b,7
$$:
	push bc
	ld hl,OP1
	call Mov9
	pop bc
	djnz $b
	call load_status_address
	set RealEquBit,(hl)
	ld e,0
app_switch_hook_ExitYEqu_loop:
	push de
	;Dis/Enable Y* function
	call load_equation
	rst rFINDSYM
	ld a,(hl)
	and 1<<5 ;the bit that dis/enables an equation
	pop de
	push de
	push af
	call load_Y_equation
	rst rFINDSYM
	ld a,(hl)
	res 5,a
	pop bc
	or b;dis/enable equation
	ld (hl),a
	pop de
	push de
	;Check that equations returns a real value
	call load_equation
	AppOnErr app_switch_hook_ExitYEqu_skipEqu
	B_CALL ParseInp
	AppOffErr
	bit numOP1,(IY+ParsFlag2)
	jr z,app_switch_hook_ExitYEqu_skipEqu;don't save
	B_CALL CkOP1Real
	call nz,ResetRealEquBit
app_switch_hook_ExitYEqu_skipEqu:
	pop de
	push de
	;Update initial value
	ld a,1
	call load_equation2
	AppOnErr app_switch_hook_ExitYEqu_skipInitial
	B_CALL ParseInp
	AppOffErr
	bit numOP1,(IY+ParsFlag2)
	jr z,app_switch_hook_ExitYEqu_skipInitial;don't save
	B_CALL CkOP1Real
	call nz,ResetRealEquBit
	jr nz,app_switch_hook_ExitYEqu_skipInitial;don't save
	B_CALL OP1ToOP6
	pop de
	push de 
	ld a,1
	call load_equation2
	rst rFINDSYM
	B_CALL SetEmptyEditPtr
	B_CALL OP6ToOP1
	B_CALL OP1ToEdit
	B_CALL CloseEditEqu
app_switch_hook_ExitYEqu_skipInitial:
	pop de
	inc e
	ld a,6;0..5
	cp e
	jr nz,app_switch_hook_ExitYEqu_loop
	B_CALL CleanAll
	;figure out which equations need to be evaluated in RK mode
	ld de,0
app_switch_hook_ExitYEqu_loop2:
	push de
	call load_equation
	rst rFINDSYM
	bit 5,(hl)
	ex de,hl
	pop de
	call nz,app_switch_hook_ExitYEqu_CheckEqu
	inc e
	ld a,6;0..5
	cp e
	jr nz,app_switch_hook_ExitYEqu_loop2
	;store which equations need to be evaluated in RK mode
	push de
	call LookupAppVar 
	push de
	pop bc
	ld hl,RKEvalOffset
	add hl,de
	pop de
	ld (hl),d
	;Erase cache if smart graph can't be used
	bit smartGraph_inv,(IY+smartFlags)
	call nz,ClearCache
	AppOffErr
	call load_simple_cache_address
	res cacheSimpleValidBit,(hl)
	ret

ResetRealEquBit:
	push af
	call load_status_address
	res RealEquBit,(hl)
	pop af
	ret
	

app_switch_hook_ExitYEqu_CheckEqu:
	push de
	ld a,1
	inc e
app_switch_hook_ExitYEqu_CheckEquLoop:
	dec e
	jr z,app_switch_hook_ExitYEqu_CheckEquSkip
	sla a
	jr app_switch_hook_ExitYEqu_CheckEquLoop
app_switch_hook_ExitYEqu_CheckEquSkip:
	pop de
	ld b,a
	and d
	ret nz;already checked
	ld a,d
	or b
	ld d,a
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
app_switch_hook_ExitYEqu_CheckEquLoop2:
	ld a,b
	or c
	ret z
	ld a,(hl)
	cp tVarEqu
	jr nz,app_switch_hook_ExitYEqu_CheckEquNotEqu
	inc hl
	ld a,(hl)
	sub tY1
	jr c,app_switch_hook_ExitYEqu_CheckEquNotY
	cp 6
	jr nc,app_switch_hook_ExitYEqu_CheckEquNotY
	;Found Y1..Y6 token
	push bc
	push hl
	ld e,a;D hasn't changed
	push de
	call load_equation
	rst rFINDSYM
	ex de,hl
	pop de
	call app_switch_hook_ExitYEqu_CheckEqu
	pop hl
	pop bc	
app_switch_hook_ExitYEqu_CheckEquNotY:
	dec hl
	ld a,(hl)
app_switch_hook_ExitYEqu_CheckEquNotEqu:
	B_CALL IsA2ByteTok
	inc hl
	dec bc
	jr nz,$f
	inc hl
	dec bc
$$:
	jr app_switch_hook_ExitYEqu_CheckEquLoop2

CurTableRow equ 91DCh
CurTableCol equ 91DDh

raw_key_hook:
	db 83h
	call SetCalcSpeed
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

GraphHook:
	db 83h
	call SetCalcSpeed
GraphHook_Not2:
	cp 06h
	jr nz,GraphHook_Not6
	ld a,b
	cp kStd
	jr nz,GraphHook_Allow
	push bc
	call load_diftol_address
	ex de,hl
	ld hl,appvarInitDataDiftol
	call Mov9
	call Mov9;copy FldRes
	pop bc
GraphHook_Not6:
GraphHook_Allow:
	xor a
	ret

YEquHook:
	db 83h
	call SetCalcSpeed
	sub 05h
	jr nz,YEquHook_Not5
	ld a,(EQS+7)
	and 0Fh
	sra a
	ld b,1
	inc a
$$:
	dec a
	jr z,$f
	sla b
	jr $b
$$:
	push bc
	call load_RKEval_address
	ld a,(hl)
	pop bc
	and b
	jr z,YEquHook_Allow
	B_CALL SetTblGraphDraw
	jr YEquHook_Allow
YEquHook_Not5:
	dec a
	jr nz,YEquHook_Not6
	ld a,(EQS+7)
	bit 0,a
	jr z,YEquHook_Allow;only do something when Y*T equations are selected
	bit 0,(IY+19)
	jr nz,YEquHook_Allow;Don't do anything when = is selected
	;The graphing style icon is never selected here because we immediately abort when X*T is edited
	ld a,b
	cp kEnter
	jr z,YEquHook_Evaluate
	cp kLeft
	jr z,YEquHook_CheckStart
	cp kUp
	jr z,YEquHook_CheckStart
	cp kDown
	jr z,YEquHook_CheckDown
	jr YEquHook_Allow
YEquHook_CheckDown:
	B_CALL CursorDown
	ld b,kEnter
	jr z,YEquHook_Evaluate
	or 0FFh
	ret
YEquHook_CheckStart:
	ld hl,(editTop)
	ld de,(editCursor)
	or a
	sbc hl,de
	jr nz,YEquHook_Allow
YEquHook_Evaluate:
	push bc
	B_CALL CursorToStart
	B_CALL CloseEditEqu

	ld a,(EQS+7)
	and 0Fh
	ld e,a
	sra e
	and 1
	call load_equation2
	B_CALL ParseInp
	B_CALL OP1ToOP6

	B_CALL OpenEditEqu
	B_CALL BufClear
	B_CALL OP6ToOP1
	B_CALL OP1ToEdit
	B_CALL CloseEditEqu ;Just close and open it to make sure that all pointers are correct

	B_CALL OpenEditEqu
	B_CALL UpdateYEqu
	pop bc
	jr YEquHook_Allow
YEquHook_Not6:
	dec a
	dec a
	jr nz,YEquHook_Not8
	ld a,(curCol)
	sub 3
	ld (curCol),a
	ld a,"Y"
	B_CALL PutC
	ld a,(EQS+7)
	ld b,a
	srl a
	and 00000111b
	add Lsub1
	bit 0,b
	jr nz,YEquHook_YiText
	B_CALL PutC
	ld a,"'"
	B_CALL PutC
	jr YEquHook_Allow
YEquHook_YiText:
	push af
	ld a,Li
	B_CALL PutC
	pop af
	B_CALL PutC
	jr YEquHook_Allow
YEquHook_Not8:
YEquHook_Allow:
	xor a
	ret


MenuHook:;PORTED
	db	83h
	call SetCalcSpeed
	or	a
	jr	nz,MenuHook_Not0
	ld	a,(MenuCurrent)
	cp 0Bh ;Vars-menu
	jr z,MenuHook_ReplaceVarsMenu
	cp	39h ;Calc Menu (parametric mode)
	jr	z,MenuHook_ReplaceCalcMenu
	cp 11h ;Calc Menu (function mode)
	jr z,MenuHook_ReplaceCalcMenu
	cp 1Bh ;Function-Menu
	jr nz,MenuHook_Allow
MenuHook_ReplaceFunctionMenu:
	ld hl,YEquMenu
	jr MenuHook_ReplaceMenu
MenuHook_ReplaceVarsMenu:
	ld hl,VarsMenu
	jr MenuHook_ReplaceMenu
MenuHook_ReplaceCalcMenu:
	ld hl,CalcMenu
MenuHook_ReplaceMenu:
	ld	de,tempSwapArea
	ld bc,25;longest menu implemented
	push	de
	ldir
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

VarsMenu:;25 bytes long
	db 2		;how many headers?
	db 7 	;how many choices under 1st header?
	db 3  ;how many choices under 2nd header?
	db 5Ah	;string table entry for "VARS"
	db 5Dh	;string table entry for "Y-VARS"
	db 82h,3Eh ;these are 2-byte keypresses of each entry
	db 82h,3Fh
	db 82h,40h
	db 82h,41h
	db 82h,43h
	db 82h,44h
	db 82h,42h
	db 82h,45h;second header
	db 82h,47h
	db 82h,48h

YEquMenu:
	db 1		;how many headers?
	db 6 	;how many choices under 1st header?
	db 30h	;string table entry for "FUNCTION"
	db kExtendEcho2,kY1 ;these are 2-byte keypresses of each entry
	db kExtendEcho2,kY2
	db kExtendEcho2,kY3
	db kExtendEcho2,kY4
	db kExtendEcho2,kY5
	db kExtendEcho2,kY6

WindowHook:;PORTED
	db 83h
	call SetCalcSpeed
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
	push af
	B_CALL StoSysTok
	pop af
	cp TMINt
	jr z,WindowHook_EraseCache
	cp TSTEPt
	jr z,WindowHook_EraseCache
	jr WindowHook_6Token
WindowHook_6NoToken:
	push af
	rst rPUSHREALO1
	pop af
	push af
	cp WindowHook_FldResIdentifier
	jr nz,WindowHook_NoError
	B_CALL CkPosInt
	jr nz,WindowHook_Error
	B_CALL ConvOP1
	cp 96/2+1
	jr nc,WindowHook_Error
	ld a,d
	or a
	jr z,WindowHook_NoError
WindowHook_Error:
	B_JUMP ErrDomain 
WindowHook_NoError:
	pop af
	call WindowHook_LoadValueAddress
	ex de,hl
	B_CALL PopReal
WindowHook_EraseCache:
	rst rPUSHREALO1
	call ClearCache
	B_CALL PopRealO1
WindowHook_6Token:
	ld	hl,OP1
	or	0FFh
	ret
WindowHook_Not6:
	dec	a
	jr	nz,WindowHook_Not7
	call load_status_address
	ld a,(hl)
	ld hl,WindowHook_TablePointers
	bit EulerBit,a
	jr z,$f
	ld de,4
	add hl,de
$$:
	bit SlopeFldBit,a
	jr z,$f
	inc hl
	inc hl
$$:
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
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
	call WindowHook_LoadValueAddress
	rst rMOV9TOOP1
	ret

WindowHook_LoadValueAddress:
	push af
	call LookupAppVar
	ld hl,FldresOffset+2
	add hl,de
	pop de
	ld e,d
	ld d,0FFh
	add hl,de
	ret

WindowHook_TablePointers:
	dw WindowHook_TableEuler+3
	dw WindowHook_TableEulerField+3
	dw WindowHook_TableRK+3
	dw WindowHook_TableRKField+3

WindowHook_TableEuler:
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
	db 0FFh

WindowHook_TableEulerField:
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
	db -2
	dw WindowHook_FldResStr
	db 0FFh

WindowHook_TableRK:
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
	db -2-9
	dw WindowHook_DifTolStr
	db 0FFh

WindowHook_TableRKField:
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
	db -2-9
	dw WindowHook_DifTolStr
	db -2
	dw WindowHook_FldResStr
	db 0FFh


WindowHook_DifTolStr:
	db "Diftol",0
WindowHook_DifTolIdentifier	equ -2-9
WindowHook_FldResStr:
	db "Fldres",0
WindowHook_FldResIdentifier	equ -2

WindowHook_Identifier equ -2-9 ;first identifier that's not a system variable token

;;;;;;;;;;;;;;;;Start of ModeHook PORTED from graph3

modeTemp		equ appBackUpScreen + 600
					; 742-749 contains data from saveStuff.. i was stupid
saveStuff	equ appBackUpScreen + 750

_Flags		equ appBackUpScreen +  30

ModeHook:
	ld	hl,97A2h	; current selection
	ld	(hl),0

	call load_status_address ;Destroys OP1
	ld a,(hl)
	ld (_Flags),a

	ld	de,saveStuff
	ld	hl,curRow
	ldi 
	ldi
	ld	hl,flags + textFlags
	ldi
	ld	hl,cursorHookPtr
	ldi 
	ldi 
	ldi
	ld	hl,flags + 34h
	ldi

	ld	sp,(onSP)
	ld	hl,ModeHook_gmMonVectors
	B_CALL AppInit

	set	saIndic,(iy + newIndicFlags)
	in	a,(6)
	ld	hl,CursorHook
	B_CALL SetCursorHook

	ld	a,cxextapps
	ld	(cxCurApp),a

	call	ModeHook_gmcxRedisp_ForSure
	B_JUMP Mon

ModeHook_BlinkItem:
	ld	b,(hl)
	ld	hl,curCol
	ld	a,(hl)
	or	a
	jr	z,ModeHook_BlinkSkip
	dec	b
	inc	(hl)
ModeHook_BlinkSkip:
	set	textInverse,(iy + textFlags)
ModeHook_BlinkLoop:
	ld	a,' '
	B_CALL PutC
	djnz	ModeHook_BlinkLoop
	res	textInverse,(iy + textFlags)
	ret

ModeHook_DrawItemX:
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	ld	(curRow),de
ModeHook_DrawItem:
	bit	1,(ix)
	jr	nz,ModeHook_NotLeft
	xor	a
	ld	(curCol),a
ModeHook_NotLeft:
	ld	a,(curCol)
	or	a
	jr	z,ModeHook_DrawNoSpace
	push	ix
	ld	a,' '
	B_CALL PutC
	pop	ix
ModeHook_DrawNoSpace:
	ld	l,(ix+3)
	ld	h,(ix+4)
	ld	a,(ix+5)
	and	(hl)
	cp	(ix+6)
	jr	nz,ModeHook_NotSel
	set	textInverse,(iy + textFlags)
ModeHook_NotSel:
	push	ix
	ld	h,(ix+1)
	ld	a,(ix+2)
	or a
	jr z,ModeHook_Token
	ld l,h
	ld h,a
	jr ModeHook_PutsLoop
ModeHook_Token:
	ld l,tGFormat
	ld	(OP1),hl
	ld	hl,OP1
	B_CALL Get_Tok_Strng
	ld	hl,OP3
ModeHook_PutsLoop:
	ld	a,(hl)
	inc	hl
	or	a
	jr	z,ModeHook_PutsDone
	push	hl
	B_CALL PutMap
	pop	hl
	ld	a,(curCol)
	inc	a
	ld	(curCol),a
	jr	ModeHook_PutsLoop
ModeHook_PutsDone:
	pop	ix
	bit	0,(ix)
	jr	nz,ModeHook_NotRight
	ld	a,(curRow)
	inc	a
	ld	(curRow),a
ModeHook_NotRight:
	res	textInverse,(iy + textFlags)
	ret

ModeHook_Table:
	db	00000101b	
	dw tRectG,  flags + grfDBFlags	
	db 1<<grfPolar,0	,0

	db	00000110b
	dw tPolarG,  flags + grfDBFlags	
	db 1<<grfPolar,1<<grfPolar	,0

	db	00001101b
	dw tCoordOn,  flags + grfDBFlags			
	db 1<<grfNoCoord,0	,0

	db	00001110b
	dw tCoordOff,  flags + grfDBFlags			
	db 1<<grfNoCoord,1<<grfNoCoord	,0

	db	00001101b
	dw tGridOff,  flags + grfDBFlags			
	db 1<<grfGrid,0	,1

	db	00001110b
	dw tGridOn,  flags + grfDBFlags			
	db 1<<grfGrid,1<<grfGrid	,1

	db	00001101b
	dw tAxisOn,  flags + grfDBFlags			
	db 1<<grfNoAxis,0	,1

	db	00001110b
	dw tAxisOff,  flags + grfDBFlags			
	db 1<<grfNoAxis,1<<grfNoAxis	,1

	db	00001101b
	dw tLblOff,  flags + grfDBFlags			
	db 1<<grfLabel,0	,0

	db	00001110b
	dw tLblOn,  flags + grfDBFlags			
	db 1<<grfLabel,1<<grfLabel	,0

	db	00001101b
	dw ModeHook_Strings_Euler,  _Flags			
	db 1<<EulerBit,0	,2

	db	00001110b
	dw ModeHook_Strings_RungeKutta,  _Flags			
	db 1<<EulerBit,1<<EulerBit	,2

	db	00001001b
	dw ModeHook_Strings_NoField,  _Flags			
	db 1<<SlopeFldBit,0	,1

	db	00001010b
	dw ModeHook_Strings_Field,  _Flags			
	db 1<<SlopeFldBit,1<<SlopeFldBit	,1

ModeHook_Strings_Euler:
	db "Euler",0
ModeHook_Strings_RungeKutta:
	db "RungeKutta",0

ModeHook_Strings_NoField:
	db "FieldOff",0
ModeHook_Strings_Field:
	db "FieldOn",0

ModeHook_gmMonVectors:
	dw ModeHook_gmcxMain,ModeHook_gmcxDummyRet,ModeHook_gmcxPutAway
	dw ModeHook_gmcxRedisp,ModeHook_gmcxDummyRet,ModeHook_gmcxDummyRet
	db 0

ModeHook_Quit:
	B_CALL POPCX
ModeHook_Quit_NoPop:
	ld	a,cxFormat
	ld	(cxCurApp),a

	call load_status_address ;Destroys OP1
	ld a,(_Flags)
	ld (hl),a

	ld	hl,saveStuff
	ld	de,curRow
	ldi 
	ldi
	ld	de,flags + textFlags
	ldi
	ld	de,cursorHookPtr
	ldi 
	ldi 
	ldi
	ld	de,flags + 34h
	ldi
	res	saIndic,(iy + newIndicFlags)
ModeHook_gmcxDummyRet:
	ret

ModeHook_gmcxPutAway:
	call	ModeHook_Quit
	bit	monAbandon,(iy + monFlags)
	jr	nz,ModeHook_Off
	ld	a,iall
	out	(intrptEnPort),a
	B_CALL LCD_DRIVERON
	set	onRunning,(iy + onFlags)
	ei
ModeHook_Off:
	B_JUMP cxPutAway

ModeHook_gmcxRedisp:
	ld	a,(kbdKey)
	cp	kClear
	ret	nz
ModeHook_gmcxRedisp_ForSure:
	B_CALL ClrScrnFull
	xor	a
	ld	(curRow),a

	ld	ix,ModeHook_Table
	ld	hl,modeTemp
ModeHook_BigLoop:
	push	hl
	ld	hl,(curRow)
	push	hl
	call	ModeHook_DrawItem
	pop	de
	pop	hl
	bit	1,(ix)
	jr	nz,ModeHook_NotLeftX
	ld	d,0
ModeHook_NotLeftX:
	ld	(hl),e
	inc	hl
	ld	(hl),d
	inc	hl
	ld	a,(curCol)
	sub	d
	ld	(hl),a
	inc	hl
	ld	a,(ix)
	and	00000101b
	ld	de,8
	add	ix,de
	jr	nz,ModeHook_BigLoop
	ret

ModeHook_gmcxMain:
	cp    kExtApps
	jr    nz,ModeHook_NotApps
	ld	a,kQuit
	;call  ModeHook_Quit
	;ld    hl,progCurrent
	;ld    de,progToEdit
	;ld    bc,8
	;ldir
	;bjump $4C51       ;execute app
ModeHook_NotApps:

	cp	kQuit
	jr	c,ModeHook_NotExternal
	cp    kTrace
	jr	z,ModeHook_GraphKey
	jr    nc,ModeHook_NotExternal
	cp	kGraph
	jr	z,ModeHook_GraphKey
	ld	hl,97A2h	; current selection
	ld	(hl),0
	cp	kFormat
	jp	z,ModeHook_gmcxRedisp_ForSure
	push	af
	push	bc
	call  ModeHook_Quit
	pop	bc
	pop	af
	ld    sp,(onSP)
	res   6,(iy + curFlags)
	B_CALL newContext0
	xor   a
	B_JUMP SendKPress
ModeHook_GraphKey:
	B_JUMP JForceGraphKey
ModeHook_NotExternal:

	cp	kLeft
	jr	nz,ModeHook_NotkLeft
	call	ModeHook_Lookup
	bit	1,(ix)
	ret	z
	call	ModeHook_DrawItemX
	ld	hl,97A2h
	dec	(hl)
ModeHook_Disallow:
	xor	a
	ret
ModeHook_NotkLeft:

	cp	kRight
	jr	nz,ModeHook_NotkRight
	call	ModeHook_Lookup
	bit	0,(ix)
	ret	z
	call	ModeHook_DrawItemX
	ld	hl,97A2h
	inc	(hl)
	xor	a
	ret
ModeHook_NotkRight:

	cp	kUp
	jr	nz,ModeHook_NotkUp
	call	ModeHook_Lookup
	bit	3,(ix)
	ret	z
	call	ModeHook_DrawItemX
	ld	hl,97A2h
	ld	de,-8
ModeHook_FindUp1:
	bit	1,(ix)
	jr	z,ModeHook_FindUp2
	dec	(hl)
	add	ix,de
	jr	ModeHook_FindUp1
ModeHook_FindUp2:
	dec	(hl)
	add	ix,de
	bit	1,(ix)
	jr	nz,ModeHook_FindUp2
	ret
ModeHook_NotkUp:

	cp	kDown
	jr	nz,ModeHook_NotkDown
	call	ModeHook_Lookup
	bit	2,(ix)
	ret	z
	call	ModeHook_DrawItemX
	ld	hl,97A2h
ModeHook_FindDown1M1:
	ld	de,8
ModeHook_FindDown1:
	inc	(hl)
	add	ix,de
	bit	1,(ix)
	jr	nz,ModeHook_FindDown1
	ret
ModeHook_NotkDown:

	cp	kClear
	jr	nz,ModeHook_NotClear
	call	ModeHook_Quit
	jp ExitApp ;B_JUMP JForceCmdNoChar
ModeHook_NotClear:

	cp	kEnter
	jp	nz,ModeHook_NotkEnter
	call	ModeHook_Lookup
	ex	de,hl
	ld	l,(ix+3)
	ld	h,(ix+4)
	ld	a,(ix+5)
	and	(hl)
	cp	(ix+6)
	ret	z
	ld a,(ix+7)
	or a
	jr z,ModeHook_DontReset
	set graphDraw,(IY+graphFlags)
	dec a
	jr z,ModeHook_DontReset
	;invalidate table cache
	set reTable,(IY+tblFlags)
	;erase cache when needed
	push hl
	push de
	push af
	call ClearCache
	pop af
	pop de
	pop hl
ModeHook_DontReset:
	ld	a,(ix+5)
	cpl
	and	(hl)
	or	(ix+6)
	ld	c,(hl)
	ld	(hl),a
	ld	a,(de)
	ld	h,0
	ld	l,a
	ld	(curRow),hl
	ld	de,-8
ModeHook_FindUp3:
	bit	1,(ix)
	jr	z,ModeHook_RedrawRow
	add	ix,de
	jr	ModeHook_FindUp3
ModeHook_RedrawRow:
	call	ModeHook_DrawItem
	bit	0,(ix)
	ld	de,8
	add	ix,de
	jr	nz,ModeHook_RedrawRow
	ret
ModeHook_NotkEnter:

	cp	kEOL + 1
	jr	nc,ModeHook_BadKey
ModeHook_RetAllow:
	xor	a
	ret

ModeHook_BadKey:
	push	af
	call	ModeHook_Quit
	pop	af
	B_JUMP JForceCmd

CursorHook:
	db	83h
	call SetCalcSpeed
	push	af
	call	ModeHook_Lookup
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	inc	hl
	ld	(curRow),de
	pop	af
	cp	24h
	jr	c,ModeHook_NZ
	call	ModeHook_DrawItem
	jr	ModeHook_Z
ModeHook_NZ	call	ModeHook_BlinkItem
ModeHook_Z	call	ModeHook_HandleIndic
	xor	a
	ret

ModeHook_HandleIndic:
	bit	saIndic,(iy + newIndicFlags)
	jr	z,ModeHook_UpdateIndic
	call	ModeHook_GetIndicChar
	jr	nz,ModeHook_UpdateIndicGo
	B_CALL restoreTR
	res	saIndic,(iy + newIndicFlags)
	ret
ModeHook_UpdateIndic:
	call	ModeHook_GetIndicChar
	ret	z
	B_CALL saveTR
	set	saIndic,(iy + newIndicFlags)
ModeHook_UpdateIndicGo:
	ld	hl,(curRow)
	push	hl
	ld	hl,0F00h
	ld	(curRow),hl
	call	ModeHook_GetIndicChar
	B_CALL PutMap
	pop	hl
	ld	(curRow),hl
	ret

ModeHook_GetIndicChar:
	ld	a,0E1h
	ld	h,(iy + shiftFlags)
	bit	3,h
	ret	nz
	bit	4,h
	ret	z
	add	a,2
	bit	5,h
	ret	nz
	dec	a
	ret

ModeHook_Lookup:
	ld	a,(97A2h)	; cur sel
	ld	c,a
	add	a,a
	add	a,c
	ld	d,0
	ld	e,a
	ld	hl,modeTemp
	add	hl,de
	add	a,c
	add	a,a
	ld	e,a
	ld	ix,ModeHook_Table
	add	ix,de
	ret

;;;;;;;;;;;;;;;;

PutsApp:;PORTED
	rst	20h
	ld	hl,OP1
	B_CALL PutS
	ret

ClearCache:
	call LookupAppVar
	ex de,hl
	inc hl
	inc hl
	ld bc,CacheSize
	B_CALL MemClear
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
	ld bc,CacheSize
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
	ex de,hl
	ld bc,appVarGraphStylesLength+appVarInitEquationsLength
	B_CALL MemClear
Exists:
	ld a,b
	or a
	jr z,NotArchived
	B_CALL Arc_Unarc
NotArchived:
	ret

CreateEquations:
	ld e,0
CreateEquations_Loop:
	push de
	ld hl,equation
	rst rMOV9TOOP1
	ld a,tY1
	pop de
	push de
	add e
	ld (OP1+2),a
	ld hl,YEquationSize
	B_CALL CreateEqu
	inc de
	inc de
	ld hl,YEquation
	ld bc,YEquationSize-1
	ldir
	ex de,hl
	pop de
	push de
	ld a,t0
	add e
	ld (hl),a
	pop de
	inc e
	ld a,6;0..5
	cp e
	jr nz,CreateEquations_Loop
	ld hl,EquOffset
	ld c,tX1T
CreateEquations_Loop2:
	push bc
	push hl
	
	call LookupAppVar
	push de
	ld hl,equation
	rst rMOV9TOOP1
	pop de

	pop hl
	pop bc
	push bc
	push hl
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)

	ld a,c
	ld (OP1+2),a
	ex de,hl
	B_CALL CreateEqu
	inc de
	inc de

	push de
	call LookupAppVar ;stack=[equ_data,appvar_offset,current_equation,...]
	ex de,hl
	pop de

	pop bc
	push bc
	add hl,bc
	ld c,(hl)
	ld (hl),0
	inc hl
	ld b,(hl)
	ld (hl),0
	inc hl
	push hl
	push bc
	ld a,b
	or c
	jr z,$f
	ldir
$$:

	pop de
	pop hl
	B_CALL DelMem

	pop hl
	inc hl
	inc hl ;new offset

	pop bc
	inc c
	ld a,tY6T+1
	cp c
	jr nz,CreateEquations_Loop2
	call LookupAppVar
	ex de,hl
	ld de,AppvarInitSize
	ld (hl),e
	inc hl
	ld (hl),d
	ret
YEquation:
	db t2ByteTok,tReal,t3,t4,tComma
YEquationEnd:
YEquationSize	equ YEquationEnd-YEquation+1

SetupCalc:
	ld a,42h
	ld (YEquHookState),a
	call create_appvar
	call load_status_address
	;Save bits
	set SimultBit,(hl)
	bit grfSimul,(IY+grfDBFlags)
	jr nz,$f
	res SimultBit,(hl)
$$:
	set ExprBit,(hl)
	bit 0,(IY+24)
	jr nz,$f
	res ExprBit,(hl)
$$:
	;save Graphing styles
	call load_style_address
	ex de,hl
	ld hl,GX1
	ld bc,6
	ldir
	call SaveEquations
	AppOnErr SetupCalc_Error
	call CreateEquations
	AppOffErr
	call EnableEquations
	B_CALL SetTblGraphDraw ;dirty graph and table
	ret

SetupCalc_Error:;Memory is full, error during CreateEqu
	push af
	call RestoreCalc_NoSave
	pop af
	res 7,a
	jp DisplayError
	

RestoreCalc:
	call SaveParamEquations
	call SaveEnabledEquations
RestoreCalc_NoSave:
	xor a
	ld (IY+35h),a;FIX:restore hooks instead of deleting them
	ld (IY+36h),a;disable hooks
	ld (YEquHookState),a;not in diffequ mode anymore
	call DeleteEquations
	call RestoreEquations
	ld b,1<<grfFuncM
	call set_graph_mode
	call load_status_address
	bit SimultBit,(hl);Simultaneous mode is enabled
	jr nz,$f
	res grfSimul,(IY+grfDBFlags)
$$:
	bit ExprBit,(hl);ExprOff bit is enabled
	jr nz,$f
	res 0,(IY+24)
$$:
	;restore Graphing style 9780..9786 GX1..GX1+5
	call load_style_address
	ld de,GX1
	ld bc,6
	ldir
	ret

SaveEnabledEquations:
	ld hl,equation
	rst rMOV9TOOP1
	ld b,0
	ld c,tX6T
SaveEnabledEquations_Loop:
	rlc b
	ld a,c
	ld (OP1+2),a
	push bc
	rst rFINDSYM
	pop bc
	bit 5,(hl)
	jr z,$f
	set 0,b
$$:
	dec c
	dec c
	ld a,tX1T-2
	cp c
	jr nz,SaveEnabledEquations_Loop
	push bc
	call LookupAppVar
	ld hl,EnabledOffset
	add hl,de
	pop bc
	ld (hl),b
	ret

EnableEquations:
	call LookupAppVar
	ld hl,EnabledOffset
	add hl,de
	ld b,(hl)
	ld c,tX1T
	push bc
	ld hl,equation
	rst rMOV9TOOP1
	pop bc
EnableEquations_Loop:
	push bc
	ld a,c
	ld (OP1+2),a
	rst rFINDSYM
	pop bc
	set 5,(hl)
	bit 0,b
	jr nz,$f
	res 5,(hl)
$$:
	bit 0,c
	jr z,$f
	rrc b
$$:
	inc c
	ld a,tY6T+1
	cp c
	jr nz,EnableEquations_Loop
	ret

SaveParamEquations:
	call load_equ_address
	push hl
	ld hl,equation
	rst rMOV9TOOP1
	pop de
	ld c,tX1T
SaveParamEquations_Loop:
	push bc
	push de
	ld a,c
	ld (OP1+2),a
	rst rFINDSYM
	ex de,hl

	ld e,(hl)
	inc hl
	ld d,(hl)
	ex de,hl
	B_CALL EnoughMem ;HL=amount of ram
	jr c,SaveParamEquations_Exit
	ex de,hl
	pop de
	push de
	push hl
	B_CALL InsertMem ;HL=bytes to insert DE=location

	rst rFINDSYM
	ex de,hl
	pop bc
	pop de
	inc bc
	inc bc;size bytes
	ldir
	pop bc
	inc c
	ld a,tY6T+1
	cp c
	jr nz,SaveParamEquations_Loop
	ret

SaveParamEquations_Exit:
	;Not enough memory,just stop saving equations
	pop bc
	pop bc
	ret

DeleteEquations:
	ld hl,equation
	rst rMOV9TOOP1
	ld a,tY1
	ld (OP1+2),a
DeleteEquations_Loop:
	rst rFINDSYM
	jr c,$f
	B_CALL DelVar
$$:
	ld hl,OP1+2
	inc (hl)
	ld a,(hl)
	cp tY6+1
	jr nz,DeleteEquations_Loop
	ld a,tX1T
	ld (OP1+2),a
DeleteEquations_Loop2:
	rst rFINDSYM
	jr c,$f
	B_CALL DelVar
$$:
	ld hl,OP1+2
	inc (hl)
	ld a,(hl)
	cp tY6T+1
	jr nz,DeleteEquations_Loop2
	ret

SaveEquations:
	ld c,tY1
SaveEquations_Loop1:
	ld a,c
	add 10;Y1->Y0+1
	ld b,a
	call RenameEquation
	inc c
	ld a,tY6+1
	cp c
	jr nz,SaveEquations_Loop1
	ld c,tX1T
SaveEquations_Loop2:
	ld a,c
	add twn-tX1T+1;X1T->w(n)+1
	ld b,a
	call RenameEquation
	inc c
	ld a,tY6T+1
	cp c
	jr nz,SaveEquations_Loop2
	ret

RestoreEquations:
	ld c,tY1+10
RestoreEquations_Loop1:
	ld a,c
	sub 10;Y1<-Y0+1
	ld b,a
	call RenameEquation
	inc c
	ld a,tY6+10+1
	cp c
	jr nz,RestoreEquations_Loop1
	ld c,twn+1 
RestoreEquations_Loop2:
	ld a,c
	sub twn-tX1T+1;X1T<-w(n)+1
	ld b,a
	call RenameEquation
	inc c
	ld a,tY6T-tX1T+twn+2
	cp c
	jr nz,RestoreEquations_Loop2
	ret

RenameEquation:;C=original name B=new name
	push bc
	ld a,c
	ld hl,equation
	rst rMOV9TOOP1
	ld (OP1+2),a
	rst rFINDSYM
	ld de,-7
	add hl,de
	pop bc
	ld (hl),b
	ret

SetCalcSpeed:
	push	af
	push	bc
	B_CALL GetBaseVer
	cp	2
	jr	nc,SetCalcSpeed_above112
	cp	1
	jr	nz,SetCalcSpeed_below112
	ld	a,b
	cp	13
	jr	c,SetCalcSpeed_below112
SetCalcSpeed_above112:
	ld	a,0FFh
	B_CALL	SetExSpeed
SetCalcSpeed_below112:
	pop	bc
	pop	af
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
	ld a,1
	call load_equation2
	B_CALL ParseInp
	B_CALL CkOP1Real
	ret z
	jr parser_hook_argument_error

load_equation:
	xor a
load_equation2:
	add e
	add e;a+=2*e
	ld hl,equation
	rst rMOV9TOOP1
	ld hl,OP1+2
	add (hl)
	ld (hl),a
	ret
equation:
	db EquObj, tVarEqu, tX1T,0

CountEquations:;returns the nr of active equations in d
	xor a
	ld b,6
CountEquations_Loop:
	rrc d
	adc a,0
	djnz CountEquations_Loop
	ret

LoadOP2:;hl=M*256+E
	push hl
	B_CALL ZeroOP2
	pop hl
	ld (OP2+1),hl
	ret

load_Y_equation:
	ld a,tY1
	add e
	ld hl,equation
	rst rMOV9TOOP1
	ld (OP1+2),a
	ret
	
load_fldres:
	call LookupAppVar
	ld hl,FldresOffset
	add hl,de
	rst rMOV9TOOP1
	ret

load_simple_cache_address:
	call LookupAppVar
	ex de,hl
	inc hl
	inc hl
	ret

load_equ_address:
	call LookupAppVar
	ld hl,EquOffset
	add hl,de
	ret

load_RKEval_address:
	call LookupAppVar
	ld hl,RKEvalOffset
	add hl,de
	ret
	
load_style_address:
	call LookupAppVar
	ld hl,StyleOffset
	add hl,de
	ret

load_status_address:
	call LookupAppVar
	ld hl,StatusOffset
	add hl,de
	ret

load_diftol_address:
	call LookupAppVar
	ld hl,DiftolOffset
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
	include "slopefield.asm"

cacheSwitchBit					equ 0
cache1ValidBit					equ 1
cache2ValidBit					equ 2

cacheSwitchMask				equ 1<<cacheSwitchBit
cache1ValidMask				equ 1<<cache1ValidBit
cache2ValidMask				equ 1<<cache2ValidBit

cacheSimpleValidBit	equ 7;in simple cache statusbits
cacheSimpleValidMask	equ 1<<cacheSimpleValidBit

appvarInitData:
	db 0;statusbits
	db 0;equations to be evaluated in RK mode
	db 0;equations that are enabled
appvarInitDataDiftol:
	db 00h,7Dh,10h,00h,00h,00h,00h,00h,00h ;.001 diftol
appvarInitDataFldRes
	db 00h,80h,80h,00h,00h,00h,00h,00h,00h ;8 fldres
appvarInitDataEnd:
appvarInitDataLength equ appvarInitDataEnd-appvarInitData

appVarGraphStylesLength		equ 6
appVarInitEquationsLength	equ 6*2*2

simpleCacheSize equ 1+7*9 ;X and Y1..Y6
;SIMPLE CACHE:
;0			statusbits
;1..9		X
;10..63	Y6..Y1 (reverse order!)


CacheSize equ simpleCacheSize+rungeCacheSize

;APPVAR
;64		Simple Cache
;252		Cache (size of runge cache)
;1			Statusbits
;1			Equations to be evaluated in RK mode
;1			Equations that are enabled
;9			Diftol
;6			Graph style of X*T
;?			equations

AppvarInitSize 	equ CacheSize+appvarInitDataLength+appVarGraphStylesLength+appVarInitEquationsLength ;runge cache is larger than euler cache

StatusOffset	equ 2+CacheSize
RKEvalOffset	equ 2+CacheSize+1
EnabledOffset	equ 2+CacheSize+2
DiftolOffset	equ 2+CacheSize+3
FldresOffset	equ 2+CacheSize+3+9
StyleOffset		equ 2+CacheSize+3+9+9
EquOffset		equ 2+CacheSize+3+9+9+appVarGraphStylesLength

;STATUSBITS
EulerBit		equ 0	;0=euler, 1=RK
SlopeFldBit	equ 1 ;0=no slope field 1=draw slope field
RealEquBit	equ 2 ;0=an equation returns a non-real value (usually a list) 
						;	1= all equations return a real value
SimultBit	equ 6 ;Copy of the simultaneous/sequential bit
ExprBit		equ 7 ;Copy of the ExprOn/Off bit

end_of_app:
app_size equ end_of_app-4080h

;FIX: Get simple cache code in here
;FIX: Modify app switch hook to set realequbit when equations contain lists

;--------------------------------MAYBE LATER--------------------------------
;CLEANUP: Use variable used during graphing to support smart graph
;FIX:implement our own version of tmin,tstep and tmax with more sensible default values for diffequs
;FIX:implement expron/exproff? (problems with either putting it on top of equ nr in upper right corner or disappearing)
;FIX:implement Initial Condition chooser on graph
;FIX:disallow y1(..) calls inside ODE's (Only work inside table, don't like modifying equations)
;FIX:disallow lists inside ODE's (can't get list graphing to work correctly with rungekutta, euler works mostly when parsflags IY+6&7 are backed up.)
;FIX:disallow recalling Y1..Y6 in diffequ mode (difficult,not worth it)


;Y= Screen
;* Use only one line to display result, followed by ...
;* When switching away make errors behave like the window screen
