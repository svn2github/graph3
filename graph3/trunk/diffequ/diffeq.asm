#define  APP_NAME "Diffequ "
#define  APP_PAGES 1
#define  FAST_SE
#include "app.inc"

_Mon				equ 401Eh
_SendKPress			equ 4024h
_JForceCmd			equ 402Ah
_newContext0		equ 4033h
_cxPutAway			equ 4036h
_IsA2ByteTok		equ 42A3h
_WPutSEOL			equ 4522h
_SetNumWindow		equ 452Bh
_saveTR			equ 4588h
_restoreTR			equ 458Bh
_BufClear			equ 4936h
_CursorToStart		equ 4939h ;4945h
_CursorDown			equ 4948h
_SetEmptyEditPtr		equ 4969h
_OP1ToEdit			equ 49A5h
_OpenEditEqu		equ 49C3h
_UpdateYEqu			equ 49CCh
_POPCX			equ 49E1h
_SetCursorHook		equ 4F60h
_EnableRawKeyHook 	equ 4F66h
_EnableWindowHook		equ 4FB1h
_EnableGraphHook		equ 4FB7h
_EnableYEquHook		equ 4FCFh
_EnableReGraphHook	equ 4FEAh
_EnableParserHook 	equ 5026h
_EnableAppSwitchHook	equ 502Ch
_EnableMenuHook		equ 5083h

GX1				equ 9780h
YEquHookState		equ 9B98h+3

StartApp:
	call	CreateAppvar

	ld	a,(YEquHookState)
	cp	$42
	jr	z,Restore

	call	SetupCalc

	in	a,(6)
	ld	hl,ParserHook
	bcall	_EnableParserHook

	ld	hl,AppChangeHook
	bcall	_EnableAppSwitchHook

	ld	hl,RegraphHook
	bcall	_EnableReGraphHook

	ld	hl,KeyHook
	bcall	_EnableRawKeyHook

	ld	hl,MenuHook
	bcall	_EnableMenuHook

	ld	hl,WindowHook
	bcall	_EnableWindowHook

	ld	hl,GraphHook
	bcall	_EnableGraphHook

	ld	hl,YeditHook
	bcall	_EnableYEquHook

ExitApp:
	bjump	_JForceCmdNoChar

Restore:
	call	RestoreCalc
	jr	ExitApp

function(ParserHook):
	.db	$83
	call	SetCalcSpeed
	or	a
	jr	z,@Return		;preparser
	push	hl
	ld	hl,$8A8A
	or	a
	sbc	hl,bc
	pop	hl
	jr	nz,@Return
	ld	a,l
	dec	a
	dec	a 
	or	h			;only Z for HL=2
	jr	z,@Real
@Return:
	xor	a
	ret

@ArgumentError:
	bjump	_ErrArgument

@Real:
	bcall	_CkPosInt
	jr	nz,@ArgumentError	;second arg not posint
	bcall	_ConvOP1
	xor	a
	or	d
	jr	nz,@ArgumentError	;second arg too big (>255)
	ld	a,e
	cp	6
	jr	nc,@ArgumentError	;second arg too big (>5)
	push	de
	bcall	_PopRealO1
	bcall	_CkPosInt
	pop	de
	jr	nz,@ArgumentError	;first arg not posint
	push	de
	bcall	_ConvOP1
	ld	e,a
	sub	34			;not yet used by omnicalc
	or	d
	pop	de
	jr	nz,@ArgumentError
	;all checks passed, E contains equation number
	;fallthrough
function(ExecuteDiffequAlgorithm):
	push	de
	call	LoadStatusAddress
	pop	de
	ld	d,(hl)
	push	de
	call	RclT
	rst	18h ;rPUSHREALO1	;save X
	pop	ix
	push	ix
	AppOnErr(@Error)
	push	ix
	pop	de
	bit	EulerBit,d
	jr	nz,@RK
	call	Euler
@Finish:
	AppOffErr
	pop	ix 
	bcall	_PopRealO1
	call	StoT
	bcall	_OP2ToOP1
	or	$FF
	ret
@RK:
	call	Runge
	jr	@Finish

@Error:
	pop	de
	push	af
	bit	EulerBit,d
	call	nz,ClearCache;Simple erase cache in case of RK
	bcall	_PopRealO1
	call	StoT
	pop	af
	ld	b,a
	and	$7F
	cp	6		;ON:BREAK
	jr	z,DisplayError
DisplayOriginalError:
	ld	a,b
DisplayError:
	bjump	_JError

function(RegraphHook):
	.db	$83
	call	SetCalcSpeed
	cp	$09
	jr	nz,@Not9
	call	SlopeField
	jr	@Return
@Not9:
	cp	$08
	jr	nz,@Return

	AppOnErr(@Error)
	ld	a,(OP1+2)
	;X*t is even
	;Y*t is odd
	srl	a
	jr	nc,@X
	;process Y
	and	$07
	ld	e,a
	call	ExecuteDiffequAlgorithm
	AppOffErr
	jr	@ReturnNZ
@X:					;just return T (already in OP1)
	set	numOP1,(iy+ParsFlag2)
	call	RclT
	AppOffErr
@ReturnNZ:
	or	$FF
	ret
@Return:
	xor	a
	ret
@Error:
	res	write_on_graph,(iy+sGrFlags)
	res	numOP1,(iy+ParsFlag2)
	ld	b,a
	and	$7F
	cp	8			;only ignore the first errors
	jr	nc,DisplayOriginalError
	ld	hl,equation
	rst	20h ;rMOV9TOOP1
	or	$FF
	ret

function(SetGraphMode):
	ld	a,(iy+grfModeFlags)
	and	$0F			;reset all graph modes
	or	b			;set graph mode to parametric
	ld	(iy+grfModeFlags),a
	set	grfSimul,(iy+grfDBFlags)
	set	0,(iy+24)		;set ExprOff because we don't want to see the X*T and Y*T expressions
	ret

RemoveGotoErrorHandler:
	and	$7F
	jp	DisplayError

function(AppChangeHook):
	.db	$83
	call	SetCalcSpeed
	push	af
	push	hl
	ld	a,b
	cp	kYequ
	call	z,@ExitYEqu
	ld	b,1<<grfParamM
	call	SetGraphMode
	pop	hl
	pop	af
	ld	b,1<<grfFuncM
	cp	cxTableEditor
	jr	z,SetGraphMode	;let SetGraphMode return
	cp	kFormat
	jp	z,ModeHook
	ret

function(AppChangeHook@ExitYEqu):
	AppOnErr(RemoveGotoErrorHandler)
	ld	e,0
@Loop:
	push	de
	;Dis/Enable Y* function	re
	call	LoadEquation
	rst	10h ;rFINDSYM
	ld	a,(hl)
	and	1<<5			;the bit that dis/enables an equation
	pop	de
	push	de
	push	af
	call	LoadYEquation
	rst	10h ;rFINDSYM
	ld	a,(hl)
	res	5,a
	pop	bc
	or	b			;dis/enable equation
	ld	(hl),a
	pop	de
	push	de
	;Update initial value
	ld	a,1
	call	LoadEquation@2
	AppOnErr(@Skip)
	bcall	_ParseInp
	AppOffErr
	bcall	_CkOP1Real
	jr	nz,@Skip		;don't save
	bcall	_OP1ToOP6
	pop	de
	push	de 
	ld	a,1
	call	LoadEquation@2
	rst	10h ;rFINDSYM
	bcall	_SetEmptyEditPtr
	bcall	_OP6ToOP1
	bcall	_OP1ToEdit
	bcall	_CloseEditEqu
@Skip:
	pop	de
	inc	e
	ld	a,6			;0..5
	cp	e
	jr	nz,@Loop
	bcall	_CleanAll
	;figure out which equations need to be evaluated in RK mode
	ld	de,0
@Loop2:
	push	de
	call	LoadEquation
	rst	10h ;rFINDSYM
	bit	5,(hl)
	ex	de,hl
	pop	de
	call	nz,@CheckEqu
	inc	e
	ld	a,6			;0..5
	cp	e
	jr	nz,@Loop2
	;store which equations need to be evaluated in RK mode
	push	de
	call	LookupAppVar 
	push	de
	pop	bc
	ld	hl,RKEvalOffset
	add	hl,de
	pop	de
	ld	(hl),d
	;Erase cache if smart graph can't be used
	bit	smartGraph_inv,(iy+smartFlags)
	call	nz,ClearCache
	AppOffErr
	ret

@CheckEqu:
	push	de
	ld	a,1
	inc	e
@CheckEquLoop:
	dec	e
	jr	z,@CheckEquSkip
	sla	a
	jr	@CheckEquLoop
@CheckEquSkip:
	pop	de
	ld	b,a
	and	d
	ret	nz			;already checked
	ld	a,d
	or	b
	ld	d,a
	ld	c,(hl)
	inc	hl
	ld	b,(hl)
	inc	hl
@CheckEquLoop2:
	ld	a,b
	or	c
	ret	z
	ld	a,(hl)
	cp	tVarEqu
	jr	nz,@CheckEquNotEqu
	inc	hl
	ld	a,(hl)
	sub	tY1
	jr	c,@CheckEquNotY
	cp	6
	jr	nc,@CheckEquNotY
	;Found Y1..Y6 token
	push	bc
	push	hl
	ld	e,a			;D hasn't changed
	push	de
	call	LoadEquation
	rst	10h ;rFINDSYM
	ex	de,hl
	pop	de
	call	@CheckEqu
	pop	hl
	pop	bc	
@CheckEquNotY:
	dec	hl
	ld	a,(hl)
@CheckEquNotEqu:
	bcall	_IsA2ByteTok
	inc	hl
	dec	bc
	jr	nz,@ZDSf
	inc	hl
	dec	bc
@ZDSf:
	jr	@CheckEquLoop2

CurTableRow = $91DC
CurTableCol = $91DD

function(KeyHook):
	.db	$83
	call	SetCalcSpeed
	push	af
	ld	a,(cxCurApp)
	cp	cxTableEditor
	jr	nz,@NoTable
	ld	hl,(CurTableRow)
	dec	l
	jr	nz,@Skip
	xor	a
	cp	h
	jr	z,@Skip
	pop	af
	cp	kUp
	jr	z,@IgnoreKey
	push	af
@NoTable:
@Skip:
	pop	af
	or	a
	ret
@IgnoreKey:
	xor	a
	ret

function(GraphHook):
	.db	$83
	call	SetCalcSpeed
@Not2:
	cp	$06
	jr	nz,@Not6
	ld	a,b
	cp	kStd
	jr	nz,@Allow
	push	bc
	call	LoadDiftolAddress
	ex	de,hl
	ld	hl,appvarInitDataDiftol
	call	Mov9
	call	Mov9			;copy FldRes
	pop	bc
@Not6:
@Allow:
	xor	a
	ret

function(YeditHook):
	.db	$83
	call	SetCalcSpeed
	sub	$05
	jr	nz,@Not5
	ld	a,(EQS+7)
	and	$0F
	sra	a
	ld	b,1
	inc	a
@ZDSb:
	dec	a
	jr	z,@ZDSf
	sla	b
	jr	@ZDSb
@ZDSf:
	push	bc
	call	LoadRKEvalAddress
	ld	a,(hl)
	pop	bc
	and	b
	jp	z,@Allow
	bcall	_SetTblGraphDraw
	jp	@Allow
@Not5:
	dec	a
	jr	nz,@Not6
	ld	a,(EQS+7)
	bit	0,a
	jp	z,@Allow		;only do something when Y*T equations are selected
	bit	0,(iy+19)
	jp	nz,@Allow		;Don't do anything when = is selected
	;The graphing style icon is never selected here because we immediately abort when X*T is edited
	ld	a,b
	cp	kEnter
	jr	z,@Evaluate
	cp	kLeft
	jr	z,@CheckStart
	cp	kUp
	jr	z,@CheckStart
	cp	kDown
	jr	z,@CheckDown
	jr	@Allow
@CheckDown:
	bcall	_CursorDown
	ld	b,kEnter
	jr	z,@Evaluate
	or	$FF
	ret
@CheckStart:
	ld	hl,(editTop)
	ld	de,(editCursor)
	or	a
	sbc	hl,de
	jr	nz,@Allow
@Evaluate:
	push	bc
	bcall	_CursorToStart
	bcall	_CloseEditEqu

	ld	a,(EQS+7)
	and	$0F
	ld	e,a
	sra	e
	and	1
	call	LoadEquation@2
	bcall	_ParseInp
	bcall	_OP1ToOP6

	bcall	_OpenEditEqu
	bcall	_BufClear
	bcall	_OP6ToOP1
	bcall	_OP1ToEdit
	bcall	_CloseEditEqu ;Just close and open it to make sure that all pointers are correct

	bcall	_OpenEditEqu
	bcall	_UpdateYEqu
	pop	bc
	jr	@Allow
@Not6:
	dec	a
	dec	a
	jr	nz,@Not8
	ld	a,(curCol)
	sub	3
	ld	(curCol),a
	ld	a,'Y'
	bcall	_PutC
	ld	a,(EQS+7)
	ld	b,a
	srl	a
	and	%00000111
	add	a,Lsub1
	bit	0,b
	jr	nz,@YiText
	bcall	_PutC
	ld	a,'\''
	bcall	_PutC
	jr	@Allow
@YiText:
	push	af
	ld	a,Li
	bcall	_PutC
	pop	af
	bcall	_PutC
	jr	@Allow
@Not8:
@Allow:
	xor	a
	ret

function(MenuHook):		;PORTED
	.db	$83
	call	SetCalcSpeed
	or	a
	jr	nz,@Not0
	ld	a,(MenuCurrent)
	cp	$0B			;Vars-menu
	jr	z,@ReplaceVarsMenu
	cp	$39			;Calc Menu (parametric mode)
	jr	z,@ReplaceCalcMenu
	cp	$11			;Calc Menu (function mode)
	jr	z,@ReplaceCalcMenu
	cp	$1B			;Function-Menu
	jr	nz,@Allow
@ReplaceFunctionMenu:
	ld	hl,YEquMenu
	jr	@ReplaceMenu
@ReplaceVarsMenu:
	ld	hl,VarsMenu
	jr	@ReplaceMenu
@ReplaceCalcMenu:
	ld	hl,CalcMenu
@ReplaceMenu:
	ld	de,tempSwapArea
	ld	bc,25			;longest menu implemented
	push	de
	ldir
	pop	hl
	or	$FF
	ret
@Not0:
@Allow:
	xor	a
	ret

CalcMenu:				;PORTED
	.db 1		;how many headers?
	.db 1 	;how many choices under 1st header?
	.db $24	;string table entry for "CALCULATE"
	.db 0,kEval ;these are 2-byte keypresses of each entry

VarsMenu:				;25 bytes long
	.db 2		;how many headers?
	.db 7 	;how many choices under 1st header?
	.db 3  	;how many choices under 2nd header?
	.db $5A	;string table entry for "VARS"
	.db $5D	;string table entry for "Y-VARS"
	.db $82,$3E	;these are 2-byte keypresses of each entry
	.db $82,$3F
	.db $82,$40
	.db $82,$41
	.db $82,$43
	.db $82,$44
	.db $82,$42
	.db $82,$45	;second header
	.db $82,$47
	.db $82,$48

YEquMenu:
	.db 1		;how many headers?
	.db 6 	;how many choices under 1st header?
	.db $30	;string table entry for "FUNCTION"
	.db kExtendEcho2,kY1 ;these are 2-byte keypresses of each entry
	.db kExtendEcho2,kY2
	.db kExtendEcho2,kY3
	.db kExtendEcho2,kY4
	.db kExtendEcho2,kY5
	.db kExtendEcho2,kY6

function(WindowHook):	;PORTED
	.db	$83
	call	SetCalcSpeed
	push	af
	ld	a,(cxCurApp)
	cp	kWindow
	jr	z,@Possible
	pop	af
	xor	a
	ret
@Possible:
	pop	af
;	call	CheckGraphMode
;	ret	z
	or	a
	jr	nz,@Not0
	or	$FF
	ld	a,(ix)
	ret
@Not0:
	dec	a
	jr	nz,@Not1
	or	$FF
	ld	a,(ix+3)
	ret
@Not1:
	dec	a
	jr	nz,@Not2
	or	$FF
	ld	a,(ix-3)
	ret
@Not2:
	dec	a
	jr	nz,@Not3
	ld	a,(ix)
	cp	@Identifier
	jr	nc,@3NoToken
	ld	d,tVarSys
	ld	e,a
	bcall	_PutTokString
	jr	@3Token
@3NoToken:
	ld	l,(ix+1)
	ld	h,(ix+2)
	call	PutsApp
@3Token:
	ld	a,'='
	bcall	_PutC
	bcall	_SetNumWindow
	jp	@DispNum
@Not3:
	dec	a
	ret	z			;A=$04 return Z
	dec	a
	jr	nz,@Not5
	ld	a,(hl)
	call	@RclValue
	ld	hl,OP1
	or	$FF
	ret
@Not5:
	dec	a
	jr	nz,@Not6
	set	graphDraw,(iy + graphFlags)
	ld	a,(hl)
	cp	@Identifier
	jr	nc,@6NoToken
	push	af
	bcall	_StoSysTok
	pop	af
	cp	TMINt
	jr	z,@EraseCache
	cp	TSTEPt
	jr	z,@EraseCache
	jr	@6Token
@6NoToken:
	push	af
	rst	18h ;rPUSHREALO1
	pop	af
	push	af
	cp	@FldResIdentifier
	jr	nz,@NoError
	bcall	_CkPosInt
	jr	nz,@Error
	bcall	_ConvOP1
	cp	(96/2)+1
	jr	nc,@Error
	ld	a,d
	or	a
	jr	z,@NoError
@Error:
	bjump	_ErrDomain 
@NoError:
	pop	af
	call	@LoadValueAddress
	ex	de,hl
	bcall	_PopReal
@EraseCache:
	rst	18h ;rPUSHREALO1
	call	ClearCache
	bcall	_PopRealO1
@6Token:
	ld	hl,OP1
	or	$FF
	ret
@Not6:
	dec	a
	jr	nz,@Not7
	call	LoadStatusAddress
	ld	hl,@TablePointers
	bit	EulerBit,a
	jr	z,@ZDSf1
	ld	de,4
	add	hl,de
@ZDSf1:
	bit	SlopeFldBit,a
	jr	z,@ZDSf2
	inc	hl
	inc	hl
@ZDSf2:
	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
@Disallow:
	or	$FF
	ret
@Not7:
	dec	a
	ret	z
	dec	a
	jr	nz,@Not9
@DispNum:
	ld	a,(ix)
	call	@RclValue
	ld	a,$15
	bcall	_FormEReal
	ld	hl,OP3
	bcall	_WPutSEOL
	or	$FF
	ret
@Not9:
	dec	a
	jr	nz,@Not10
	ld	a,b
	cp	kLastEnt
	jr	z,@Disallow
@Not10:
@Allow:
	xor	a
	ret

@RclValue:
	cp	@Identifier
	jr	nc,@RclValueNoToken
	bcall	_RclSysTok
	ret
@RclValueNoToken:
	call	@LoadValueAddress
	rst	20h ;rMOV9TOOP1
	ret

@LoadValueAddress:
	push	af
	call	LookupAppVar
	ld	hl,FldresOffset+2
	add	hl,de
	pop	de
	ld	e,d
	ld	d,$FF
	add	hl,de
	ret

@TablePointers:
	.dw @TableEuler+3
	.dw @TableEulerField+3
	.dw @TableRK+3
	.dw @TableRKField+3

@TableEuler:
	.db $FF
	.dw 0
	.db TMINt
	.dw 0
	.db TMAXt
	.dw 0
	.db TSTEPt
	.dw 0
	.db XMINt
	.dw 0
	.db XMAXt
	.dw 0
	.db XSCLt
	.dw 0
	.db YMINt
	.dw 0
	.db YMAXt
	.dw 0
	.db YSCLt
	.dw 0
	.db $FF

@TableEulerField:
	.db $FF
	.dw 0
	.db TMINt
	.dw 0
	.db TMAXt
	.dw 0
	.db TSTEPt
	.dw 0
	.db XMINt
	.dw 0
	.db XMAXt
	.dw 0
	.db XSCLt
	.dw 0
	.db YMINt
	.dw 0
	.db YMAXt
	.dw 0
	.db YSCLt
	.dw 0
	.db -2
	.dw @FldResStr
	.db $FF

@TableRK:
	.db $FF
	.dw 0
	.db TMINt
	.dw 0
	.db TMAXt
	.dw 0
	.db TSTEPt
	.dw 0
	.db XMINt
	.dw 0
	.db XMAXt
	.dw 0
	.db XSCLt
	.dw 0
	.db YMINt
	.dw 0
	.db YMAXt
	.dw 0
	.db YSCLt
	.dw 0
	.db -2-9
	.dw @DifTolStr
	.db $FF

@TableRKField:
	.db $FF
	.dw 0
	.db TMINt
	.dw 0
	.db TMAXt
	.dw 0
	.db TSTEPt
	.dw 0
	.db XMINt
	.dw 0
	.db XMAXt
	.dw 0
	.db XSCLt
	.dw 0
	.db YMINt
	.dw 0
	.db YMAXt
	.dw 0
	.db YSCLt
	.dw 0
	.db -2-9
	.dw @DifTolStr
	.db -2
	.dw @FldResStr
	.db $FF

@DifTolStr:
	.db "Diftol",0
@FldResStr:
	.db "Fldres",0
@DifTolIdentifier	= (-2-9) & 255
@FldResIdentifier	= (-2) & 255
@Identifier		= (-2-9) & 255 ;first identifier that's not a system variable token

;;;;;;;;;;;;;;;;Start of ModeHook PORTED from graph3

modeTemp		= appBackUpScreen + 600
					; 742-749 contains data from saveStuff.. i was stupid
saveStuff		= appBackUpScreen + 750
_Flags		= appBackUpScreen +  30

function(ModeHook):
	ld	hl,$97A2		; current selection
	ld	(hl),0

	call	LoadStatusAddress	;Destroys OP1
	ld	a,(hl)
	ld	(_Flags),a

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
	ld	hl,flags + $34
	ldi

	ld	sp,(onSP)
	ld	hl,@gmMonVectors
	bcall	_AppInit

	set	saIndic,(iy + newIndicFlags)
	in	a,(6)
	ld	hl,CursorHook
	bcall	_SetCursorHook

	ld	a,cxextapps
	ld	(cxCurApp),a

	call	@gmcxRedisp@ForSure
	bjump _Mon

@BlinkItem:
	ld	b,(hl)
	ld	hl,curCol
	ld	a,(hl)
	or	a
	jr	z,@BlinkSkip
	dec	b
	inc	(hl)
@BlinkSkip:
	set	textInverse,(iy + textFlags)
@BlinkLoop:
	ld	a,' '
	bcall	_PutC
	djnz	@BlinkLoop
	res	textInverse,(iy + textFlags)
	ret

@DrawItemX:
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	ld	(curRow),de
@DrawItem:
	bit	1,(ix)
	jr	nz,@NotLeft
	xor	a
	ld	(curCol),a
@NotLeft:
	ld	a,(curCol)
	or	a
	jr	z,@DrawNoSpace
	push	ix
	ld	a,' '
	bcall	_PutC
	pop	ix
@DrawNoSpace:
	ld	l,(ix+3)
	ld	h,(ix+4)
	ld	a,(ix+5)
	and	(hl)
	cp	(ix+6)
	jr	nz,@NotSel
	set	textInverse,(iy + textFlags)
@NotSel:
	push	ix
	ld	h,(ix+1)
	ld	a,(ix+2)
	or	a
	jr	z,@Token
	ld	l,h
	ld	h,a
	jr	@PutsLoop
@Token:
	ld	l,tGFormat
	ld	(OP1),hl
	ld	hl,OP1
	bcall	_Get_Tok_Strng
	ld	hl,OP3
@PutsLoop:
	ld	a,(hl)
	inc	hl
	or	a
	jr	z,@PutsDone
	push	hl
	bcall	_PutMap
	pop	hl
	ld	a,(curCol)
	inc	a
	ld	(curCol),a
	jr	@PutsLoop
@PutsDone:
	pop	ix
	bit	0,(ix)
	jr	nz,@NotRight
	ld	a,(curRow)
	inc	a
	ld	(curRow),a
@NotRight:
	res	textInverse,(iy + textFlags)
	ret

@Table:
	.db	%00000101
	.dw tRectG,  flags + grfDBFlags	
	.db 1<<grfPolar,0	,0

	.db	%00000110
	.dw tPolarG,  flags + grfDBFlags	
	.db 1<<grfPolar,1<<grfPolar	,0

	.db	%00001101
	.dw tCoordOn,  flags + grfDBFlags			
	.db 1<<grfNoCoord,0	,0

	.db	%00001110
	.dw tCoordOff,  flags + grfDBFlags			
	.db 1<<grfNoCoord,1<<grfNoCoord	,0

	.db	%00001101
	.dw tGridOff,  flags + grfDBFlags			
	.db 1<<grfGrid,0	,1

	.db	%00001110
	.dw tGridOn,  flags + grfDBFlags			
	.db 1<<grfGrid,1<<grfGrid	,1

	.db	%00001101
	.dw tAxisOn,  flags + grfDBFlags			
	.db 1<<grfNoAxis,0	,1

	.db	%00001110
	.dw tAxisOff,  flags + grfDBFlags			
	.db 1<<grfNoAxis,1<<grfNoAxis	,1

	.db	%00001101
	.dw tLblOff,  flags + grfDBFlags			
	.db 1<<grfLabel,0	,0

	.db	%00001110
	.dw tLblOn,  flags + grfDBFlags			
	.db 1<<grfLabel,1<<grfLabel	,0

	.db	%00001101
	.dw @Strings_Euler,  _Flags			
	.db 1<<EulerBit,0	,2

	.db	%00001110
	.dw @Strings_RungeKutta,  _Flags			
	.db 1<<EulerBit,1<<EulerBit	,2

	.db	%00001001
	.dw @Strings_NoField,  _Flags			
	.db 1<<SlopeFldBit,0	,1

	.db	%00001010
	.dw @Strings_Field,  _Flags			
	.db 1<<SlopeFldBit,1<<SlopeFldBit	,1

@Strings_Euler:
	.db "Euler",0
@Strings_RungeKutta:
	.db "RungeKutta",0

@Strings_NoField:
	.db "FieldOff",0
@Strings_Field:
	.db "FieldOn",0

@gmMonVectors:
	.dw @gmcxMain,@gmcxDummyRet,@gmcxPutAway
	.dw @gmcxRedisp,@gmcxDummyRet,@gmcxDummyRet
	.db 0

@Quit:
	bcall	_POPCX
@Quit@NoPop:
	ld	a,cxFormat
	ld	(cxCurApp),a

	call	LoadStatusAddress	;Destroys OP1
	ld	a,(_Flags)
	ld	(hl),a

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
	ld	de,flags + $34
	ldi
	res	saIndic,(iy + newIndicFlags)
@gmcxDummyRet:
	ret

@gmcxPutAway:
	call	@Quit
	bit	monAbandon,(iy + monFlags)
	jr	nz,@Off
	ld	a,iall
	out	(intrptEnPort),a
	bcall	_LCD_DRIVERON
	set	onRunning,(iy + onFlags)
	ei
@Off:
	bjump _cxPutAway

@gmcxRedisp:
	ld	a,(kbdKey)
	cp	kClear
	ret	nz
@gmcxRedisp@ForSure:
	bcall	_ClrScrnFull
	xor	a
	ld	(curRow),a

	ld	ix,@Table
	ld	hl,modeTemp
@BigLoop:
	push	hl
	ld	hl,(curRow)
	push	hl
	call	@DrawItem
	pop	de
	pop	hl
	bit	1,(ix)
	jr	nz,@NotLeftX
	ld	d,0
@NotLeftX:
	ld	(hl),e
	inc	hl
	ld	(hl),d
	inc	hl
	ld	a,(curCol)
	sub	d
	ld	(hl),a
	inc	hl
	ld	a,(ix)
	and	%00000101
	ld	de,8
	add	ix,de
	jr	nz,@BigLoop
	ret

@gmcxMain:
	cp    kExtApps
	jr    nz,@NotApps
	ld	a,kQuit
	;call	@Quit
	;ld	hl,progCurrent
	;ld	de,progToEdit
	;ld	bc,8
	;ldir
	;bjump _$4C51       ;execute app
@NotApps:

	cp	kQuit
	jr	c,@NotExternal
	cp    kTrace
	jr	z,@GraphKey
	jr    nc,@NotExternal
	cp	kGraph
	jr	z,@GraphKey
	ld	hl,$97A2		; current selection
	ld	(hl),0
	cp	kFormat
	jp	z,@gmcxRedisp@ForSure
	push	af
	push	bc
	call  @Quit
	pop	bc
	pop	af
	ld    sp,(onSP)
	res   6,(iy + curFlags)
	bcall	_newContext0
	xor   a
	bjump _SendKPress
@GraphKey:
	bjump _JForceGraphKey
@NotExternal:

	cp	kLeft
	jr	nz,@NotkLeft
	call	@Lookup
	bit	1,(ix)
	ret	z
	call	@DrawItemX
	ld	hl,$97A2
	dec	(hl)
@Disallow:
	xor	a
	ret
@NotkLeft:

	cp	kRight
	jr	nz,@NotkRight
	call	@Lookup
	bit	0,(ix)
	ret	z
	call	@DrawItemX
	ld	hl,$97A2
	inc	(hl)
	xor	a
	ret
@NotkRight:

	cp	kUp
	jr	nz,@NotkUp
	call	@Lookup
	bit	3,(ix)
	ret	z
	call	@DrawItemX
	ld	hl,$97A2
	ld	de,-8
@FindUp1:
	bit	1,(ix)
	jr	z,@FindUp2
	dec	(hl)
	add	ix,de
	jr	@FindUp1
@FindUp2:
	dec	(hl)
	add	ix,de
	bit	1,(ix)
	jr	nz,@FindUp2
	ret
@NotkUp:

	cp	kDown
	jr	nz,@NotkDown
	call	@Lookup
	bit	2,(ix)
	ret	z
	call	@DrawItemX
	ld	hl,$97A2
@FindDown1M1:
	ld	de,8
@FindDown1:
	inc	(hl)
	add	ix,de
	bit	1,(ix)
	jr	nz,@FindDown1
	ret
@NotkDown:

	cp	kClear
	jr	nz,@NotClear
	call	@Quit
	jp	ExitApp ;bjump _JForceCmdNoChar
@NotClear:

	cp	kEnter
	jp	nz,@NotkEnter
	call	@Lookup
	ex	de,hl
	ld	l,(ix+3)
	ld	h,(ix+4)
	ld	a,(ix+5)
	and	(hl)
	cp	(ix+6)
	ret	z
	ld	a,(ix+7)
	or	a
	jr	z,@DontReset
	set	graphDraw,(iy+graphFlags)
	dec	a
	jr	z,@DontReset
	;invalidate table cache
	set	reTable,(iy+tblFlags)
	;erase cache when needed
	push	hl
	push	de
	push	af
	call	ClearCache
	pop	af
	pop	de
	pop	hl
@DontReset:
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
@FindUp3:
	bit	1,(ix)
	jr	z,@RedrawRow
	add	ix,de
	jr	@FindUp3
@RedrawRow:
	call	@DrawItem
	bit	0,(ix)
	ld	de,8
	add	ix,de
	jr	nz,@RedrawRow
	ret
@NotkEnter:

	cp	kEOL + 1
	jr	nc,@BadKey
@RetAllow:
	xor	a
	ret

@BadKey:
	push	af
	call	@Quit
	pop	af
	bjump _JForceCmd

CursorHook:
	.db	$83
	call	SetCalcSpeed
	push	af
	call	@Lookup
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	inc	hl
	ld	(curRow),de
	pop	af
	cp	$24
	jr	c,@NZ
	call	@DrawItem
	jr	@Z
@NZ	call	@BlinkItem
@Z	call	@HandleIndic
	xor	a
	ret

@HandleIndic:
	bit	saIndic,(iy + newIndicFlags)
	jr	z,@UpdateIndic
	call	@GetIndicChar
	jr	nz,@UpdateIndicGo
	bcall	_restoreTR
	res	saIndic,(iy + newIndicFlags)
	ret
@UpdateIndic:
	call	@GetIndicChar
	ret	z
	bcall	_saveTR
	set	saIndic,(iy + newIndicFlags)
@UpdateIndicGo:
	ld	hl,(curRow)
	push	hl
	ld	hl,$0F00
	ld	(curRow),hl
	call	@GetIndicChar
	bcall	_PutMap
	pop	hl
	ld	(curRow),hl
	ret

@GetIndicChar:
	ld	a,$E1
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

@Lookup:
	ld	a,($97A2)	; cur sel
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
	ld	ix,@Table
	add	ix,de
	ret

;;;;;;;;;;;;;;;;

function(PutsApp):		;PORTED
	rst	20h
	ld	hl,OP1
	bcall	_PutS
	ret

function(ClearCache):
	call LookupAppVar
	ex de,hl
	inc hl
	inc hl
	ld bc,rungeCacheSize
	bcall	_MemClear
	ret

function(Mov9):			;PORTED
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

function(CreateAppvar):		;PORTED
	ld	hl,AppvarName
	rst	20h ;rMOV9TOOP1
	bcall	_ChkFindSym
	jr	nc,@Exists
	ld	hl,AppvarInitSize
	bcall	_CreateAppVar
	inc	de
	inc	de
	ex	de,hl
	ld	bc,rungeCacheSize
	push	hl
	push	bc
	bcall	_MemClear
	pop	hl
	pop	bc
	add	hl,bc
	ex	de,hl
	ld	hl,appvarInitData
	ld	bc,appvarInitDataLength
	ldir
	ex	de,hl
	ld	bc,appVarGraphStylesLength+appVarInitEquationsLength
	bcall	_MemClear
@Exists:
	ld	a,b
	or	a
	jr	z,@NotArchived
	bcall	_Arc_Unarc
@NotArchived:
	ret

function(CreateEquations):
	ld	e,0
@Loop:
	push	de
	ld	hl,equation
	rst	20h ;rMOV9TOOP1
	ld	a,tY1
	pop	de
	push	de
	add	a,e
	ld	(OP1+2),a
	ld	hl,YEquationSize
	bcall	_CreateEqu
	inc	de
	inc	de
	ld	hl,YEquation
	ld	bc,YEquationSize-1
	ldir
	ex	de,hl
	pop	de
	push	de
	ld	a,t0
	add	a,e
	ld	(hl),a
	pop	de
	inc	e
	ld	a,6			;0..5
	cp	e
	jr	nz,@Loop
	ld	hl,EquOffset
	ld	c,tX1T
@Loop2:
	push	bc
	push	hl
	
	call	LookupAppVar
	push	de
	ld	hl,equation
	rst	20h ;rMOV9TOOP1
	pop	de

	pop	hl
	pop	bc
	push	bc
	push	hl
	add	hl,de
	ld	e,(hl)
	inc	hl
	ld	d,(hl)

	ld	a,c
	ld	(OP1+2),a
	ex	de,hl
	bcall	_CreateEqu
	inc	de
	inc	de

	push	de
	call	LookupAppVar	;stack=[equ_data,appvar_offset,current_equation,...]
	ex	de,hl
	pop	de

	pop	bc
	push	bc
	add	hl,bc
	ld	c,(hl)
	ld	(hl),0
	inc	hl
	ld	b,(hl)
	ld	(hl),0
	inc	hl
	push	hl
	push	bc
	ld	a,b
	or	c
	jr	z,@ZDSf
	ldir
@ZDSf:

	pop	de
	pop	hl
	bcall	_DelMem

	pop	hl
	inc	hl
	inc	hl			;new offset

	pop	bc
	inc	c
	ld	a,tY6T+1
	cp	c
	jr	nz,@Loop2
	call	LookupAppVar
	ex	de,hl
	ld	de,AppvarInitSize
	ld	(hl),e
	inc	hl
	ld	(hl),d
	ret
YEquation:
	.db	t2ByteTok,tReal,t3,t4,tComma
YEquationEnd:
YEquationSize = YEquationEnd - YEquation + 1

function(SetupCalc):
	ld	a,$42
	ld	(YEquHookState),a
	call	CreateAppvar
	call	LoadStatusAddress
	;Save bits
	set	SimultBit,(hl)
	bit	grfSimul,(iy+grfDBFlags)
	jr	nz,@ZDSf1
	res	SimultBit,(hl)
@ZDSf1:
	set	ExprBit,(hl)
	bit	0,(iy+24)
	jr	nz,@ZDSf2
	res	ExprBit,(hl)
@ZDSf2:
	;save Graphing styles
	call	LoadStyleAddress
	ex	de,hl
	ld	hl,GX1
	ld	bc,6
	ldir
	call	SaveEquations
	AppOnErr(@Error)
	call	CreateEquations
	AppOffErr
	call	EnableEquations
	ret

@Error:	;Memory is full, error during CreateEqu
	push	af
	call	RestoreCalc@NoSave
	pop	af
	res	7,a
	jp	DisplayError
	

function(RestoreCalc):
	call	SaveParamEquations
	call	SaveEnabledEquations
@NoSave:
	xor	a
	ld	(iy+$35),a		;FIX: restore hooks instead of deleting them
	ld	(iy+$36),a		;disable hooks
	ld	(YEquHookState),a	;not in diffequ mode anymore
	call	DeleteEquations
	call	RestoreEquations
	ld	b,1<<grfFuncM
	call	SetGraphMode
	call	LoadStatusAddress
	bit	SimultBit,(hl)	;Simultaneous mode is enabled
	jr	nz,@ZDSf1
	res	grfSimul,(iy+grfDBFlags)
@ZDSf1:
	bit	ExprBit,(hl)	;ExprOff bit is enabled
	jr	nz,@ZDSf2
	res	0,(iy+24)
@ZDSf2:
	;restore Graphing style 9780..9786 GX1..GX1+5
	call	LoadStyleAddress
	ld	de,GX1
	ld	bc,6
	ldir
	ret

function(SaveEnabledEquations):
	ld	hl,equation
	rst	20h ;rMOV9TOOP1
	ld	b,0
	ld	c,tX6T
@Loop:
	rlc	b
	ld	a,c
	ld	(OP1+2),a
	push	bc
	rst	10h ;rFINDSYM
	pop	bc
	bit	5,(hl)
	jr	z,@ZDSf
	set	0,b
@ZDSf:
	dec	c
	dec	c
	ld	a,tX1T-2
	cp	c
	jr	nz,@Loop
	push	bc
	call	LookupAppVar
	ld	hl,EnabledOffset
	add	hl,de
	pop	bc
	ld	(hl),b
	ret

function(EnableEquations):
	call	LookupAppVar
	ld	hl,EnabledOffset
	add	hl,de
	ld	b,(hl)
	ld	c,tX1T
	push	bc
	ld	hl,equation
	rst	20h ;rMOV9TOOP1
	pop	bc
@Loop:
	push	bc
	ld	a,c
	ld	(OP1+2),a
	rst	10h ;rFINDSYM
	pop	bc
	set	5,(hl)
	bit	0,b
	jr	nz,@ZDSf1
	res	5,(hl)
@ZDSf1:
	bit	0,c
	jr	z,@ZDSf2
	rrc	b
@ZDSf2:
	inc	c
	ld	a,tY6T+1
	cp	c
	jr	nz,@Loop
	ret

function(SaveParamEquations):
	call	LoadEquAddress
	push	hl
	ld	hl,equation
	rst	20h ;rMOV9TOOP1
	pop	de
	ld	c,tX1T
@Loop:
	push	bc
	push	de
	ld	a,c
	ld	(OP1+2),a
	rst	10h ;rFINDSYM
	ex	de,hl

	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	ex	de,hl
	bcall	_EnoughMem		;HL=amount of ram
	jr	c,@Exit
	ex	de,hl
	pop	de
	push	de
	push	hl
	bcall	_InsertMem		;HL=bytes to insert DE=location

	rst	10h ;rFINDSYM
	ex	de,hl
	pop	bc
	pop	de
	inc	bc
	inc	bc			;size bytes
	ldir
	pop	bc
	inc	c
	ld	a,tY6T+1
	cp	c
	jr	nz,@Loop
	ret
@Exit:
	;Not enough memory, just stop saving equations
	pop	bc
	pop	bc
	ret

function(DeleteEquations):
	ld	hl,equation
	rst	20h ;rMOV9TOOP1
	ld	a,tY1
	ld	(OP1+2),a
@Loop:
	rst	10h ;rFINDSYM
	jr	c,@ZDSf1
	bcall	_DelVar
@ZDSf1:
	ld	hl,OP1+2
	inc	(hl)
	ld	a,(hl)
	cp	tY6+1
	jr	nz,@Loop
	ld	a,tX1T
	ld	(OP1+2),a
@Loop2:
	rst	10h ;rFINDSYM
	jr	c,@ZDSf2
	bcall	_DelVar
@ZDSf2:
	ld	hl,OP1+2
	inc	(hl)
	ld	a,(hl)
	cp	tY6T+1
	jr	nz,@Loop2
	ret

function(SaveEquations):
	ld	c,tY1
@Loop1:
	ld	a,c
	add	a,10	;Y1->Y0+1
	ld	b,a
	call	RenameEquation
	inc	c
	ld	a,tY6+1
	cp	c
	jr	nz,@Loop1
	ld	c,tX1T
@Loop2:
	ld	a,c
	add	a,twn-tX1T+1	;X1T->w(n)+1
	ld	b,a
	call	RenameEquation
	inc	c
	ld	a,tY6T+1
	cp	c
	jr	nz,@Loop2
	ret

function(RestoreEquations):
	ld	c,tY1+10
@Loop1:
	ld	a,c
	sub	10			;Y1<-Y0+1
	ld	b,a
	call	RenameEquation
	inc	c
	ld	a,tY6+10+1
	cp	c
	jr	nz,@Loop1
	ld	c,twn+1 
@Loop2:
	ld	a,c
	sub	twn-tX1T+1		;X1T<-w(n)+1
	ld	b,a
	call	RenameEquation
	inc	c
	ld	a,tY6T-tX1T+twn+2
	cp	c
	jr	nz,@Loop2
	ret

function(RenameEquation):	;C=original name B=new name
	push	bc
	ld	a,c
	ld	hl,equation
	rst	20h ;rMOV9TOOP1
	ld	(OP1+2),a
	rst	10h ;rFINDSYM
	ld	de,-7
	add	hl,de
	pop	bc
	ld	(hl),b
	ret

function(SetCalcSpeed):
 	push	af
 	push	bc
 	bcall	_GetBaseVer
 	cp	2
 	jr	nc,@Above112
 	cp	1
 	jr	nz,@Below112
 	ld	a,b
 	cp	13
 	jr	c,@Below112
@Above112:
 	ld	a,$FF
 	bcall	_SetExSpeed
@Below112:
 	pop	bc
 	pop	af
 	ret
 
function(LookupAppVar):		;PORTED
	ld	hl,AppvarName
	rst	20h
	bcall	_ChkFindSym
	ret	nc
	bjump	_ErrUndefined

AppvarName:
	.db	AppVarObj,"Diffequ",0

function(SameSign):		;Z if same sign of OP1 and OP2
	ld	a,(OP1)
	ld	hl,OP2
	xor	(hl)
	and	$80
	ret

function(LoadYi0):
	ld	a,1
	call	LoadEquation@2
	bcall	_ParseInp
	bcall	_CkOP1Real
	ret	z
	jp	ParserHook@ArgumentError

function(LoadEquation):
	xor	a
@2:
	add	a,e
	add	a,e			;a+=2*e
	ld	hl,equation
	rst	20h ;rMOV9TOOP1
	ld	hl,OP1+2
	add	a,(hl)
	ld	(hl),a
	ret
equation:
	.db EquObj, tVarEqu, tX1T,0

function(MultABy9):		;destroys B
	ld	b,a
	add	a,a
	add	a,a
	add	a,a
	add	a,b
	ret

function(CountEquations):	;returns the number of active equations in d
	xor	a
	ld	b,6
@Loop:
	rrc	d
	adc	a,0
	djnz	@Loop
	ret

function(LoadOP2):		;hl=M*256+E
	push	hl
	bcall	_ZeroOP2
	pop	hl
	ld	(OP2+1),hl
	ret

function(LoadYEquation):
	ld	a,tY1
	add	a,e
	ld	hl,equation
	rst	20h ;rMOV9TOOP1
	ld	(OP1+2),a
	ret
	
function(LoadFldres):
	call	LookupAppVar
	ld	hl,FldresOffset
	add	hl,de
	rst	20h ;rMOV9TOOP1
	ret

function(LoadSimpleCacheAddress):
	call	LookupAppVar
	ex	de,hl
	inc	hl
	inc	hl
	ret

function(LoadEquAddress):
	call	LookupAppVar
	ld	hl,EquOffset
	add	hl,de
	ret

function(LoadRKEvalAddress):
	call	LookupAppVar
	ld	hl,RKEvalOffset
	add	hl,de
	ret
	
function(LoadStyleAddress):
	call	LookupAppVar
	ld	hl,StyleOffset
	add	hl,de
	ret

function(LoadStatusAddress):
	call	LookupAppVar
	ld	hl,StatusOffset
	add	hl,de
	ret

function(LoadDiftolAddress):
	call	LookupAppVar
	ld	hl,DiftolOffset
	add	hl,de
	ret

function(LoadDiftol):
	call	LoadDiftolAddress
	rst	20h ;rMOV9TOOP1
	ret

function(RclT):
	ld	a,(cxCurApp)
	cp	cxTableEditor
	jr	nz,@T
	bcall	_RclX
	ret
@T	bcall	_TName
	bcall	_RclVarSym
	ret

function(StoT):
	ld a,(cxCurApp)
	cp cxTableEditor
	jr nz,@T
	bcall	_StoX
@T	bcall	_StoT
	ret

X0	= TMINt
Xstep	= TSTEPt

#include "euler.asm"
#include "runge-kutta.asm"
#include "slopefield.asm"

cacheSwitchBit		= 0
cache1ValidBit		= 1
cache2ValidBit		= 2

cacheSwitchMask		= 1<<cacheSwitchBit
cache1ValidMask		= 1<<cache1ValidBit
cache2ValidMask		= 1<<cache2ValidBit

cacheSimpleValidBit	= 7	;in simple cache statusbits
cacheSimpleValidMask	= 1<<cacheSimpleValidBit

appvarInitData:
	.db 0		;statusbits
	.db 0		;equations to be evaluated in RK mode
	.db 0		;equations that are enabled
appvarInitDataDiftol:
	.db $00,$7D,$10,$00,$00,$00,$00,$00,$00 ;.001 diftol
appvarInitDataFldRes
	.db $00,$80,$80,$00,$00,$00,$00,$00,$00 ;8 fldres
appvarInitDataEnd:
appvarInitDataLength = appvarInitDataEnd - appvarInitData

appVarGraphStylesLength		= 6
appVarInitEquationsLength	= 6*2*2

simpleCacheSize	= 1+7*9 ;X and Y1..Y6
;SIMPLE CACHE:
;0		statusbits
;1..9		X
;10..63	Y6..Y1 (reverse order!)


rungeCacheSize	= simpleCacheSize + endpointCacheSize

;APPVAR
;316		Cache (size of runge cache, always starts with SimpleCache)
;1			Statusbits
;1			Equations to be evaluated in RK mode
;1			Equations that are enabled
;9			Diftol
;6			Graph style of X*T
;?			equations

AppvarInitSize 	= rungeCacheSize + appvarInitDataLength + appVarGraphStylesLength + appVarInitEquationsLength
			;runge cache is larger than euler cache

StatusOffset	= 2 + rungeCacheSize
RKEvalOffset	= 2 + rungeCacheSize + 1
EnabledOffset	= 2 + rungeCacheSize + 2
DiftolOffset	= 2 + rungeCacheSize + 3
FldresOffset	= 2 + rungeCacheSize + 3 + 9
StyleOffset		= 2 + rungeCacheSize + 3 + 9 + 9
EquOffset		= 2 + rungeCacheSize + 3 + 9 + 9 + appVarGraphStylesLength

;STATUSBITS
EulerBit		= 0	;0=euler, 1=RK
SlopeFldBit		= 1	;0=no slope field 1=draw slope field
SimultBit		= 6	;Copy of the simultaneous/sequential bit
ExprBit		= 7	;Copy of the ExprOn/Off bit

end_of_app:
app_size = end_of_app-4080h


;--------------------------------MAYBE LATER--------------------------------
;CLEANUP: Use variable used during graphing to support smart graph
;FIX:implement our own version of tmin,tstep and tmax with more sensible default values for diffequs
;FIX:implement expron/exproff? (problems with either putting it on top of equ nr in upper right corner or disappearing)
;FIX:implement Initial Condition chooser on graph
;FIX:disallow y1(..) calls inside ODE's (Only work inside table, don't like modifying equations)
;FIX:disallow recalling Y1..Y6 in diffequ mode (difficult,not worth it)


;Y= Screen
;* Use only one line to display result, followed by ...
;* When switching away make errors behave like the window screen

;FIX: add fast speed set back in to hooks where it is required
