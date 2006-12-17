StartApp:
	call	DEQ@CreateAppvar

	ld	a,(YEditHookState)
	cp	$42
	jr	z,DEQ@Restore

	call	DEQ@SetupCalc

	in	a,(6)
	ld	hl,DEQ@ParserHook
	bcall	_SetParserHook

	ld	hl,DEQ@AppChangeHook
	bcall	_SetAppSwitchHook

	ld	hl,DEQ@RegraphHook
	bcall	_SetReGraphHook

	ld	hl,DEQ@KeyHook
	bcall	_SetGetKeyHook

	ld	hl,DEQ@MenuHook
	bcall	_SetMenuHook

	ld	hl,DEQ@WindowHook
	bcall	_SetWindowHook

	ld	hl,DEQ@GraphHook
	bcall	_SetGraphModeHook

	ld	hl,DEQ@YeditHook
	bcall	_SetYeditHook

ExitApp:
	bjump	_JForceCmdNoChar

DEQ@Restore:
	call	DEQ@RestoreCalc
	jr	ExitApp

function(DEQ@ParserHook):
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
function(DEQ@ExecuteDiffequAlgorithm):
	push	de
	call	DEQ@LoadStatusAddress
	pop	de
	ld	d,(hl)
	push	de
	bit	RealEquBit,d
	jr	z,@DataTypeError	;bomb out when the equations aren't real

	call	DEQ@RclT
	rst	18h ;rPUSHREALO1	;save X

	call	DEQ@LoadSimpleCacheAddress
	push	hl
	B_CALL _CpyTo1FPST	;X
	pop	hl
	bit	cacheSimpleValidBit,(hl)
	jr	z,@SkipSimpleCache
	inc	hl
	push	hl
	bcall	_Mov9OP2Cp
	pop	hl
	jr	nz,@SkipSimpleCache
	push	hl
	ld	de,9
	bcall	_DeallocFPS1
	pop	hl
	pop	de
	dec	e			;skip X
	call	DEQ@Runge@HLPlusCacheOffset
	rst	20h ;rMOV9TOOP1
	or	$FF
	ret

@SkipSimpleCache:
	pop	ix
	push	ix
	AppOnErr(@Error)
	push	ix
	pop	de
	bit	EulerBit,d
	jr	nz,@RK
	call	DEQ@Euler
@Finish:
	AppOffErr
	pop	ix
	bcall	_PopRealO1
	call	DEQ@StoT
	bcall	_OP2ToOP1
	or	$FF
	ret
@RK:
	call	DEQ@Runge
	jr	@Finish

@DataTypeError:
	ld	a,9			;datatype error without goto option
	bjump	_JError

@Error:
	pop	de
	push	af
	bit	EulerBit,d
	call	nz,DEQ@ClearCache	;Simple erase cache in case of RK
	bcall	_PopRealO1
	call	DEQ@StoT
	pop	af
	ld	b,a
	and	$7F
	cp	6			;ON:BREAK
	jr	z,DisplayError
DisplayOriginalError:
	ld	a,b
DisplayError:
	bjump	_JError

function(DEQ@RegraphHook):
	.db	$83
	call	SetCalcSpeed
	cp	$09
	jr	nz,@Not9
	call	DEQ@SlopeField
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
	call	DEQ@ExecuteDiffequAlgorithm
	AppOffErr
	jr	@ReturnNZ
@X:					;just return T (already in OP1)
	set	numOP1,(iy+ParsFlag2)
	call	DEQ@RclT
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

function(DEQ@SetGraphMode):
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

function(DEQ@AppChangeHook):
	.db	$83
	call	SetCalcSpeed
	push	af
	push	hl
	ld	a,b
	cp	kYequ
	call	z,@ExitYEqu
	ld	b,1<<grfParamM
	call	DEQ@SetGraphMode
	pop	hl
	pop	af
	ld	b,1<<grfFuncM
	cp	cxTableEditor
	jr	z,DEQ@SetGraphMode	;let SetGraphMode return
	cp	kFormat
	jp	z,ModeHook
	ret

function(DEQ@AppChangeHook@ExitYEqu):
	AppOnErr(RemoveGotoErrorHandler)

	;figure out which equations need to be evaluated in RK mode
	ld	de,0
@Loop2:
	push	de
	call	DEQ@LoadEquation
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
	call	DEQ@LookupAppVar
	push	de
	pop	bc
	ld	hl,RKEvalOffset
	add	hl,de
	pop	de
	ld	(hl),d
	push	de

	;Erase cache if smart graph can't be used
	bit	smartGraph_inv,(iy+smartFlags)
	call	nz,DEQ@ClearCache

	call	DEQ@LoadSimpleCacheAddress
	res	cacheSimpleValidBit,(hl)
	;fill simple cache with T
	call	DEQ@LoadSimpleCacheAddress
	set	cacheSimpleValidBit,(hl)
	inc	hl
	push	hl
	bcall	_OP1Set0
	call	DEQ@StoT
	pop	de
	ld	b,7
@ZDSb9:
	push	bc
	ld	hl,OP1
	call	Mov9
	pop	bc
	djnz	@ZDSb9
	call	DEQ@LoadStatusAddress
	set	RealEquBit,(hl)

	pop	de
	ld	e,0
@Loop:
	push	de
	;Dis/Enable Y* function
	call	DEQ@LoadEquation
	rst	10h ;rFINDSYM
	ld	a,(hl)
	and	1<<5			;the bit that dis/enables an equation
	pop	de
	push	de
	push	af
	call	DEQ@LoadYEquation
	rst	10h ;rFINDSYM
	ld	a,(hl)
	res	5,a
	pop	bc
	or	b			;dis/enable equation
	ld	(hl),a
	pop	de
	push	de
	;Check that equations returns a real value
	call	DEQ@LoadEquation
	AppOnErr(@SkipEqu)
	bcall	_ParseInp
	AppOffErr
	bit	numOP1,(IY+ParsFlag2)
	jr	z,@SkipEqu		;don't save
	bcall	_CkOP1Real
	pop	de
	push	de
	call	nz,@ResetRealEquBit
@SkipEqu:
	pop	de
	push	de
	;Update initial value
	ld	a,1
	call	DEQ@LoadEquation@2
	AppOnErr(@SkipInitial)
	bcall	_ParseInp
	AppOffErr
	bit	numOP1,(IY+ParsFlag2)
	jr	z,@SkipInitial	;don't save
	bcall	_CkOP1Real
	pop	de
	push	de
	call	nz,@ResetRealEquBit
	jr	nz,@SkipInitial	;don't save
	bcall	_OP1ToOP6
	pop	de
	push	de
	ld	a,1
	call	DEQ@LoadEquation@2
	rst	10h ;rFINDSYM
	bcall	_SetEmptyEditPtr
	bcall	_OP6ToOP1
	bcall	_OP1ToEdit
	bcall	_CloseEditEqu
@SkipInitial:
	pop	de
	inc	e
	ld	a,6			;0..5
	cp	e
	jr	nz,@Loop
	bcall	_CleanAll
	AppOffErr
	ret

@ResetRealEquBit:			;FIX: make sure this is only called when enabled/used equations are nonreal
	push	af
	ld	b,e
	inc	b
	rlc	d
@ZDSb8:
	rrc	d
	djnz	@ZDSb8

	bit	0,d
	jr	z,@ZDSf8
	call	DEQ@LoadStatusAddress
	res	RealEquBit,(hl)
@ZDSf8:
	pop	af
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
	call	DEQ@LoadEquation
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

function(DEQ@KeyHook):
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

function(DEQ@GraphHook):
	.db	$83
	call	SetCalcSpeed
@Not2:
	cp	$06
	jr	nz,@Not6
	ld	a,b
	cp	kStd
	jr	nz,@Allow
	push	bc
	call	DEQ@LoadDiftolAddress
	ex	de,hl
	ld	hl,appvarInitDataDiftol
	call	Mov9
	call	Mov9			;copy FldRes
	pop	bc
@Not6:
@Allow:
	xor	a
	ret

function(DEQ@YeditHook):
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
	call	DEQ@LoadRKEvalAddress
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
	call	DEQ@LoadEquation@2
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

function(DEQ@MenuHook):		;PORTED (changed)
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

CalcMenu:
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

function(DEQ@WindowHook):	;PORTED (changed to support different menus, 3d functionality should be possible using this function)
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
	call	DEQ@ClearCache
	bcall	_PopRealO1
@6Token:
	ld	hl,OP1
	or	$FF
	ret
@Not6:
	dec	a
	jr	nz,@Not7
	call	DEQ@LoadStatusAddress
	ld	a,(hl)
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
	call	DEQ@LookupAppVar
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


;;;;;;;;;;;;;;;;

function(DEQ@ClearCache):
	call	DEQ@LookupAppVar
	ex	de,hl
	inc	hl
	inc	hl
	ld	bc,CacheSize
	bcall	_MemClear
	ret

function(DEQ@CreateAppvar):		;PORTED (changed here and in 3d, take a look at this when merging)
	ld	hl,AppvarName
	rst	20h ;rMOV9TOOP1
	bcall	_ChkFindSym
	jr	nc,@Exists
	ld	hl,AppvarInitSize
	bcall	_CreateAppVar
	inc	de
	inc	de
	ex	de,hl
	ld	bc,CacheSize
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

function(DEQ@CreateEquations):
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

	call	DEQ@LookupAppVar
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
	call	DEQ@LookupAppVar	;stack=[equ_data,appvar_offset,current_equation,...]
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
	call	DEQ@LookupAppVar
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

function(DEQ@SetupCalc):
	ld	a,$42
	ld	(YEditHookState),a
	call	DEQ@CreateAppvar
	call	DEQ@LoadStatusAddress
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
	call	DEQ@LoadStyleAddress
	ex	de,hl
	ld	hl,GX1
	ld	bc,6
	ldir
	call	DEQ@SaveEquations
	AppOnErr(@Error)
	call	DEQ@CreateEquations
	AppOffErr
	call	DEQ@EnableEquations
	bcall	_SetTblGraphDraw	;dirty graph and table
	ret

@Error:	;Memory is full, error during CreateEqu
	push	af
	call	DEQ@RestoreCalc@NoSave
	pop	af
	res	7,a
	jp	DisplayError


function(DEQ@RestoreCalc):
	call	DEQ@SaveParamEquations
	call	DEQ@SaveEnabledEquations
@NoSave:
	xor	a
	ld	(iy+$35),a
	ld	(iy+$36),a		;disable hooks
	ld	(YEditHookState),a	;not in diffequ mode anymore
	call	DEQ@DeleteEquations
	call	DEQ@RestoreEquations
	ld	b,1<<grfFuncM
	call	DEQ@SetGraphMode
	call	DEQ@LoadStatusAddress
	bit	SimultBit,(hl)	;Simultaneous mode is enabled
	jr	nz,@ZDSf1
	res	grfSimul,(iy+grfDBFlags)
@ZDSf1:
	bit	ExprBit,(hl)	;ExprOff bit is enabled
	jr	nz,@ZDSf2
	res	0,(iy+24)
@ZDSf2:
	;restore Graphing style 9780..9786 GX1..GX1+5
	call	DEQ@LoadStyleAddress
	ld	de,GX1
	ld	bc,6
	ldir
	ret

function(DEQ@SaveEnabledEquations):
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
	call	DEQ@LookupAppVar
	ld	hl,EnabledOffset
	add	hl,de
	pop	bc
	ld	(hl),b
	ret

function(DEQ@EnableEquations):
	call	DEQ@LookupAppVar
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

function(DEQ@SaveParamEquations):
	call	DEQ@LoadEquAddress
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

function(DEQ@DeleteEquations):
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

function(DEQ@SaveEquations):
	ld	c,tY1
@Loop1:
	ld	a,c
	add	a,10	;Y1->Y0+1
	ld	b,a
	call	DEQ@RenameEquation
	inc	c
	ld	a,tY6+1
	cp	c
	jr	nz,@Loop1
	ld	c,tX1T
@Loop2:
	ld	a,c
	add	a,twn-tX1T+1	;X1T->w(n)+1
	ld	b,a
	call	DEQ@RenameEquation
	inc	c
	ld	a,tY6T+1
	cp	c
	jr	nz,@Loop2
	ret

function(DEQ@RestoreEquations):
	ld	c,tY1+10
@Loop1:
	ld	a,c
	sub	10			;Y1<-Y0+1
	ld	b,a
	call	DEQ@RenameEquation
	inc	c
	ld	a,tY6+10+1
	cp	c
	jr	nz,@Loop1
	ld	c,twn+1
@Loop2:
	ld	a,c
	sub	twn-tX1T+1		;X1T<-w(n)+1
	ld	b,a
	call	DEQ@RenameEquation
	inc	c
	ld	a,tY6T-tX1T+twn+2
	cp	c
	jr	nz,@Loop2
	ret

function(DEQ@RenameEquation):	;C=original name B=new name
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

function(DEQ@LookupAppVar):		
	ld	hl,AppvarName
	jp LookupAppVar

AppvarName:
	.db	AppVarObj,"Diffequ",0

function(DEQ@SameSign):		;Z if same sign of OP1 and OP2
	ld	a,(OP1)
	ld	hl,OP2
	xor	(hl)
	and	$80
	ret

function(DEQ@LoadYi0):
	ld	a,1
	call	DEQ@LoadEquation@2
	bcall	_ParseInp
	bcall	_CkOP1Real
	ret	z
	jp	DEQ@ParserHook@ArgumentError

function(DEQ@LoadEquation):
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

function(DEQ@CountEquations):	;returns the number of active equations in d
	xor	a
	ld	b,6
@Loop:
	rrc	d
	adc	a,0
	djnz	@Loop
	ret

function(DEQ@LoadOP2):		;hl=M*256+E
	push	hl
	bcall	_ZeroOP2
	pop	hl
	ld	(OP2+1),hl
	ret

function(DEQ@LoadYEquation):
	ld	a,tY1
	add	a,e
	ld	hl,equation
	rst	20h ;rMOV9TOOP1
	ld	(OP1+2),a
	ret

function(DEQ@LoadFldres):
	call	DEQ@LookupAppVar
	ld	hl,FldresOffset
	add	hl,de
	rst	20h ;rMOV9TOOP1
	ret

function(DEQ@LoadSimpleCacheAddress):
	call	DEQ@LookupAppVar
	ex	de,hl
	inc	hl
	inc	hl
	ret

function(DEQ@LoadEquAddress):
	call	DEQ@LookupAppVar
	ld	hl,EquOffset
	add	hl,de
	ret

function(DEQ@LoadRKEvalAddress):
	call	DEQ@LookupAppVar
	ld	hl,RKEvalOffset
	add	hl,de
	ret

function(DEQ@LoadStyleAddress):
	call	DEQ@LookupAppVar
	ld	hl,StyleOffset
	add	hl,de
	ret

function(DEQ@LoadStatusAddress):
	call	DEQ@LookupAppVar
	ld	hl,StatusOffset
	add	hl,de
	ret

function(DEQ@LoadDiftolAddress):
	call	DEQ@LookupAppVar
	ld	hl,DiftolOffset
	add	hl,de
	ret

function(DEQ@LoadDiftol):
	call	DEQ@LoadDiftolAddress
	rst	20h ;rMOV9TOOP1
	ret

function(DEQ@RclT):
	ld	a,(cxCurApp)
	cp	cxTableEditor
	jr	nz,@T
	bcall	_RclX
	ret
@T	bcall	_TName
	bcall	_RclVarSym
	ret

function(DEQ@StoT):
	ld a,(cxCurApp)
	cp cxTableEditor
	jr nz,@T
	bcall	_StoX
@T	bcall	_StoT
	ret

X0	= TMINt
Xstep	= TSTEPt

#include "diffequ/euler.asm"
#include "diffequ/runge-kutta.asm"
#include "diffequ/slopefield.asm"
;#include "euler.asm"
;#include "runge-kutta.asm"
;#include "slopefield.asm"
;#include "../shared.asm"

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


CacheSize		= simpleCacheSize + rungeCacheSize

;APPVAR
;64		SimpleCache
;252		Cache (size of runge cache)
;1			Statusbits
;1			Equations to be evaluated in RK mode
;1			Equations that are enabled
;9			Diftol
;6			Graph style of X*T
;?			equations

AppvarInitSize 	= CacheSize + appvarInitDataLength + appVarGraphStylesLength + appVarInitEquationsLength
			;runge cache is larger than euler cache

StatusOffset	= 2 + CacheSize
RKEvalOffset	= 2 + CacheSize + 1
EnabledOffset	= 2 + CacheSize + 2
DiftolOffset	= 2 + CacheSize + 3
FldresOffset	= 2 + CacheSize + 3 + 9
StyleOffset		= 2 + CacheSize + 3 + 9 + 9
EquOffset		= 2 + CacheSize + 3 + 9 + 9 + appVarGraphStylesLength

;STATUSBITS
EulerBit		= 0	;0=euler, 1=RK
SlopeFldBit		= 1	;0=no slope field 1=draw slope field
RealEquBit		= 2	;0=an equation returns a non-real value (usually a list)
				;1=all equations return a real value
SimultBit		= 6	;Copy of the simultaneous/sequential bit
ExprBit		= 7	;Copy of the ExprOn/Off bit

end_of_app:
app_size = end_of_app-4080h

;FIX:restore hooks instead of deleting them
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
