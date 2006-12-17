;Equates
_Mon		  equ	401Eh
_sendKPress       equ   4024h
_JForceCmd        equ   402Ah
_newContext0      equ   4033h
_cxPutAway        equ   4036h
_IsA2ByteTok      equ   42A3h
_RESETWINTOP      equ   42AFh
_wputsEOL         equ   4522h
_setNumWindow     equ   452Bh
_homeUp	          equ	4558h
_saveCmdShadow    equ   4573h
_rstrShadow       equ   4579h
_saveTR           equ   4588h
_restoreTR        equ   458Bh
_LoadMenuNum	  equ	45D9h
_LoadMenuNumL	  equ	45DCh
_MenCatRet	  equ	45DFh
_RstrOScreen	  equ	45F7h
_SaveOScreen	  equ	45FAh
_numPPutAway	  equ	477Ah
_numRedisp	  equ	477Dh
_COORDISP	  equ	4816h
_Ceiling	  equ	489Ah
_PutXY            equ   489Dh
_bufToTop	  equ	4927h
_BufClear	  equ   4936h
_CursorToStart	  equ   4939h ;4945h
_CursorDown	  equ   4948h
_SetEmptyEditPtr  equ   4969h
_OP1ToEdit	  equ   49A5h
_rclToQueue       equ   49B4h
_OpenEditEqu	  equ   49C3h
_UpdateYEqu	  equ   49CCh
_dispErrorScreen  equ	49DEh
_POPCX            equ   49E1h
_CheckSplitFlag   equ   49F0h
_cxRedisp	  equ	4C6Ch
_ClearRow         equ   4CEDh
_SetCursorHook    equ   4F60h
_SetGetKeyHook	  equ	4F66h
_ClrCursorHook    equ   4F69h
_SetWindowHook    equ   4FB1h
_SetGraphModeHook equ   4FB7h
_SetRegraphHook   equ   4FEAh
_SetYeditHook     equ   4FCFh
_SetReGraphHook	  equ   4FEAh
_SetTraceHook     equ   4FF3h
_SetParserHook 	  equ   5026h
_SetAppSwitchHook equ   502Ch
_SetAppChangeHook equ   502Ch
_SetMenuHook	  equ	5083h

GX1               equ   9780h
YEditHookState	  equ   9B98h+3

ThreeDState       equ   41h
DiffEquState      equ   42h


;Some functions used in both 3d and diffequ mode
function(PutsApp):
      rst   20h
      ld    hl,OP1
      bcall _PutS
      ret

function(Mov9ToOP2):
	ld    de,OP2
Mov9:
	ldi \ ldi \ ldi
	ldi \ ldi \ ldi
	ldi \ ldi \ ldi
	ret

function(LookupAppVar):
	rst	20h
	bcall	_ChkFindSym
	ret	nc
	bjump	_ErrUndefined

function(ThreeD@CheckGraphMode):
      ld    e,a
      ld    a,(YEditHookState)
      cp    ThreeDState
;      ld    a,(flags + grfModeFlags)
;      and   %11110000
      jr    z,@Z
      xor   a
      ld    a,e
      ret
@Z    or    $FF
      ld    a,e
      ret

function(DEQ@CheckGraphMode):
      ld    e,a
      ld    a,(YEditHookState)
      cp    DiffEquState
      jr    z,@Z
      xor   a
      ld    a,e
      ret
@Z    or    $FF
      ld    a,e
      ret

function(GetGraphMode):
	ld a,(YEditHookState)
	cp ThreeDState
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

;;;;;;;;;;;;;;;;
function(YeditHook):
	call  SetCalcSpeed

	cp    6
	jp    nz,@NotInstall
	ld    a,b
	cp    kApp
	ld    a,6
	jp    nz,@NotInstall
	;Y= key pressed run install/uninstall routine
	jp    ThreeD@FlipInstalled

@NotInstall:
	call  ThreeD@CheckGraphMode
	jp    nz,ThreeD@YeditHook
	call  DEQ@CheckGraphMode
	ret   z
	jp    DEQ@YeditHook

;;

function(WindowHook):
	call  SetCalcSpeed

	push	af
	ld	a,(cxCurApp)
	cp	kWindow
	jr	z,@Possible
	pop	af
	xor	a
	ret
@Possible:
	pop	af
	call	ThreeD@CheckGraphMode
	jp	nz,ThreeD@WindowHook
	call	DEQ@CheckGraphMode
	ret z
	jp DEQ@WindowHook

;;

function(GraphHook):
	call	SetCalcSpeed
	call	ThreeD@CheckGraphMode
	jp 	nz,ThreeD@GraphHook
	call 	DEQ@CheckGraphMode
	jp 	nz,DEQ@GraphHook
	xor	a
	ret

;;

function(AppChangeHook):
	call  SetCalcSpeed
	ld    d,a
	call  GetGraphMode
	jp    z,ThreeD@AppChangeHook
	cp    DiffEquState
	ld    a,d
	jp    z,DEQ@AppChangeHook
	ret

;;;;;;;;;;;;;;;;Start of ModeHook

function(ModeHook):
	ld	hl,$97A2		; current selection
	ld	(hl),0

	call	getGraphMode
	jr	z,@SkipDEQFlags
	call	DEQ@LoadStatusAddress	;Destroys OP1
	ld	a,(hl)
	ld	(_Flags),a

@SkipDEQFlags:
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

@Table3D:
.db %00000101 \ .dw tRectG,     flags + grfDBFlags \ .db %00001000,      %00000000      ,0
.db %00000110 \ .dw tPolarG,    flags + grfDBFlags \ .db %00001000,      %00001000      ,0
.db %00001101 \ .dw tCoordOn,   _Flags             \ .db %00010000,      %00000000      ,0
.db %00001110 \ .dw tCoordOff,  _Flags             \ .db %00010000,      %00010000      ,0
.db %00001101 \ .dw tAxisOff,   _Flags             \ .db %00100000,      %00100000      ,0
.db %00001110 \ .dw tAxisOn,    _Flags             \ .db %00100000,      %00000000      ,0
.db %00001001 \ .dw tLblOff,    _Flags             \ .db %01000000,      %00000000      ,0
.db %00001010 \ .dw tLblOn,     _Flags             \ .db %01000000,      %01000000      ,0

@TableDEQ:
.db %00000101 \ .dw tRectG,     flags + grfDBFlags \ .db 1<<grfPolar,    0	        ,0
.db %00000110 \ .dw tPolarG,    flags + grfDBFlags \ .db 1<<grfPolar,    1<<grfPolar	,0
.db %00001101 \ .dw tCoordOn,   flags + grfDBFlags \ .db 1<<grfNoCoord,  0	        ,0
.db %00001110 \ .dw tCoordOff,  flags + grfDBFlags \ .db 1<<grfNoCoord,  1<<grfNoCoord	,0
.db %00001101 \ .dw tGridOff,   flags + grfDBFlags \ .db 1<<grfGrid,     0	        ,1
.db %00001110 \ .dw tGridOn,    flags + grfDBFlags \ .db 1<<grfGrid,     1<<grfGrid	,1
.db %00001101 \ .dw tAxisOn,    flags + grfDBFlags \ .db 1<<grfNoAxis,   0	        ,1
.db %00001110 \ .dw tAxisOff,   flags + grfDBFlags \ .db 1<<grfNoAxis,   1<<grfNoAxis	,1
.db %00001101 \ .dw tLblOff,    flags + grfDBFlags \ .db 1<<grfLabel,    0	        ,0
.db %00001110 \ .dw tLblOn,     flags + grfDBFlags \ .db 1<<grfLabel,    1<<grfLabel	,0
.db %00001101 \ .dw @Strings_Euler,      _Flags    \ .db 1<<EulerBit,    0	        ,2
.db %00001110 \ .dw @Strings_RungeKutta, _Flags    \ .db 1<<EulerBit,    1<<EulerBit	,2
.db %00001001 \ .dw @Strings_NoField,    _Flags    \ .db 1<<SlopeFldBit, 0	        ,1
.db %00001010 \ .dw @Strings_Field,      _Flags    \ .db 1<<SlopeFldBit, 1<<SlopeFldBit	,1

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

	call	getGraphMode
	jr	z,@Quit@SkipDEQFlags
	call	DEQ@LoadStatusAddress	;Destroys OP1
	ld	a,(_Flags)
	ld	(hl),a

@Quit@SkipDEQFlags:
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

	call	@LoadTable
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
	;erase cache when needed, not reachable in 3D mode
	push	hl
	push	de
	push	af
	call	DEQ@ClearCache
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
	call 	@LoadTable
	add	ix,de
	ret

@LoadTable:
	call	getGraphMode
	jr	z,@LoadTable@3D
	ld	ix,@TableDEQ
	ret
@LoadTable@3D:
	ld ix,@Table3D
	ret
