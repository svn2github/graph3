#define  APP_NAME "Graph3  "
#define  APP_PAGES 1
#define  FAST_SE
#include "app.inc"

#define RES 11

_Mon			equ	401Eh
_sendKPress       equ   4024h
_JForceCmd        equ   402Ah
_newContext0      equ   4033h
_cxPutAway        equ   4036h
_RESETWINTOP      equ   42AFh
_wputsEOL         equ   4522h
_setNumWindow     equ   452Bh
_homeUp		equ	4558h
_saveCmdShadow    equ   4573h
_rstrShadow       equ   4579h
_saveTR           equ   4588h
_restoreTR        equ   458Bh
_LoadMenuNum	equ	45D9h
_LoadMenuNumL	equ	45DCh
_MenCatRet		equ	45DFh
_RstrOScreen	equ	45F7h
_SaveOScreen	equ	45FAh
_numPPutAway	equ	477Ah
_numRedisp		equ	477Dh
_COORDISP		equ	4816h
_Ceiling		equ	489Ah
_PutXY            equ   489Dh
_bufToTop		equ	4927h
_rclToQueue       equ   49B4h
_dispErrorScreen	equ	49DEh
_POPCX            equ   49E1h
_CheckSplitFlag   equ   49F0h
_cxRedisp		equ	4C6Ch
_ClearRow         equ   4CEDh
_SetCursorHook    equ   4F60h
_SetGetKeyHook	equ	4F66h
_ClrCursorHook    equ   4F69h
_SetGraphModeHook equ   4FB7h
_SetRegraphHook   equ   4FEAh
_SetTraceHook     equ   4FF3h
_SetYeditHook     equ   4FCFh
_SetWindowHook    equ   4FB1h
_SetAppChangeHook equ   502Ch
_SetMenuHook	equ	5083h

	jp	Init		; we do this so that if they update the app, the pointers are intact
HookY	.db	$83
	jp	ThreeD@YeditHook
HookW	.db	$83
	jp	ThreeD@WindowHook
HookG	.db	$83
	jp	ThreeD@GraphHook
HookA	.db	$83
	jp	ThreeD@AppChangeHook

function(Init):
	ld	hl,saveSScreen
	ld	de,saveSScreen + 1
	ld	bc,7 * 12 - 1
	ld	(hl),$FF
	ldir
	inc	hl
	inc	de
	ld	bc,50 * 12 - 1
	ld	(hl),$00
	ldir
	inc	hl
	inc	de
	ld	bc, 7 * 12 - 1
	ld	(hl),$FF
	ldir
	bcall	_RstrOScreen

	set	textInverse,(iy + textFlags)
	xor	a
	ld	(penRow),a
	ld	hl,ThreeD@Strings@Title
	call	ThreeD@VPutsCenter
	ld	a,$39
	ld	(penRow),a
	ld	hl,ThreeD@Strings@Email
	call	ThreeD@VPutsCenter
	res	textInverse,(iy + textFlags)

	bit	2,(iy+$35)
	jr	z,@Ok1
	ld	hl,$9B90 + 2
	ld	a,(hl)
	inc	a
	jr	z,@Ok1
	call	@CheckInstall
	jp	nz,@Conflict
	jr	@Uninstall
@Ok1	bit	3,(iy+$35)
	jr	z,@Ok2
	ld	hl,$9B94 + 2
	ld	a,(hl)
	inc	a
	jr	z,@Ok2
	call	@CheckInstall
	jr	nz,@Conflict
	jr	@Uninstall
@Ok2	bit	4,(iy+$35)
	jr	z,@Ok3
	ld	hl,$9B98 + 2
	ld	a,(hl)
	inc	a
	jr	z,@Ok3
	call	@CheckInstall
	jr	nz,@Conflict
	jr	@Uninstall
@Ok3	bit	2,(iy+$36)
	jr	z,@Ok4
	ld	hl,$9BB0 + 2
	ld	a,(hl)
	inc	a
	jr	z,@Ok4
	call	@CheckInstall
	jr	nz,@Conflict
	jr	@Uninstall
@Ok4	; if we got here, no conflict exists and no uninstall; simply install it and show message.

      ld    hl,ThreeD@Strings@GraphName
      rst   20h
      bcall _ChkFindSym
      jr	c,@NonExistant
      bcall	_DelVarArc
@NonExistant:

      in    a,(6)
      ld    hl,HookY
      bcall _SetYeditHook
      ld    hl,HookW
      bcall _SetWindowHook
      ld	hl,HookG
      bcall	_SetGraphModeHook
      ld    hl,HookA
      bcall _SetAppChangeHook

	ld	hl,ThreeD@Strings@Installed1
	ld	de,ThreeD@Strings@Installed2
@Common:
	call	ThreeD@MessageBox
	cp	kYequ
	jr	z,@Yequ
	bjump	_JForceCmdNoChar
@Yequ	ld	a,kYequ
	bcall	_newContext0
	xor	a
	bjump	_SendKPress

@Uninstall:
	ld	hl,flags + $35
	ld	a,(hl)
	and	%11100011
	ld	(hl),a
	inc	hl
	res	2,(hl)

	ld	hl,ThreeD@Strings@Removed1
	ld	de,ThreeD@Strings@Removed2
	jr	@Common

@Conflict:
	ld	a,e
	ld	de,$8040	; Field: App Name
	ld	hl,$4000	; Start at the beginning (the page is in A already)
	push	af
	bcall	$8075		; search for app field
	pop	af
	inc	hl
	inc	hl
	ld	de,OP5
	ld	b,8
@ConflictLoop:
	push	bc
	ld	b,a
	bcall	_LoadCIndPaged
	inc	hl
	ex	de,hl
	ld	(hl),c
	inc	hl
	ex	de,hl
	pop	bc
	djnz	@ConflictLoop
@ConflictFind:
	ex	de,hl
	dec	hl
	ld	a,(hl)
	cp	' '
	jr	z,@ConflictFind
	inc	hl
	ld	(hl),0
	ld	hl,OP5
@ConflictShorten:
	ld	a,(hl)
	inc	hl
	or	a
	jr	z,@ConflictDone
	add	a,a
	jr	nz,@ConflictShorten
	dec	hl
	ld	(hl),0
@ConflictDone:
	ld	hl,ThreeD@Strings@Uninstall
	ld	de,OP5-8
	push	de
	ld	bc,8
	ldir
	pop	de
	jr	@Common

@CheckInstall:
	ld	a,(hl)
	push	af
	bcall	_GetBaseVer
	cp	2
	jr	nc,@Good
	cp	1
	jr	nz,@Bad
	ld	a,b
	cp	13	;1.13?
	jr	c,@Bad
@Good	pop	af
	bcall	$50BC	;get app base page
	ld	e,a
	jr	@Cont
@Bad	pop	af
	ld	hl,$8230
	ld	d,0
	ld	e,a
	add	hl,de
	ld	e,(hl)
@Cont	in	a,(6)
	cp	e
	ret

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

;Include 3D functionality
#include "3d/mainhook.asm"
