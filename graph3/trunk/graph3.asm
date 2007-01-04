;
; Copyright (c) 2007 Detached Solutions
;
; Permission is hereby granted, free of charge, to any person
; obtaining a copy of this software and associated documentation
; files (the "Software"), to deal in the Software without
; restriction, including without limitation the rights to use,
; copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the
; Software is furnished to do so, subject to the following
; conditions:
;
; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
; OTHER DEALINGS IN THE SOFTWARE.
;

#define  APP_NAME "Graph3  "
#define  APP_PAGES 1
#define  FAST_SE
#include "app.inc"

#define RES 11

	jp	Init		; we do this so that if they update the app, the pointers are intact
HookY	.db	$83
	jp	YeditHook
HookW	.db	$83
	jp	WindowHook
HookG	.db	$83
	jp	GraphHook
HookA	.db	$83
	jp	AppChangeHook

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
	call	VPutsCenter
	ld	a,$39
	ld	(penRow),a
	ld	hl,ThreeD@Strings@Email
	call	VPutsCenter
	res	textInverse,(iy + textFlags)

	bit	2,(iy+$35)
	jr	z,@Ok1
	ld	hl,$9B90 + 2
	ld	a,(hl)
	inc	a
	jr	z,@Ok1
	call	@CheckInstall
	jp	nz,@Conflict
	jp	@Uninstall
@Ok1	bit	3,(iy+$35)
	jr	z,@Ok2
	ld	hl,$9B94 + 2
	ld	a,(hl)
	inc	a
	jr	z,@Ok2
	call	@CheckInstall
	jp	nz,@Conflict
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

	xor	a
	ld	(YEdithookState),a
	
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
	call	MessageBox
	cp	kYequ
	jr	z,@Yequ
ExitApp:
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

#include "menu.asm"
#include "shared.asm"

;include DEQ functionality
#include "diffequ/diffeq.asm"

;Include 3D functionality
#include "3d/mainhook.asm"
