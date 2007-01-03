
;Original code from:
;User Interface Guideline Sample Application (U1)
;by Dan Englender

;Modified for use in graph3
	
function(NumberedMenu):
				;Input
				; HL points to menu data
				;Output
				; A = column
				; D = currently selected item
	;Display the header
	push	hl
	inc	hl
	ld	de,0
	ld	(curRow),de
	set	textInverse,(IY+textFlags)    ;Set inverse text mode
	call	@putsapp
	pop	hl
@nmRestart:
	ld	c,1			;C = top display item in column
	ld	d,c			;D = currently selected item  
	ld	e,0			;E = 0 = need to refresh text
@nmLoop:	          
	call	@nmDispNumbers		;Display row number/letters
@nmLoop2:
	push	hl
	push	bc                                            
	push	de
	ld hl,MenuCurrent
	ld (hl),0FFh	;for alphaup+down
	push hl
	B_CALL	_GetKey
	pop hl
	ld (hl),0
	pop	de
	pop	bc
	pop	hl
	cp kQuit
	jr nz,@ZDS1
	ld d,(hl)
	ret
@ZDS1:
	sub	kUp
	jr	z,@nmMoveUp
	dec a ;	kDown
	jp	z,@nmMoveDown
	dec a ;	kEnter
	ret z
	dec a ;	kAlphaEnter
	ret z
	dec a ; kAlphaUp
	jr z,@nmAlphaUp
	dec a ; kAlphaDown
	jr z,@nmAlphaDown
	sub k0-kAlphaDown ;k0
	jr     z,@nmPress0           ;Zero comes *after* other numbers
	dec a ;k1
	cp	10
	jr     c,@nmPressNum         ;Number (non-zero) pressed
	sub	kCapA-k1
	cp	26
	jr     c,@nmPressLet         ;Letter pressed
		 jr z,@nmPressXTN
	jr	@nmLoop2   
  
@nmPressXTN:
	bit grfFuncM,(IY+grfModeFlags)
	ld a,'X'-'A'
	jr nz,@nmPressLet
	bit grfParamM,(IY+grfModeFlags)
	ld a,'T'-'A'
	jr z,@nmLoop2
@nmPressLet:
	add    a,10                 ;Letters come after numbers
	jr	@nmPressNum
@nmPress0:
	ld     a,9                  ;Zero comes after nine
@nmPressNum:
	cp	(hl)			;Does the number/letter pressed exist? (a is one lower)
	jr	nc,@nmLoop2		;If not, return to getkey loop
	inc a
	ld	d,a
	ret

@nmAlphaUp:
	ld a,c	;TOP DISPLAYED ITEM
	ld e,0	;redraw
	sub 6
	ld d,a ;selected item
	ld c,a
	jr c,@nmAlphaUp@1
	jr z,@nmAlphaUp@1
	jr @nmLoop
@nmAlphaUp@1:
	ld d,1
	ld c,d
	jr @nmLoop

@nmAlphaDown:
	ld a,c	;TOP DISPLAYED ITEM
	ld e,0	;redraw
	add a,6
	ld d,a ;selected item
	ld a,(hl)
	cp d
	jr z,@ZDS2
	jr nc,@nmAlphaDown@scroll
@ZDS2:
	ld d,a ;selected item
	jr @nmLoop
@nmAlphaDown@scroll:
	ld c,d
	jr @nmLoop
	
@nmMoveUp:				;Move up
	ld	a,d
	dec	d
	jr	z,@nmMUCarry		;Scrolling from top back to bottom
	cp	c			;Is current = top?
	jp	nz,@nmLoop		;If not, don't worry about top
	ld	e,0			;Refresh text if we changed top
	dec	c
	jp	@nmLoop
@nmMUCarry: 
	ld	e,0			;Refresh text if going top to bottom
	inc	d
	ld	a,(8442h)		;Key repeat location
	cp	31h			;Was the key repeating?
	jp	c,@nmLoop2		;If so, we don't want to jump to bottom
	ld a,(hl)
	ld	d,a
	sub	7
	jr nc,@ZDS3
	xor a
@ZDS3:
	inc a
	ld	c,a
	jp	@nmLoop

@nmMoveDown:				;Move down
	ld a,(hl)
	cp	d
	jr	z,@nmMDCarry		;Scrolling from bottom back to top
	ld	a,d
	inc	d
	sub	c
	cp	6	             	;Do we need to adjust top too?
	jp	nz,@nmLoop
	ld	e,0			;Refresh text if adjusting top
	inc	c
	jp	@nmLoop
@nmMDCarry:
	ld	e,0			;Refresh text for bottom to top
	ld	a,(8442h)		;Key repeat location
	cp	31h
	jp	c,@nmLoop2
	jp	@nmRestart		            
	
@nmDispNumbers:			;Display numbers and text
	push	hl
	push	bc
	push	de    
	push   de                   ;Registers need to be saved
	ld	e,(hl)    		;E=# of entries in column

	push de ;get pointer to first string
	ld de,18
	add hl,de
	pop de

	;find first str to display
	ld	b,c
	dec	b
	jr	z,@nmCOCont
	xor	a
@nmCOLoop:
	push	bc
	ld     b,h                  ;Enough BC for our CPIR
	cpir
	pop	bc
	djnz	@nmCOLoop		
@nmCOCont:	;HL->first text string to display

	ld     b,1
@nmDNLoop:
	ld	a,b
	ld	(curRow),a
	xor	a
	ld	(curCol),a
	ld	a,c
	cp	d			;Is it the current selected item?
	jr	nz,@nmDNNoInv
	set	textInverse,(IY+textFlags)	;If so, we need to invert the number
@nmDNNoInv:

	;A=char representation of C
	ld	a,c
	cp	10
	jr	z,@nmGCZero
	jr	nc,@nmGCLetter
	add	a,L0
	jr @nmGCCont
@nmGCZero:
	ld	a,L0
	jr @nmGCCont
@nmGCLetter:
	add	a,LcapA-11	
@nmGCCont:

	B_CALL	_PutC
	ld	a,b
	cp	1			;If it's not the first, no up arrow
	jr	nz,@nmDNNoArrowUp
	cp	c			;If there are non above, no up arrow
	jr	z,@nmDNNoArrowUp
	ld	a,LupArrow
	jr	@nmDNCont
@nmDNNoArrowUp:
	cp	7			;If it's not the last, no down arrow
	jr	nz,@nmDNNoArrowDown
	ld	a,c
	cp	e			;If there's none below, no down arrow
	jr	z,@nmDNNoArrowDown
	ld	a,LdownArrow
	jr	@nmDNCont
@nmDNNoArrowDown:			;If we don't need an up or down arrow
	ld	a,Lcolon		; we want a colon
@nmDNCont:
	B_CALL	_PutC 
	pop	af
	push	af
	res	textInverse,(IY+textFlags)	;Text shouldn't be inverted
	call	nc,@putsapp			;If E=0 then we need to display text
	ld	a,c
	cp	e
	jr	z,@nmClearremaininglines
	inc	b
	inc	c
	ld	a,b
	cp	8
	jr	nz,@nmDNLoop
@nmDNEnd:
	pop	de
	pop	de
	ld	e,1
	pop	bc
	pop	hl
	ret

@nmClearremaininglines:
	ld a,b
	ld hl,8
@nmClearremaininglines@loop:
	dec l
	cp l
	jr z,@nmDNEnd
	ld (currow),hl
	B_CALL _EraseEOL
	jr @nmClearremaininglines@loop
	

@putsapp:				;Display a string of large text
	ld	a,(hl)
	inc	hl
	or	a
	jr	z,@putsapp@clearline
	B_CALL	_PutC
	jr	@putsapp
@putsapp@clearline:
	res	textInverse,(IY+textFlags)    ;In general, we don't want inverse
	ld a,(curcol)
	or a
	ret z
	B_CALL	_EraseEOL
	ret
