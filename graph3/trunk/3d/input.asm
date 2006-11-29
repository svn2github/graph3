function(ThreeD@CreateTemp):
      ld    a,(hl)
      inc   hl
      push  hl
      ld    h,(hl)
      ld    l,a
      push  hl
      call  ThreeD@DeleteEntry
      pop   hl
      push  hl
      bcall _CreateEqu
      ld    a,(_Flags)
      rra
      jr    nc,@NotSel
      set   5,(hl)
@NotSel:
      pop   bc
      pop   hl
      inc   de
      inc   bc
      ldir
      ret

function(ThreeD@DeleteEntry):
      call  ThreeD@LookupEntry
      ret   c
@Del  ld    a,(hl)
      push  af
      bcall _DelVarArc
      pop   af
      ret

function(ThreeD@LookupGraph):
      ld    hl,ThreeD@Strings@GraphName
      rst   20h
      bcall _ChkFindSym
      ret   nc
      bjump _ErrUndefined

function(ThreeD@LookupEntry):
      ld    hl,ThreeD@Strings@EntryName
      rst   20h
      bcall _ChkFindSym
      ret

function(ThreeD@Quit):
      res   textWrite,(iy + sGrFlags)
      xor   a
      ld    (iy + textFlags),a
      ld    a,(SaveMode)
      ld    (flags + grfModeFlags),a
      call  ThreeD@DeleteEntry
      call  ThreeD@LookupGraph
      ld    hl,flags + graphFlags
      ld    a,(hl)
      set   graphDraw,(hl)
      set   smartGraph_inv,(iy + smartFlags)
      rrca
      and   %10000000
      ld    hl,_Flags
      or    (hl)
      ld    (hl),a
      inc   de
      inc   de
      ld    bc,94
      ldir
      ld    hl,_XYZTable
      ld    bc,RES*RES
@SaveGrid:
      inc   hl
      inc   hl
      ldi
      jp    pe,@SaveGrid

      ld    a,(SaveX1TMode)
      ld    ($9780),a   ;(y0LineType + 1)
      ld    hl,saveStuff - 8
      ld    de,$9BC0    ;menuHookPtr
      ldi \ ldi \ ldi
      ld    de,flags + $36
      ldi
      ld    de,rawKeyHookPtr
      ldi \ ldi \ ldi
      ld    de,flags + $34
      ldi
      ret

JumpHL:
      jp    (hl)

function(ThreeD@VPutsCenter):
      ld    b,0
      ld    de,OP1+1
@Loop:
      ld    a,(hl)
      ld    (de),a
      or    a
      jr    z,@Done
      inc   hl
      inc   de
      inc   b
      jr    @Loop
@Done:
      ld    hl,OP1
      ld    (hl),b
      bcall _SStringLength
      inc   hl
      ld    a,48
      srl   b
      sub   b
      ld    (penCol),a
      bcall _VPutS
      ret

function(ThreeD@MessageBox):
      ld    a,(flags + sGrFlags)
      push  af
      push  de
      push  hl
      res   textWrite,(iy + sGrFlags)
      call  ThreeD@DialogBox
      pop   hl
      ld    a,19
      ld    (penRow),a
      call  ThreeD@VPutsCenter
      pop   hl
      ld    a,25
      ld    (penRow),a
      call  ThreeD@VPutsCenter
      ld    a,36
      ld    (penRow),a
      ld    hl,ThreeD@Strings@AnyKey
      call  ThreeD@VPutsCenter
      pop   af
      ld    (flags + sGrFlags),a
      bcall _RunIndicOff
      bcall _GetKey
      ret

function(ThreeD@DialogBox):
      ld    hl,saveSScreen
      push  hl
      bcall _SaveDisp

      ld    hl,saveSScreen + 192
      call  @Horiz
      inc   hl
      ld    c,28
@BigLoop:
      ld    a,(hl)
      and   %11111100
      or    %00000010
      ld    (hl),a
      inc   hl
      xor   a
      ld    b,10
@Loop2:
      ld    (hl),a
      inc   hl
      djnz  @Loop2
      ld    a,(hl)
      or    %11000000
      ld    (hl),a
      inc   hl
      dec   c
      jr    nz,@BigLoop
      call  @Horiz
      ld    a,(hl)
      or    %11000000
      ld    (hl),a
      inc   hl
      call  @Horiz2
      set   7,(hl)
      ld    hl,saveSScreen + 192 + 12 + 11
      res   6,(hl)

      ld    b,64
      pop   hl
      bcall _RestoreDisp
      ret

@Horiz:
      set   0,(hl)
@Horiz2:
      inc   hl
      ld    b,10
      ld    a,$FF
@Loop1:
      ld    (hl),a
      inc   hl
      djnz  @Loop1
      ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

modeTemp    = appBackupScreen + 600
                              ; 742-749 contains data from saveStuff.. i was stupid
saveStuff   = appBackupScreen + 750
tempSP      = appBackupScreen + 766

function(ThreeD@AppChangeHook):
      ld    d,a
      call  ThreeD@CheckGraphMode
      jp    z,@Done

      ld    a,b
      or    a
      jp    z,@Done
      add   a,a
      cp    kYequ*2
      jr    nz,@NotYequ
      push  hl
      push  de
      push  bc
      call  ThreeD@LookupEntry
      ex    de,hl
      ld    a,(hl)
      inc   hl
      ld    h,(hl)
      ld    l,a
      push  hl
      call  ThreeD@LookupGraph
      ld    h,d
      ld    l,e
      ld    c,(hl)
      inc   hl
      ld    b,(hl)
      ld    hl,-(96 + (RES*RES))
      add   hl,bc
      ld    b,h
      ld    c,l
      pop   hl
      push  de
      or    a
      sbc   hl,bc
      jr    c,@BigEnough
      jr    z,@BigEnough
      push  de
      bcall _ErrNotEnoughMem
      ex    de,hl
      pop   de
      push  de
      push  hl
      ex    de,hl
      ld    bc,96 + (RES*RES)
      add   hl,bc
      ex    de,hl
      pop   hl
      push  hl
      bcall _InsertMem
      pop   bc
      pop   hl
      push  hl
      ld    a,(hl)
      inc   hl
      ld    h,(hl)
      ld    l,a
      add   hl,bc
      ex    de,hl
      pop   hl
      ld    (hl),e
      inc   hl
      ld    (hl),d
@BigEnough:
      pop   de
      ld    hl,96 + (RES*RES)
      add   hl,de
      ex    de,hl
      push  de
      call  ThreeD@LookupEntry
      ld    a,(hl)
      ex    de,hl
      pop   de
      push  af
      ld    c,(hl)
      inc   hl
      ld    b,(hl)
      dec   hl
      inc   bc
      inc   bc
      ldir
      ld    a,($9780)
      and   %00000010
      ld    b,a
      pop   af
      rlca
      rlca
      rlca
      and   %00000001
      ld    c,a
      ld    hl,_Flags
      ld    a,(hl)
      and   %11111100
      or    b
      or    c
      ld    (hl),a
      pop   bc
      pop   de
      pop   hl
@NotYequ:
      ld    a,b
      add   a,a
      jr    c,@Quit

      ld    a,(cxCurApp)
      cp    kYequ
      jr    z,@Done
      cp    kWindow
      jr    z,@Done
      cp    kGraph
      jr    z,@Done
      cp    kError
      jr    z,@Done
      cp    kFormat
      jp    z,ThreeD@ModeHook
@Quit:
      push  hl
      push  de
      push  bc
      call  ThreeD@Quit
      pop   bc
      pop   de
      pop   hl
@Done:
      ld    a,d
      ret

function(ThreeD@YeditHook):
      cp    6
      jp    nz,@NotKey
      ld    a,b
      cp    kApp
      ld    a,6
      jp    nz,@NotKey
      call  ThreeD@CheckGraphMode
      ld    a,kYequ | $80
      jp    nz,@ResetView

      ld    de,saveStuff - 8
      ld    hl,$9BC0    ;menuHookPtr
      ldi \ ldi \ ldi
      ld    hl,flags + $36
      ldi
      ld    hl,rawKeyHookPtr
      ldi \ ldi \ ldi
      ld    hl,flags + $34
      ldi
      in    a,(6)
      ld    hl,MenuHook
      bcall _SetMenuHook
      ld    hl,ThreeD@KeyHook
      bcall _SetGetKeyHook
      res   0,(iy + $34)

      res   graphDraw,(iy + graphFlags)   ; we have our own graphDraw flag
      res   smartGraph_inv,(iy + smartFlags)
      set   textWrite,(iy + sGrFlags)
      push  hl
      bcall _CloseEditEqu
      ld    hl,ThreeD@Strings@GraphName
      rst   20h
      bcall _ChkFindSym
      jr    nc,@Exists
      ld    hl,ThreeD@Strings@AppvarInitSize
      bcall _CreateAppVar
      inc   de
      inc   de
      ld    hl,ThreeD@Strings@AppvarInit
      ld    bc,ThreeD@Strings@AppvarInitSize
      ldir
@Exists:
      ld    a,b
      or    a
      jr    z,@NotArchived
      bcall _Arc_Unarc
@NotArchived:
      call  ThreeD@LookupGraph
      ex    de,hl
      inc   hl
      inc   hl
      ld    de,_Flags
      ld    bc,94
      ldir
      ld    de,_XYZTable
      ld    bc,RES*RES
@SaveGrid:
      inc   de
      inc   de
      ldi
      jp    pe,@SaveGrid
      call  ThreeD@CreateTemp
      pop   hl

      ld    a,($9780)   ;(y0LineType + 1)
      ld    (SaveX1TMode),a
      xor   a
      ld    hl,_Flags
      bit   1,(hl)
      jr    z,@NotFilled
      ld    a,3
@NotFilled:
      ld    ($9780),a

      ld    de,flags+grfModeFlags
      ld    a,(de)
      ld    (SaveMode),a
      and   %00001111
      ld    (de),a
      ld    a,kYequ + 1 ; dirty to force redraw
@ResetView:
      ld    sp,(onSP)
      res   6,(iy + curFlags)
      ld    (cxCurApp),a
      ld    a,kYequ
      bcall _newContext0
      xor   a
      bjump _SendKPress

@NotKey:
      call  ThreeD@CheckGraphMode
      ret   z
      or    a
      jr    nz,@Not0
      ld    a,tY0+1     ; our special custom equation
      ld    ($9654),a
      xor   a
      ret
@Not0:

      dec   a
      ret   z
      dec   a
      jr    nz,@Not2
      ld    hl,$1A0B    ;custom equation, and already maxed on the viewing of it
      ld    (eqs + 6),hl
      ld    b,h
      ld    a,(kbdGetKy)
      cp    skClear
      jr    nz,@NotClear
      ld    a,3
      ld    ($9780),a
      xor   a
      ret
@NotClear:
      ld    a,($9780)   ;(y0LineType + 1)
      or    a
      ret   z
      cp    3
      ret   z
      ld    a,3
      jr    c,@FixStyle
      xor   a
@FixStyle:
      ld    ($9780),a
      xor   a
      ret
@Not2:
      dec   a
      jr    nz,@Not3
      ld    a,(winTop)
      dec   a
      ld    (curRow),a
      ld    hl,ThreeD@Strings@Graph3
      call  PutsApp
      bcall _homeUp
      or    $FF
      ret
@Not3:
      dec   a
      ret   z
      dec   a
      ret   z
      dec   a
      jr    nz,@Not6
      ld    a,b
      cp    kLastEnt
      jr    z,@Disallow
      cp    kUp
      jr    nz,@Allow
      ld    hl,(editCursor)
      ld    de,(editTop)
      or    a
      sbc   hl,de
      jr    nz,@Allow
@Disallow:
      or    $FF
      ret
@Not6:
      dec   a
      ret   z
      dec   a
      jr    nz,@Not8
      ld    a,(curCol)
      sub   3
      ld    (curCol),a
      ld    a,'Z'
      bcall _PutC
      ld    a,$81
      bcall _PutC
;     ld    hl,ThreeD@Strings@EntryName
;     rst   20h
      xor   a
      ret
@Not8:
@Allow:
      xor   a
      ret

function(ThreeD@WindowHook):
      push  af
      ld    a,(cxCurApp)
      cp    kWindow
      jr    z,@Possible
      pop   af
      xor   a
      ret
@Possible:
      pop   af
      call  ThreeD@CheckGraphMode
      ret   z
      or    a
      jr    nz,@Not0
      or    $FF
      ld    a,(ix)
      ret
@Not0:
      dec   a
      jr    nz,@Not1
      or    $FF
      ld    a,(ix+3)
      ret
@Not1:
      dec   a
      jr    nz,@Not2
      or    $FF
      ld    a,(ix-3)
      ret
@Not2:
      dec   a
      jr    nz,@Not3
      ld    l,(ix+1)
      ld    h,(ix+2)
      call  PutsApp
      ld    a,'='
      bcall _PutC
      bcall _SetNumWindow
      jr    @DispNum
@Not3:
      dec   a
      ret   z
      dec   a
      jr    nz,@Not5
      ld    d,0
      ld    e,(hl)
      ld    hl,_xMin
      add   hl,de
      rst   20h
      ld    hl,OP1
      or    $FF
      ret
@Not5:
      dec   a
      jr    nz,@Not6
      set   graphDraw,(iy + graphFlags)
      ld    d,0
      ld    e,(hl)
      ld    hl,_xMin
      add   hl,de
      ex    de,hl
      bcall _MovFrOP1
      ld    hl,OP1
      or    $FF
      ret
@Not6:
      dec   a
      jr    nz,@Not7
      ld    hl,@Table + 3
@Disallow:
      or    $FF
      ret
@Not7:
      dec   a
      ret   z
      dec   a
      jr    nz,@Not9
@DispNum:
      ld    hl,_xMin
      ld    d,0
      ld    e,(ix)
      add   hl,de
      rst   20h
      ld    a,$15
      bcall _FormEReal
      ld    hl,OP3
      bcall _wputsEOL
      or    $FF
      ret
@Not9:
      dec   a
      jr    nz,@Not10
      ld    a,b
      cp    kLastEnt
      jr    z,@Disallow
@Not10:
@Allow:
      xor   a
      ret
@Table:
      .db   $FF
      .dw   0
      .db   0 * 9
      .dw   @Xmin
      .db   1 * 9
      .dw   @Xmax
      .db   2 * 9
      .dw   @Ymin
      .db   3 * 9
      .dw   @Ymax
      .db   4 * 9
      .dw   @Zmin
      .db   5 * 9
      .dw   @Zmax
      .db   $FF
@Xmin .db "Xmin",0
@Xmax .db "Xmax",0
@Ymin .db "Ymin",0
@Ymax .db "Ymax",0
@Zmin .db "Zmin",0
@Zmax .db "Zmax",0

function(ThreeD@CheckGraphMode):
      ld    e,a
      ld    a,(flags + grfModeFlags)
      and   %11110000
      jr    z,@Z
      xor   a
      ld    a,e
      ret
@Z    or    $FF
      ld    a,e
      ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function(ThreeD@ModeHook):
      ld    hl,$97A2    ; current selection
      ld    (hl),0

      ld    de,saveStuff
      ld    hl,curRow
      ldi \ ldi
      ld    hl,flags + textFlags
      ldi
      ld    hl,cursorHookPtr
      ldi \ ldi \ ldi
      ld    hl,flags + $34
      ldi

      ld    sp,(onSp)
      ld    hl,@gmMonVectors
      bcall _AppInit

      set   saIndic,(iy + newIndicFlags)
      in    a,(6)
      ld    hl,CursorHook
      bcall _SetCursorHook

      ld    a,cxextapps
      ld    (cxCurApp),a

      call  @gmcxRedisp@ForSure
      bjump _Mon

@BlinkItem:
      ld    b,(hl)
      ld    hl,curCol
      ld    a,(hl)
      or    a
      jr    z,@BlinkSkip
      dec   b
      inc   (hl)
@BlinkSkip:
      set   textInverse,(iy + textFlags)
@BlinkLoop:
      ld    a,' '
      bcall _PutC
      djnz  @BlinkLoop
      res   textInverse,(iy + textFlags)
      ret

@DrawItemX:
      ld    e,(hl)
      inc   hl
      ld    d,(hl)
      ld    (curRow),de
@DrawItem:
      bit   1,(ix)
      jr    nz,@NotLeft
      xor   a
      ld    (curCol),a
@NotLeft:
      ld    a,(curCol)
      or    a
      jr    z,@DrawNoSpace
      push  ix
      ld    a,' '
      bcall _PutC
      pop   ix
@DrawNoSpace:
      ld    l,(ix+3)
      ld    h,(ix+4)
      ld    a,(ix+5)
      and   (hl)
      cp    (ix+6)
      jr    nz,@NotSel
      set   textInverse,(iy + textFlags)
@NotSel:
      ld    l,(ix+1)
      ld    h,(ix+2)
      ld    (OP1),hl
      ld    hl,OP1
      push  ix
      bcall _Get_Tok_Strng
      ld    hl,OP3
@PutsLoop:
      ld    a,(hl)
      inc   hl
      or    a
      jr    z,@PutsDone
      push  hl
      bcall _PutMap
      pop   hl
      ld    a,(curCol)
      inc   a
      ld    (curCol),a
      jr    @PutsLoop
@PutsDone:
      pop   ix
      bit   0,(ix)
      jr    nz,@NotRight
      ld    a,(curRow)
      inc   a
      ld    (curRow),a
@NotRight:
      res   textInverse,(iy + textFlags)
      ret

@Table:
.db   %00000101,  tGFormat,   tRectG,     \ .dw flags + grfDBFlags      \ .db %00001000,%00000000     ,0
.db   %00000110,  tGFormat,   tPolarG,    \ .dw flags + grfDBFlags      \ .db %00001000,%00001000     ,0
.db   %00001101,  tGFormat,   tCoordOn,   \ .dw _Flags                  \ .db %00010000,%00000000     ,0
.db   %00001110,  tGFormat,   tCoordOff,  \ .dw _Flags                  \ .db %00010000,%00010000     ,0
.db   %00001101,  tGFormat,   tAxisOff,   \ .dw _Flags                  \ .db %00100000,%00100000     ,0
.db   %00001110,  tGFormat,   tAxisOn,    \ .dw _Flags                  \ .db %00100000,%00000000     ,0
.db   %00001001,  tGFormat,   tLblOff,    \ .dw _Flags                  \ .db %01000000,%00000000     ,0
.db   %00001010,  tGFormat,   tLblOn,     \ .dw _Flags                  \ .db %01000000,%01000000     ,0

@gmMonVectors:
.dw @gmcxMain,@gmcxDummyRet,@gmcxPutAway
.dw @gmcxRedisp,@gmcxDummyRet,@gmcxDummyRet
.db 0

@Quit:
      bcall _POPCX
@Quit@NoPop:
      ld    a,cxFormat
      ld    (cxCurApp),a

      ld    hl,saveStuff
      ld    de,curRow
      ldi \ ldi
      ld    de,flags + textFlags
      ldi
      ld    de,cursorHookPtr
      ldi \ ldi \ ldi
      ld    de,flags + $34
      ldi
      res   saIndic,(iy + newIndicFlags)
@gmcxDummyRet:
      ret

@gmcxPutAway:
      call  @Quit
      bit   monAbandon,(iy + monFlags)
      jr    nz,@Off
      ld    a,iall
      out   (intrptEnPort),a
      bcall _LCD_DRIVERON
      set   onRunning,(iy + onFlags)
      ei
@Off:
      bjump _cxPutAway

@gmcxRedisp:
      ld    a,(kbdKey)
      cp    kClear
      ret   nz
@gmcxRedisp@ForSure:
      bcall _ClrScrnFull
      xor   a
      ld    (curRow),a

      ld    ix,@Table
      ld    hl,modeTemp
@BigLoop:
      push  hl
      ld    hl,(curRow)
      push  hl
      call  @DrawItem
      pop   de
      pop   hl
      bit   1,(ix)
      jr    nz,@NotLeftX
      ld    d,0
@NotLeftX:
      ld    (hl),e
      inc   hl
      ld    (hl),d
      inc   hl
      ld    a,(curCol)
      sub   d
      ld    (hl),a
      inc   hl
      ld    a,(ix)
      and   %00000101
      ld    de,8
      add   ix,de
      jr    nz,@BigLoop
      ret

@gmcxMain:
      cp    kExtApps
      jr    nz,@NotApps
      ld    a,kQuit
      ;call  @Quit
      ;ld    hl,progCurrent
      ;ld    de,progToEdit
      ;ld    bc,8
      ;ldir
      ;bjump $4C51       ;execute app
@NotApps:

      cp    kQuit
      jr    c,@NotExternal
      cp    kTrace
      jr    z,@GraphKey
      jr    nc,@NotExternal
      cp    kGraph
      jr    z,@GraphKey
      ld    hl,$97A2    ; current selection
      ld    (hl),0
      cp    kFormat
      jp    z,@gmcxRedisp@ForSure
      push  af
      push  bc
      call  @Quit
      pop   bc
      pop   af
      ld    sp,(onSP)
      res   6,(iy + curFlags)
      bcall _newContext0
      xor   a
      bjump _SendKPress
@GraphKey:
      bjump _JForceGraphKey
@NotExternal:

      cp    kLeft
      jr    nz,@NotkLeft
      call  @Lookup
      bit   1,(ix)
      ret   z
      call  @DrawItemX
      ld    hl,$97A2
      dec   (hl)
@Disallow:
      xor   a
      ret
@NotkLeft:

      cp    kRight
      jr    nz,@NotkRight
      call  @Lookup
      bit   0,(ix)
      ret   z
      call  @DrawItemX
      ld    hl,$97A2
      inc   (hl)
      xor   a
      ret
@NotkRight:

      cp    kUp
      jr    nz,@NotkUp
      call  @Lookup
      bit   3,(ix)
      ret   z
      call  @DrawItemX
      ld    hl,$97A2
      ld    de,-8
@FindUp1:
      bit   1,(ix)
      jr    z,@FindUp2
      dec   (hl)
      add   ix,de
      jr    @FindUp1
@FindUp2:
      dec   (hl)
      add   ix,de
      bit   1,(ix)
      jr    nz,@FindUp2
      ret
@NotkUp:

      cp    kDown
      jr    nz,@NotkDown
      call  @Lookup
      bit   2,(ix)
      ret   z
      call  @DrawItemX
      ld    hl,$97A2
@FindDown1M1:
      ld    de,8
@FindDown1:
      inc   (hl)
      add   ix,de
      bit   1,(ix)
      jr    nz,@FindDown1
      ret
@NotkDown:

      cp    kClear
      jr    nz,@NotClear
      call  @Quit
      bjump _JForceCmdNoChar
@NotClear:

      cp    kEnter
      jp    nz,@NotkEnter
      call  @Lookup
      ex    de,hl
      ld    l,(ix+3)
      ld    h,(ix+4)
      ld    a,(ix+5)
      and   (hl)
      cp    (ix+6)
      ret   z
      ld    a,(ix+5)
      cpl
      and   (hl)
      or    (ix+6)
      ld    c,(hl)
      ld    (hl),a
      ld    a,(de)
      ld    h,0
      ld    l,a
      ld    (curRow),hl
      ld    de,-8
@FindUp3:
      bit   1,(ix)
      jr    z,@RedrawRow
      add   ix,de
      jr    @FindUp3
@RedrawRow:
      call  @DrawItem
      bit   0,(ix)
      ld    de,8
      add   ix,de
      jr    nz,@RedrawRow
      ret
@NotkEnter:

      cp    kEOL + 1
      jr    nc,@BadKey
@RetAllow:
      xor   a
      ret

@BadKey:
      push  af
      call  @Quit
      pop   af
      bjump _JForceCmd

CursorHook:
      .db   $83
      push  af
      call  @Lookup
      ld    e,(hl)
      inc   hl
      ld    d,(hl)
      inc   hl
      ld    (curRow),de
      pop   af
      cp    $24
      jr    c,@NZ
      call  @DrawItem
      jr    @Z
@NZ   call  @BlinkItem
@Z    call  @HandleIndic
      xor   a
      ret

@HandleIndic:
      bit   saIndic,(iy + newIndicFlags)
      jr    z,@UpdateIndic
      call  @GetIndicChar
      jr    nz,@UpdateIndicGo
      bcall _restoreTR
      res   saIndic,(iy + newIndicFlags)
      ret
@UpdateIndic:
      call  @GetIndicChar
      ret   z
      bcall _saveTR
      set   saIndic,(iy + newIndicFlags)
@UpdateIndicGo:
      ld    hl,(curRow)
      push  hl
      ld    hl,$0F00
      ld    (curRow),hl
      call  @GetIndicChar
      bcall _PutMap
      pop   hl
      ld    (curRow),hl
      ret

@GetIndicChar:
      ld    a,$E1
      ld    h,(iy + shiftFlags)
      bit   3,h
      ret   nz
      bit   4,h
      ret   z
      add   a,2
      bit   5,h
      ret   nz
      dec   a
      ret

@Lookup:
      ld    a,($97A2)   ; cur sel
      ld    c,a
      add   a,a
      add   a,c
      ld    d,0
      ld    e,a
      ld    hl,modeTemp
      add   hl,de
      add   a,c
      add   a,a
      ld    e,a
      ld    ix,@Table
      add   ix,de
      ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function(ThreeD@KeyHook):
      .db   $83
      cp    kSPlot
      jr    z,@NotIn3D
      cp    kTblSet
      jr    z,@NotIn3D
      cp    kTable
      jr    z,@NotIn3D
      cp    kCalc
      jr    z,@NotIn3D
      cp    kGraph
      jr    nz,@NotGraph
      ld    a,(cxCurApp)
      cp    kGraph
      ld    a,kGraph
      jr    nz,@NotGraph
      bit   0,(iy + asm_Flag1)
      jr    nz,@NotGraph
      ld    a,kColon
@NotGraph:
      ld    e,a
      ld    a,(kbdGetKy)
      cp    skGraphVar
      jr    nz,@NotXVar
      ld    a,kCapX
      ret
@NotXVar:
      ld    a,e
      cp    kStat
      jr    nz,@NotStat
      ld    a,kCapY
      ret
@NotStat:
      cp    kClear
      jr    nz,@NotClear
      ld    a,(menuCurrent)
      or    a
      ld    a,kClear
      jr    nz,@NotClear
      ld    a,(cxCurApp)
      cp    kGraph
      ld    a,kClear
      jr    nz,@NotClear
      ld    a,kGraph
      bit   0,(iy + asm_Flag1)
      jr    nz,@NotClear
      ld    a,kQuit
@NotClear:
      cp    kZoom
      jr    nz,@NotZoom
      ld    a,(cxCurApp)
      cp    kGraph
      ld    a,kZoom
      jr    nz,@NotZoom
      bit   0,(iy + asm_Flag1)
      jr    nz,@NotZoom
      ld    a,(menuCurrent)
      cp    4     ;is it zoom menu already?
      ld    a,kZoom
      jr    nz,@NotZoom
      ld    a,kAreYouSure
@NotZoom:
      ret

@NotIn3D:
      bcall _DelRes
      bcall _SaveOScreen
      ld    hl,saveSScreen + 16*12
      ld    de,statVars
      ld    bc,31*12
      ldir

      ld    hl,ThreeD@Strings@NotAvailable1
      ld    de,ThreeD@Strings@NotAvailable2
      call  ThreeD@MessageBox
      cp    kQuit
      ret   z

      ld    hl,statVars
      ld    de,saveSScreen + 16*12
      ld    bc,31*12
      ldir
      bcall _RstrOScreen
      xor   a
      ret

function(MenuHook):
      .db   $83
      or    a
      jr    nz,@Not0
      ld    a,(menuCurrent)
      dec   a           ; is it our custom platypi menu?
      jr    z,@Platypi
      cp    $04 - 1     ; check for zoom menu (- 1 on account of dec a)
      jr    nz,@Not0
      ld    hl,ThreeD@Strings@ZoomMenu
      ld    de,tempSwapArea
      push  de
      call  Mov9
      pop   hl
      or    $FF
      ret
@Not0:
      cp    2
      jr    nz,@Not2
      ld    a,(menuCurrent)
      dec   a
      jr    nz,@Not2
      ld    hl,ThreeD@Strings@Platypi
      ld    de,OP1
      push  de
      ld    bc,17
      ldir
      pop   hl
      bcall _PutS
      or    $FF
      ret
@Not2:
      xor   a
      ret

@Platypi:
      ld    hl,ThreeD@Strings@PlatypiMenu
      ld    de,tempSwapArea
      push  de
      call  Mov9
      pop   hl
      or    $FF
      ret
