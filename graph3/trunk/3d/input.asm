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
      jp LookupAppVar

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

      xor   a
      ld    (YEditHookState),a

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

modeTemp    = appBackupScreen + 600
                              ; 742-749 contains data from saveStuff.. i was stupid
saveStuff   = appBackupScreen + 750
tempSP      = appBackupScreen + 766

function(ThreeD@AppChangeHook):
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
      jp    z,ModeHook
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

function(ThreeD@Installation):
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

      ld    a,ThreeDState
      ld    (YEditHookState),a

      ld    a,kYequ + 1 ; dirty to force redraw
      ret


function(ThreeD@YeditHook):
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
      call  MessageBox
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
