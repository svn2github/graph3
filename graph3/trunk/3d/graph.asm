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

; Good rotation angle is 5.6288 degrees
;  sin(theta) ~  1607/16384   \_______________ both of these values are
;   00011001000111(00)         _______________ *very* close approximations.
;  cos(theta) ~ 16305/16384   /                this is a rare find.
;   11111110110001(00)

; Note: Before use, you must either do LD A,D or LD D,A depending what you want to do
;#define MultByE add a,e\ jp po,$+9\ jp m,$+11\ jp $+6\ jp p,$+5\ neg\ ld h,MulTable>>8\ ld l,a\ ld a,d\ sub e\ jp po,$+9\ jp m,$+11\ jp $+6\ jp p,$+5\ neg\ ld d,(hl)\ inc h\ ld e,(hl)\ ld l,a\ ld a,e\ sub (hl)\ ld e,a\ dec h\ ld a,d\ sbc a,(hl)\ ld d,a
#define MultByE add a,e\ ld h,MulTable>>8\ ld l,a\ ld a,d\ sub e\ ld d,(hl)\ inc h\ ld e,(hl)\ ld l,a\ ld a,e\ sub (hl)\ ld e,a\ dec h\ ld a,d\ sbc a,(hl)\ ld d,a

function(ThreeD@Graph@cxMain):
      cp    kColon
      jr    nz,@NotMode
      ld    hl,_Flags
      ld    a,(hl)
      xor   %00000010
      ld    (hl),a
      jp    @Redraw
@NotMode:
      cp    kCapX
      jr    nz,@NotX
      ld    hl,ThreeD@Strings@ViewXAxis
      ld    de,_RotXX
      ld    bc,18
      ldir
      jp    @Redraw
@NotX:
      cp    kCapY
      jr    nz,@NotY
      ld    hl,ThreeD@Strings@ViewYAxis
      ld    de,_RotXX
      ld    bc,18
      ldir
      jp    @Redraw
@NotY:
      cp    kCapZ
      jr    nz,@NotZ
      ld    hl,ThreeD@Strings@ViewZAxis
      ld    de,_RotXX
      ld    bc,18
      ldir
      jp    @Redraw
@NotZ:

      bit   0,(iy + asm_Flag1)
      jp    z,@NormalGraph
      cp    kLeft
      jr    nz,@TraceNotLeft
      ld    hl,_X
      rst   20h
      ld    hl,_xMin
      call  Mov9ToOP2
      bcall _FPSub
      ld    hl,_deltaX
      call  Mov9ToOP2
      bcall _FPDiv
      bcall _RndGuard
      bcall _Ceiling
      bcall _Minus1
      ld    hl,_deltaX
      call  Mov9ToOP2
      bcall _FPMult
      ld    a,(OP1)
      add   a,a
      jr    c,@TraceNotLeft
      ld    hl,_xMin
      call  Mov9ToOP2
      rst   30h   ;op1 += op2
      ld    hl,OP1
      ld    de,_X
      call  Mov9
      jp    @Redraw
@TraceNotLeft:
      cp    kRight
      jr    nz,@TraceNotRight
      ld    hl,_X
      rst   20h
      ld    hl,_xMin
      call  Mov9ToOP2
      bcall _FPSub
      ld    hl,_deltaX
      call  Mov9ToOP2
      bcall _FPDiv
      bcall _RndGuard
      bcall _Intgr
      bcall _Plus1
      ld    hl,_deltaX
      call  Mov9ToOP2
      bcall _FPMult
      ld    hl,_xMin
      call  Mov9ToOP2
      rst   30h   ;op1 += op2
      ld    hl,_xMax
      call  Mov9ToOP2
      bcall _CpOP1OP2
      jr    z,$+4
      jr    nc,@TraceNotRight
      ld    hl,OP1
      ld    de,_X
      call  Mov9
      jp    @Redraw
@TraceNotRight:
      cp    kDown
      jr    nz,@TraceNotUp
      ld    hl,_Y
      rst   20h
      ld    hl,_yMin
      call  Mov9ToOP2
      bcall _FPSub
      ld    hl,_deltaY
      call  Mov9ToOP2
      bcall _FPDiv
      bcall _RndGuard
      bcall _Ceiling
      bcall _Minus1
      ld    hl,_deltaY
      call  Mov9ToOP2
      bcall _FPMult
      ld    a,(OP1)
      add   a,a
      jr    c,@TraceNotUp
      ld    hl,_yMin
      call  Mov9ToOP2
      rst   30h   ;op1 += op2
      ld    hl,OP1
      ld    de,_Y
      call  Mov9
      jp    @Redraw
@TraceNotUp:
      cp    kUp
      jr    nz,@TraceNotDown
      ld    hl,_Y
      rst   20h
      ld    hl,_yMin
      call  Mov9ToOP2
      bcall _FPSub
      ld    hl,_deltaY
      call  Mov9ToOP2
      bcall _FPDiv
      bcall _RndGuard
      bcall _Intgr
      bcall _Plus1
      ld    hl,_deltaY
      call  Mov9ToOP2
      bcall _FPMult
      ld    hl,_yMin
      call  Mov9ToOP2
      rst   30h   ;op1 += op2
      ld    hl,_yMax
      call  Mov9ToOP2
      bcall _CpOP1OP2
      jr    z,$+4
      jr    nc,@TraceNotDown
      ld    hl,OP1
      ld    de,_Y
      call  Mov9
      jp    @Redraw
@TraceNotDown:
      cp    kEnter
      jp    nz,@TraceNotEnter
      ld    hl,_xMin
      call  Mov9ToOP2
      rst   20h
      bcall _FPSub
      call  @FPMultDiv
      bcall _OP1ToOP4
      ld    hl,_yMin
      call  Mov9ToOP2
      rst   20h
      bcall _FPSub
      call  @FPMultDiv
      bcall _OP1ToOP5
      ld    hl,_zMin
      call  Mov9ToOP2
      rst   20h
      bcall _FPSub
      call  @FPMultDiv
      bcall _OP1ToOP6

      bcall _OP2Set2
      bcall _FPDiv
      ld    hl,_Z
      call  Mov9ToOP2
      rst   30h   ;OP1 += OP2
      ld    hl,OP1
      ld    de,_zMax
      call  Mov9
      bcall _OP6ToOP2
      bcall _FPSub
      ld    hl,OP1
      ld    de,_zMin
      call  Mov9

      bcall _OP5ToOP1
      bcall _OP2Set2
      bcall _FPDiv
      ld    hl,_Y
      call  Mov9ToOP2
      rst   30h   ;OP1 += OP2
      ld    hl,OP1
      ld    de,_yMax
      call  Mov9
      bcall _OP5ToOP2
      bcall _FPSub
      ld    hl,OP1
      ld    de,_yMin
      call  Mov9

      bcall _OP4ToOP1
      bcall _OP2Set2
      bcall _FPDiv
      ld    hl,_X
      call  Mov9ToOP2
      rst   30h   ;OP1 += OP2
      ld    hl,OP1
      ld    de,_xMax
      call  Mov9
      bcall _OP4ToOP2
      bcall _FPSub
      ld    hl,OP1
      ld    de,_xMin
      call  Mov9
      set   graphDraw,(iy + graphFlags)
      call  ThreeD@DoGraph
      or    $FF
      ret

@TraceNotEnter:
      xor   a
      ret

@FPMultDiv:
      bit   1,(iy + asm_Flag1)      ;is zooming?
      ret   z
      bit   2,(iy + asm_Flag1)
      jr    z,@FPMultDivZoomIn
      res   0,(iy + asm_Flag1)
      res   2,(iy + graphFlags)     ;disable cursor
      xor   a
      ld    (curGStyle),a
;     bcall _OP2Set2
      bcall _Times2
      ret
@FPMultDivZoomIn:
      res   0,(iy + asm_Flag1)
      res   2,(iy + graphFlags)     ;disable cursor
      xor   a
      ld    (curGStyle),a
      bcall _OP2Set2
      bcall _FPDiv
      ret

@NormalGraph:
      cp    kUp
      jr    nz,@NotUp
      ld    de,(_RotYX)
      ld    hl,(_RotZX)
      call  RotateNegative
      ld    (_RotYX),de
      ld    (_RotZX),hl
      ld    de,(_RotYY)
      ld    hl,(_RotZY)
      call  RotateNegative
      ld    (_RotYY),de
      ld    (_RotZY),hl
      ld    de,(_RotYZ)
      ld    hl,(_RotZZ)
      call  RotateNegative
      ld    (_RotYZ),de
      ld    (_RotZZ),hl
      jr    @Redraw
@NotUp:
      cp    kDown
      jr    nz,@NotDown
      ld    de,(_RotYX)
      ld    hl,(_RotZX)
      call  RotatePositive
      ld    (_RotYX),de
      ld    (_RotZX),hl
      ld    de,(_RotYY)
      ld    hl,(_RotZY)
      call  RotatePositive
      ld    (_RotYY),de
      ld    (_RotZY),hl
      ld    de,(_RotYZ)
      ld    hl,(_RotZZ)
      call  RotatePositive
      ld    (_RotYZ),de
      ld    (_RotZZ),hl
@Redraw:
      call  ThreeD@DrawGraph
      or    $FF
      ret
@NotDown:
      cp    kLeft
      jr    nz,@NotLeft
      ld    de,(_RotXX)
      ld    hl,(_RotZX)
      call  RotateNegative
      ld    (_RotXX),de
      ld    (_RotZX),hl
      ld    de,(_RotXY)
      ld    hl,(_RotZY)
      call  RotateNegative
      ld    (_RotXY),de
      ld    (_RotZY),hl
      ld    de,(_RotXZ)
      ld    hl,(_RotZZ)
      call  RotateNegative
      ld    (_RotXZ),de
      ld    (_RotZZ),hl
      jr    @Redraw
@NotLeft:
      cp    kRight
      jr    nz,@NotRight
      ld    de,(_RotXX)
      ld    hl,(_RotZX)
      call  RotatePositive
      ld    (_RotXX),de
      ld    (_RotZX),hl
      ld    de,(_RotXY)
      ld    hl,(_RotZY)
      call  RotatePositive
      ld    (_RotXY),de
      ld    (_RotZY),hl
      ld    de,(_RotXZ)
      ld    hl,(_RotZZ)
      call  RotatePositive
      ld    (_RotXZ),de
      ld    (_RotZZ),hl
      jr    @Redraw
@NotRight:
      xor   a
      ret

FindPixel:
      ld    hl,FP_Bits
      ld    a,b
      and   $07         ; a = bit offset
      add   a,l
      ld    l,a
      adc   a,h
      sub   l
      ld    h,a
      ld    a,(hl)
      ld    h,0
      ld    l,c
      add   hl,hl
      add   hl,hl
      ld    d,h
      ld    e,l
      add   hl,hl
      add   hl,de
      ld    d,0
      ld    e,b
      srl   e
      srl   e
      srl   e
      add   hl,de
      ret

FP_Bits: .db $80,$40,$20,$10,$08,$04,$02,$01

function(ThreeD@DrawPoly):
      ld    (tempSP),sp
      ld    sp,_Poly1
      pop   de
      ld    b,d
      ld    c,e
      ld    h,1
      pop   de
      ld    a,c
      cp    e
      jr    c,@SortB
      jr    nz,@SortA
      ld    a,d
      cp    b
      jr    c,@SortB
@SortA:
      ld    b,d
      ld    c,e
      ld    h,2
@SortB:
      pop   de
      ld    a,c
      cp    e
      jr    c,@SortD
      jr    nz,@SortC
      ld    a,d
      cp    b
      jr    c,@SortD
@SortC:
      ld    b,d
      ld    c,e
      ld    h,3
@SortD:
      pop   de
      ld    a,c
      cp    e
      jr    c,@SortF
      jr    nz,@SortE
      ld    a,d
      cp    b
      jr    c,@SortF
@SortE:
      ld    b,d
      ld    c,e
      ld    h,4
@SortF:
      ld    sp,_Poly1
      dec   h
      jr    z,@Sort1
      dec   h
      jr    z,@Sort2
      dec   h
      jr    z,@Sort3
@Sort4:
      pop   ix
      pop   hl
      pop   de
      pop   bc
      push  de
      push  hl
      push  ix
      push  bc
      jp    @SortEnd
@Sort3:
      pop   ix
      pop   hl
      pop   de
      pop   bc
      push  hl
      push  ix
      push  bc
      push  de
      jp    @SortEnd
@Sort2:
      pop   ix
      pop   hl
      pop   de
      pop   bc
      push  ix
      push  bc
      push  de
      push  hl
      ;jp   @SortEnd
@Sort1:
@SortEnd:
      ld    sp,(tempSP)

      ld    a,(_Poly2+1)
      ld    hl,(_Poly1)
      sub   h
      ld    e,a
      ld    a,l
      ld    hl,_Poly4
      sub   (hl)
      ld    d,a
      MultByE
      push  de
      ld    a,(_Poly2)
      ld    hl,(_Poly1)
      sub   l
      ld    e,a
      ld    a,h
      ld    hl,_Poly4+1
      sub   (hl)
      ld    d,a
      MultByE
      pop   hl
      or    a
      sbc   hl,de
      jp    m,@IsClockwise
      ld    de,(_Poly2)
      ld    hl,(_Poly4)
      ld    (_poly2),hl
      ld    (_Poly4),de
@IsClockwise:
      push  af          ; save whether it's clockwise
      ld    a,(_XYZTable + 6)       ; for the platypus egg
      sub   -19
      jp    nz,@NotPlatypus
      pop   hl    ; ditch the old clockwise flag
      ld    a,(PlatypiFill)
      or    a
      push  af
@NotPlatypus:
      ld    bc,$0103
      ld    a,(_Poly3)
      ld    hl,_Poly4
      cp    (hl)
      jr    c,@EdgeCountTest
      ld    bc,$0301
      ld    hl,_Poly2
      dec   a
      cp    (hl)
      jr    c,@EdgeCountDone
      dec   b
      inc   c
      jr    @EdgeCountDone
@EdgeCountTest:
      ld    hl,_Poly2
      cp    (hl)
      jp    nc,@EdgeCountDone
      dec   c
      dec   c
@EdgeCountDone:
      ex    af,af'
      ld    a,c
      ex    af,af'
      push  bc

      ld    hl,_ScanBuffer + 128
      ld    de,(_Poly1)
      ld    bc,(_Poly2)
      call  ThreeD@ScanLine

      ex    af,af'
      dec   a
      jr    z,@RightDone
      ex    af,af'
      ld    e,(hl)
      dec   l
      ld    d,(hl)
      push  hl
      push  de
      ld    de,(_Poly2)
      ld    bc,(_Poly3)
      call  ThreeD@ScanLine
      pop   de
      ex    (sp),hl
      ld    b,(hl)
      inc   l
      ld    c,(hl)
      ld    a,d
      cp    b
      jr    nc,$+3
      ld    b,d
      ld    a,c
      cp    e
      jr    nc,$+3
      ld    (hl),e
      dec   l
      ld    (hl),b
      pop   hl

      ex    af,af'
      dec   a
      jr    z,@RightDone
      ex    af,af'
      ld    e,(hl)
      dec   l
      ld    d,(hl)
      push  hl
      push  de
      ld    de,(_Poly3)
      ld    bc,(_Poly4)
      call  ThreeD@ScanLine
      pop   de
      ex    (sp),hl
      ld    b,(hl)
      inc   l
      ld    c,(hl)
      ld    a,d
      cp    b
      jr    nc,$+3
      ld    b,d
      ld    a,c
      cp    e
      jr    nc,$+3
      ld    (hl),e
      dec   l
      ld    (hl),b
      pop   hl
@RightDone:
      pop   af
      ex    af,af'
      ld    a,l
      rrca
      and   %00111111
      inc   a
      push  af

      ld    hl,_ScanBuffer
      ld    de,(_Poly1)
      ld    bc,(_Poly4)
      call  ThreeD@ScanLine

      ex    af,af'
      dec   a
      jr    z,@LeftDone
      ex    af,af'
      ld    e,(hl)
      dec   l
      ld    d,(hl)
      push  hl
      push  de
      ld    de,(_Poly4)
      ld    bc,(_Poly3)
      call  ThreeD@ScanLine
      pop   de
      ex    (sp),hl
      ld    b,(hl)
      inc   l
      ld    c,(hl)
      ld    a,d
      cp    b
      jr    nc,$+3
      ld    b,d
      ld    a,c
      cp    e
      jr    nc,$+3
      ld    (hl),e
      dec   l
      ld    (hl),b
      pop   hl

      ex    af,af'
      dec   a
      jr    z,@LeftDone
      ex    af,af'
      ld    e,(hl)
      dec   l
      ld    d,(hl)
      push  hl
      push  de
      ld    de,(_Poly3)
      ld    bc,(_Poly2)
      call  ThreeD@ScanLine
      pop   de
      ex    (sp),hl
      ld    b,(hl)
      inc   l
      ld    c,(hl)
      ld    a,d
      cp    b
      jr    nc,$+3
      ld    b,d
      ld    a,c
      cp    e
      jr    nc,$+3
      ld    (hl),e
      dec   l
      ld    (hl),b
      pop   hl
@LeftDone:
      ld    d,BitTable >> 8
      srl   l
      inc   l
      pop   af
      cp    l
      jr    nc,@LeftRightCompare
      ld    l,a
@LeftRightCompare:
      exx
      ld    a,(_Poly1)
      add   a,a
      add   a,a
      ld    hl,plotSScreen
      ld    d,0
      ld    e,a
      add   hl,de
      add   hl,de
      add   hl,de
      ex    de,hl
      ld    bc,_ScanBuffer
      pop   af
      jp    m,ThreeD@FillLine1I
function(ThreeD@FillLine1):
      ld    a,(bc)
      inc   c
      exx
      ld    e,a
      exx
      rrca
      rrca
      rrca
      and   %00011111
      ld    h,0
      ld    l,a
      add   hl,de
      ld    a,(bc)
      dec   c
      exx
      ld    c,a
      ld    a,e
      or    7
      ld    b,a
      cp    c
      ld    a,(de)
      jr    c,@NotDone
      ld    b,a
      ld    e,c
      ld    a,(de)
      cpl
      and   b
      exx
      or    (hl)
      ld    (hl),a
      jp    ThreeD@FillLine2
@NotDone:
      exx
      or    (hl)
      ld    (hl),a
      inc   hl
      exx
@Loop:
      ld    a,b
      add   a,8
      cp    c
      jp    nc,@Done
      ld    b,a
      exx
      ld    (hl),$FF
      inc   hl
      exx
      jr    @Loop
@Done:
      ld    e,c
      ld    a,(de)
      cpl
      exx
      or    (hl)
      ld    (hl),a
function(ThreeD@FillLine2):
      set   7,c
      ld    a,(bc)
      inc   c
      exx
      cp    e
      jr    c,@SkipItAll
      ld    c,a
      ld    a,e
      or    7
      ld    b,a
      cp    c
      ld    a,(de)
      cpl
      jr    c,@NotDone
      ld    b,a
      ld    e,c
      ld    a,(de)
      or    b
      exx
      and   (hl)
      ld    (hl),a
      jp    ThreeD@FillLine3
@NotDone:
      exx
      and   (hl)
      ld    (hl),a
      inc   hl
      exx
@Loop:
      ld    a,b
      add   a,8
      cp    c
      jp    nc,@Done
      ld    b,a
      exx
      ld    (hl),$00
      inc   hl
      exx
      jr    @Loop
@Done:
      ld    e,c
      ld    a,(de)
      exx
      and   (hl)
      ld    (hl),a
      jp    ThreeD@FillLine3
@SkipItAll:
      ld    e,a
      exx
      rrca
      rrca
      rrca
      and   %00011111
      ld    h,0
      ld    l,a
      add   hl,de
function(ThreeD@FillLine3):
      ld    a,(bc)
      res   7,c
      inc   c
      exx
      ld    c,a
      ld    a,e
      or    7
      ld    b,a
      cp    c
      ld    a,(de)
      jr    c,@NotDone
      ld    b,a
      ld    e,c
      ld    a,(de)
      cpl
      and   b
      exx
      or    (hl)
      ld    (hl),a
      jp    ThreeD@FillLine4
@NotDone:
      exx
      or    (hl)
      ld    (hl),a
      inc   hl
      exx
@Loop:
      ld    a,b
      add   a,8
      cp    c
      jp    nc,@Done
      ld    b,a
      exx
      ld    (hl),$FF
      inc   hl
      exx
      jr    @Loop
@Done:
      ld    e,c
      ld    a,(de)
      cpl
      exx
      or    (hl)
      ld    (hl),a
function(ThreeD@FillLine4):
      ld    hl,12
      add   hl,de
      ex    de,hl
      exx
      dec   l
      exx
      jp    nz,ThreeD@FillLine1
      ret

function(ThreeD@FillLine1I):
      ld    a,(bc)
      inc   c
      exx
      ld    e,a
      exx
      rrca
      rrca
      rrca
      and   %00011111
      ld    h,0
      ld    l,a
      add   hl,de
      ld    a,(bc)
      dec   c
      exx
      ld    c,a
      ld    a,e
      or    7
      ld    b,a
      cp    c
      ld    a,(de)
      cpl
      jr    c,@NotDone
      ld    b,a
      ld    e,c
      ld    a,(de)
      or    b
      exx
      and   (hl)
      ld    (hl),a
      jp    ThreeD@FillLine2I
@NotDone:
      exx
      and   (hl)
      ld    (hl),a
      inc   hl
      exx
@Loop:
      ld    a,b
      add   a,8
      cp    c
      jp    nc,@Done
      ld    b,a
      exx
      ld    (hl),$00
      inc   hl
      exx
      jr    @Loop
@Done:
      ld    e,c
      ld    a,(de)
      exx
      and   (hl)
      ld    (hl),a
function(ThreeD@FillLine2I):
      set   7,c
      ld    a,(bc)
      inc   c
      exx
      cp    e
      jr    c,@SkipItAll
      ld    c,a
      ld    a,e
      or    7
      ld    b,a
      cp    c
      ld    a,(de)
      jr    c,@NotDone
      ld    b,a
      ld    e,c
      ld    a,(de)
      cpl
      and   b
      exx
      or    (hl)
      ld    (hl),a
      jp    ThreeD@FillLine3I
@NotDone:
      exx
      or    (hl)
      ld    (hl),a
      inc   hl
      exx
@Loop:
      ld    a,b
      add   a,8
      cp    c
      jp    nc,@Done
      ld    b,a
      exx
      ld    (hl),$FF
      inc   hl
      exx
      jr    @Loop
@Done:
      ld    e,c
      ld    a,(de)
      cpl
      exx
      or    (hl)
      ld    (hl),a
      jp    ThreeD@FillLine3I
@SkipItAll:
      ld    e,a
      exx
      rrca
      rrca
      rrca
      and   %00011111
      ld    h,0
      ld    l,a
      add   hl,de
function(ThreeD@FillLine3I):
      ld    a,(bc)
      res   7,c
      inc   c
      exx
      ld    c,a
      ld    a,e
      or    7
      ld    b,a
      cp    c
      ld    a,(de)
      cpl
      jr    c,@NotDone
      ld    b,a
      ld    e,c
      ld    a,(de)
      or    b
      exx
      and   (hl)
      ld    (hl),a
      jp    ThreeD@FillLine4I
@NotDone:
      exx
      and   (hl)
      ld    (hl),a
      inc   hl
      exx
@Loop:
      ld    a,b
      add   a,8
      cp    c
      jp    nc,@Done
      ld    b,a
      exx
      ld    (hl),$00
      inc   hl
      exx
      jr    @Loop
@Done:
      ld    e,c
      ld    a,(de)
      exx
      and   (hl)
      ld    (hl),a
function(ThreeD@FillLine4I):
      ld    hl,12
      add   hl,de
      ex    de,hl
      exx
      dec   l
      exx
      jp    nz,ThreeD@FillLine1I
      ret

function(ThreeD@DrawGraph):
      ld    a,(curGStyle)
      or    a
      jp    z,@NotTracing

      ld    hl,_X
      rst   20h
      bcall _StoX
      ld    hl,_xMin
      call  Mov9ToOP2
      bcall _FPSub
      ld    hl,_deltaX
      call  Mov9ToOP2
      bcall _FPDiv
      bcall _OP2Set8
      bcall _FPMult
      call  ThreeD@ConvertOP1
      push  af

      ld    hl,_Y
      rst   20h
      bcall _StoY
      ld    hl,_yMin
      call  Mov9ToOP2
      bcall _FPSub
      ld    hl,_deltaY
      call  Mov9ToOP2
      bcall _FPDiv
      bcall _OP2Set8
      bcall _FPMult
      call  ThreeD@ConvertOP1
      push  af

      ld    hl,ThreeD@Strings@EntryName
      rst   20h
      AppOnErr(@ErrorHandler)
      bcall _ParseInp
      AppOffErr
      ld    hl,OP1
      ld    de,_Z
      call  Mov9
      bcall _CkOP1Real
      ld    a,$80
      jr    nz,@OutRanged
      ld    hl,_zTemp
      call  Mov9ToOP2
      rst   30h   ;op1 += op2
      ld    hl,_rangeZ
      call  Mov9ToOP2
      bcall _FPMult
      call  ThreeD@ConvertOP1
@OutRanged:
      ld    hl,_XYZTable + RES*RES*3
      pop   de    ;D = y
      pop   bc    ;B = x
      ld    (hl),b
      inc   hl
      ld    (hl),d
      inc   hl
      ld    (hl),a
      add   a,a
      jr    c,@NotTracing
@NotTracing:

      di
      call  @GraphFunction

      ld    hl,ThreeD@LineBegin
      ld    de,saveSScreen
      ld    bc,LineSize
      ldir
      ld    ix,_XYTable

#define AXISSIZE 40

      ld    de,_XYTable
      ld    a,$80
      ld    (de),a
      ld    a,(YAxis)
      cp    $80
      jr    z,@NoXAxis
      ld    b,a
      ld    a,(ZAxis)
      cp    $80
      jr    z,@NoXAxis
      ld    c,a
      push  bc
      ld    a,-AXISSIZE
      call  ThreeD@RotatePoint
      pop   bc
      ld    a,AXISSIZE
      call  ThreeD@RotatePoint
      ld    hl,_Flags
      bit   grfNoAxis,(hl)
      jr    nz,@NoXAxis
      ld    hl,(_XYTable + 0)
      ld    de,(_XYTable + 2)
      call  ThreeD@Line
@NoXAxis:
      ld    de,_XYTable + 4
      ld    a,$80
      ld    (de),a
      ld    a,(XAxis)
      cp    $80
      jr    z,@NoYAxis
      ld    b,a
      ld    a,(ZAxis)
      cp    $80
      jr    z,@NoYAxis
      ld    c,a
      push  bc
      ld    a,b
      ld    b,-AXISSIZE
      call  ThreeD@RotatePoint
      pop   bc
      ld    a,b
      ld    b,AXISSIZE
      call  ThreeD@RotatePoint
      ld    hl,_Flags
      bit   grfNoAxis,(hl)
      jr    nz,@NoYAxis
      ld    hl,(_XYTable + 4)
      ld    de,(_XYTable + 6)
      call  ThreeD@Line
@NoYAxis:
      ld    de,_XYTable + 8
      ld    a,$80
      ld    (de),a
      ld    a,(YAxis)
      cp    $80
      jr    z,@NoZAxis
      ld    b,a
      ld    a,(XAxis)
      cp    $80
      jr    z,@NoZAxis
      ld    c,a
      push  bc
      ld    a,c
      ld    c,-AXISSIZE
      call  ThreeD@RotatePoint
      pop   bc
      ld    a,c
      ld    c,AXISSIZE
      call  ThreeD@RotatePoint
      ld    hl,_Flags
      bit   grfNoAxis,(hl)
      jr    nz,@NoZAxis
      ld    hl,(_XYTable + 8)
      ld    de,(_XYTable + 10)
      call  ThreeD@Line
@NoZAxis:

@NoAxes:

      ld    hl,_Flags
      bit   grfLabel,(hl)
      jr    z,@NoLabels
      set   textEraseBelow,(iy + textFlags)
      ld    a,'X'
      ld    de,_XYTable
@LabelLoop:
      push  de
      push  af
      ld    a,(de)
      inc   e
      inc   e
      cp    $80
      jr    z,@LabelSkip
      ld    a,(de)
      sub   3
      jr    nc,@LabelOk1
      xor   a
@LabelOk1:
      cp    57 + 1
      jr    c,@LabelOk2
      ld    a,57
@LabelOk2:
      ld    h,a
      inc   e
      ld    a,(de)
      sub   2
      jr    nc,@LabelOk3
      xor   a
@LabelOk3:
      cp    91 + 1
      jr    c,@LabelOk4
      ld    a,91
@LabelOk4:
      ld    l,a
      ld    (penCol),hl
      set   4,(iy + $24)      ; allow text in right-most column
      ld    a,' '
      bcall _VPutMap
      pop   af
      push  af
      bcall _VPutMap
      res   4,(iy + $24)
@LabelSkip:
      pop   af
      pop   de
      inc   e
      inc   e
      inc   e
      inc   e
      inc   a
      cp    'Z'+1
      jr    c,@LabelLoop
      res   textEraseBelow,(iy + textFlags)
@NoLabels:

      ld    hl,_Flags
      bit   grfNoCoord,(hl)
      jr    nz,@NoCoord
      ld    a,(curGStyle)
      or    a
      jr    z,@NoCoord
      res   2,(iy + smartFlags)
      bcall _COORDISP
      ld    a,(penRow)
      sub   6
      ld    h,a
      ld    l,1
      ld    (penCol),hl
      ld    a,'Z'
      bcall _VPutMap
      ld    a,'='
      bcall _VPutMap
      ld    hl,_Z
      rst   20h
      bcall _CkOP1Real
      jr    nz,@NoCoord
      ld    a,9
      bcall _FormEReal
      ld    hl,OP3
      bcall _VPutS
@NoCoord:

Graph@cxRedisp:
      bcall _GrBufCpy

      res   2,(iy + graphFlags)     ;enable cursor
      ld    a,(curGStyle)
      or    a
      ret   z
      ld    hl,(_XYTable + (RES*RES*2))
      ld    a,l
      add   a,a   ; test if cursor is on a point whose z value is out of range
      ret   c     ; (if it is, we still show the trace coordinates, but no cursor)
      ld    a,(lcdTallP)
      dec   a
      sub   l
      ld    l,a   ; fix Y coordinate for OS
      ld    (curGY),hl
      set   2,(iy + graphFlags)     ;enable cursor
      ret

@ErrorHandler:
      cp    E_Break
      jr    z,@DoError
      cp    E_NonReal
      jr    c,@ErrorRet
      jr    nz,@DoError
      ld    a,(OP1)
      and   $1F
      cp    CplxObj
      ld    a,$80
      jr    nz,@ErrorRet
@DoError:
      ld    a,tun
      ld    ($9654),a   ; seems to contain name of variable causing an error
      ld    a,(errNo)
      ;and   $7F
      bjump _JError
@ErrorRet:
      ld    a,CplxObj
      ld    (_Z),a      ; dirty Z value to be a non-real number if an error happened (div by 0 usually)
      ld    a,$80
      jp    @OutRanged

@GraphFunction:
      ld    (tempSP),sp
      ld    hl,0
      ld    b,64
      ld    sp,plotSScreen + 64*12
      bit   grfSplit,(iy + sGrFlags)
      jr    z,@ClearScreen
      ld    b,31
      ld    sp,plotSScreen + 32*12
      dec   hl
      push  hl
      push  hl
      push  hl
      push  hl
      push  hl
      push  hl
      inc   hl
@ClearScreen:
      push  hl
      push  hl
      push  hl
      push  hl
      push  hl
      push  hl
      djnz  @ClearScreen
      ld    sp,(tempSP)

      ld    de,$0080
      ld    hl,(_RotXX)
      add   hl,de
      ld    a,h
      ld    (RotXX),a
      ld    hl,(_RotXY)
      add   hl,de
      ld    a,h
      ld    (RotXY),a
      ld    hl,(_RotXZ)
      add   hl,de
      ld    a,h
      ld    (RotXZ),a
      ld    hl,(_RotYX)
      add   hl,de
      ld    a,h
      ld    (RotYX),a
      ld    hl,(_RotYY)
      add   hl,de
      ld    a,h
      ld    (RotYY),a
      ld    hl,(_RotYZ)
      add   hl,de
      ld    a,h
      ld    (RotYZ),a
      ld    hl,(_RotZX)
      add   hl,de
      ld    a,h
      ld    (RotZX),a
      ld    hl,(_RotZY)
      add   hl,de
      ld    a,h
      ld    (RotZY),a
      ld    hl,(_RotZZ)
      add   hl,de
      ld    a,h
      ld    (RotZZ),a

      ld    hl,_Flags
      bit   0,(hl)
      ret   z

      ld    a,1         ; this makes keys repeat as fast as possible
      ld    ($8442),a

      ld    hl,_XYZTable
      ld    de,_XYTable
DoTransformPoints:
      ld    a,(hl)
      inc   hl
      ld    b,(hl)
      inc   hl
      ld    c,(hl)
      inc   hl
      dec   c
      jp    po,@InRange
      ld    a,$80
      ld    (de),a
      inc   e
      inc   e
      jp    @OutRange
@InRange:
      inc   c
      call  ThreeD@RotatePoint
@OutRange:
      ld    a,e
      cp    (RES*RES*2) + 2         ; this is the loop counter value
      jp    c,DoTransformPoints

      ld    hl,_Flags
      bit   1,(hl)
      jp    z,ThreeD@WireFrame

;FindFarCorner:
      ld    hl,(RotZX)  ;L = RotZX, H = RotZY
      ld    a,h
      ld    b,2
      ld    c,a
;FindFarCorner0:
      ld    a,l
      cp    c
      jp    m,@FindFarCorner1
      ld    b,1
      ld    c,a
@FindFarCorner1:
      xor   a
      sub   h
      cp    c
      jp    m,@FindFarCorner2
      ld    b,0
      ld    c,a
@FindFarCorner2:
      xor   a
      sub   l
      cp    c
      jp    m,@FindFarCorner3
      ld    b,3
@FindFarCorner3:

      ld    c,RES - 1
      dec   b
      jp    z,ThreeD@DrawGraph2
      dec   b
      jp    z,ThreeD@DrawGraph3
      dec   b
      jp    z,ThreeD@DrawGraph4
function(ThreeD@DrawGraph1):
      ld    iy,_XYTable + ((RES - 1) * 2)
@Yloop:
      ld    b,RES - 1
@Xloop:
      push  bc
      ld    a,(_XYZTable + 6)       ; for the platypus egg
      sub   -19
      jp    nz,@NotPlatypus
      ld    a,b
      ld    b,c
      ld    c,a
      ld    a,RES - 1
      sub   b
      ld    b,a
      dec   c
      call  SavePlatypus
@NotPlatypus:
      ld    l,(iy +  0)
      bit   7,l
      jr    nz,@SkipPoly
      ld    h,(iy +  1)
      ld    (_Poly1),hl
      ld    l,(iy + ((RES * 2) - 2))
      bit   7,l
      jr    nz,@SkipPoly
      ld    h,(iy + ((RES * 2) - 1))
      ld    (_Poly3),hl
      ld    l,(iy -  2)
      bit   7,l
      jr    nz,@SkipPoly
      ld    h,(iy -  1)
      ld    (_Poly4),hl
      ld    l,(iy + ((RES * 2) + 0))
      bit   7,l
      jr    nz,@SkipPoly
      ld    h,(iy + ((RES * 2) + 1))
      ld    (_Poly2),hl
      call  ThreeD@DrawPoly
@SkipPoly:
      pop   bc
      dec   iy
      dec   iy
      djnz  @Xloop
      ld    de,(RES * 2) + ((RES - 1) * 2)
      add   iy,de
      dec   c
      jr    nz,@Yloop
      ld    iy,flags
      ret

function(ThreeD@DrawGraph2):
      ld    iy,_XYTable + ((RES - 2) * 2 * RES) + ((RES - 1) * 2)
@Yloop:
      ld    b,RES - 1
@Xloop:
      push  bc
      ld    a,(_XYZTable + 6)       ; for the platypus egg
      sub   -19
      jp    nz,@NotPlatypus
      dec   b
      dec   c
      call  SavePlatypus
@NotPlatypus:
      ld    l,(iy +  0)
      bit   7,l
      jr    nz,@SkipPoly
      ld    h,(iy +  1)
      ld    (_Poly1),hl
      ld    l,(iy + ((RES * 2) - 2))
      bit   7,l
      jr    nz,@SkipPoly
      ld    h,(iy + ((RES * 2) - 1))
      ld    (_Poly3),hl
      ld    l,(iy -  2)
      bit   7,l
      jr    nz,@SkipPoly
      ld    h,(iy -  1)
      ld    (_Poly4),hl
      ld    l,(iy + ((RES * 2) + 0))
      bit   7,l
      jr    nz,@SkipPoly
      ld    h,(iy + ((RES * 2) + 1))
      ld    (_Poly2),hl
      call  ThreeD@DrawPoly
@SkipPoly:
      pop   bc
      ld    de,-(RES*2)
      add   iy,de
      djnz  @Xloop
      ld    de,((RES * 2) * (RES - 1))-2
      add   iy,de
      dec   c
      jr    nz,@Yloop
      ld    iy,flags
      ret

function(ThreeD@DrawGraph3):
      ld    iy,_XYTable + ((RES - 2) * 2 * RES) + 2
@Yloop:
      ld    b,RES - 1
@Xloop:
      push  bc
      ld    a,(_XYZTable + 6)       ; for the platypus egg
      sub   -19
      jp    nz,@NotPlatypus
      ld    a,b
      ld    b,c
      ld    c,a
      dec   b
      ld    a,RES - 1
      sub   c
      ld    c,a
      call  SavePlatypus
@NotPlatypus:
      ld    l,(iy +  0)
      bit   7,l
      jr    nz,@SkipPoly
      ld    h,(iy +  1)
      ld    (_Poly1),hl
      ld    l,(iy + ((RES * 2) - 2))
      bit   7,l
      jr    nz,@SkipPoly
      ld    h,(iy + ((RES * 2) - 1))
      ld    (_Poly3),hl
      ld    l,(iy -  2)
      bit   7,l
      jr    nz,@SkipPoly
      ld    h,(iy -  1)
      ld    (_Poly4),hl
      ld    l,(iy + ((RES * 2) + 0))
      bit   7,l
      jr    nz,@SkipPoly
      ld    h,(iy + ((RES * 2) + 1))
      ld    (_Poly2),hl
      call  ThreeD@DrawPoly
@SkipPoly:
      pop   bc
      inc   iy
      inc   iy
      djnz  @Xloop
      ld    de,-((RES * 2) + ((RES - 1) * 2))
      add   iy,de
      dec   c
      jr    nz,@Yloop
      ld    iy,flags
      ret

function(ThreeD@DrawGraph4):
      ld    iy,_XYTable + 2
@Yloop:
      ld    b,RES - 1
@Xloop:
      push  bc
      ld    a,(_XYZTable + 6)       ; for the platypus egg
      sub   -19
      jp    nz,@NotPlatypus
      ld    a,RES - 1
      sub   b
      ld    b,a
      ld    a,RES - 1
      sub   c
      ld    c,a
      call  SavePlatypus
@NotPlatypus:
      ld    l,(iy +  0)
      bit   7,l
      jr    nz,@SkipPoly
      ld    h,(iy +  1)
      ld    (_Poly1),hl
      ld    l,(iy + ((RES * 2) - 2))
      bit   7,l
      jr    nz,@SkipPoly
      ld    h,(iy + ((RES * 2) - 1))
      ld    (_Poly3),hl
      ld    l,(iy -  2)
      bit   7,l
      jr    nz,@SkipPoly
      ld    h,(iy -  1)
      ld    (_Poly4),hl
      ld    l,(iy + ((RES * 2) + 0))
      bit   7,l
      jr    nz,@SkipPoly
      ld    h,(iy + ((RES * 2) + 1))
      ld    (_Poly2),hl
      call  ThreeD@DrawPoly
@SkipPoly:
      pop   bc
      ld    de,RES*2
      add   iy,de
      djnz  @Xloop
      ld    de,2-((RES * 2) * (RES - 1))
      add   iy,de
      dec   c
      jr    nz,@Yloop
      ld    iy,flags
      ret

SavePlatypus:
      ld    a,b
      add   a,a   ; * 2
      ld    h,a
      add   a,a   ; * 4
      add   a,a   ; * 8
      add   a,h   ; * 10
      add   a,c
      ld    h,0
      ld    l,a
      ld    de,PlatypusFlags
      add   hl,de
      ld    a,(hl)
      ld    (PlatypiFill),a
      ret

function(ThreeD@WireFrame):
      ld    hl,ThreeD@LineBegin
      ld    de,saveSScreen
      ld    bc,LineSize
      ldir

      ld    c,RES
      ld    iy,_XYTable
@Yloop:
      ld    b,RES
@Xloop:
      ld    a,b
      cp    2
      jr    c,@Skip1
      ld    l,(iy +  0)
      bit   7,l
      jr    nz,@Skip2
      ld    h,(iy +  1)
      ld    e,(iy +  2)
      bit   7,e
      jr    nz,@Skip1
      ld    d,(iy +  3)
      push  bc
      call  ThreeD@Line
      pop   bc
@Skip1:
      ld    a,c
      cp    2
      jr    c,@Skip2
      ld    l,(iy +  0)
      bit   7,l
      jr    nz,@Skip2
      ld    h,(iy +  1)
      ld    e,(iy + ((RES*2) + 0))
      bit   7,e
      jr    nz,@Skip2
      ld    d,(iy + ((RES*2) + 1))
      push  bc
      call  ThreeD@Line
      pop   bc
@Skip2:
      inc   iy
      inc   iy
      djnz  @Xloop
      dec   c
      jr    nz,@Yloop
      ld    iy,flags
      ret

_Poly1      = $86F8
_Poly2      = $86FA
_Poly3      = $86FC
_Poly4      = $86FE
_ScanBuffer = $8700

;(D,E)-(B,C)
function(ThreeD@ScanLine):
      ld    a,c
      sub   e
      ld    c,a
      ld    a,b
      sub   d
      jr    c,@NegSlope
;PosSlope:
      ld    b,a
      cp    c
      jr    c,@PosSlope@Yindep
;PosSlope@Xindep:
      ld    a,b
      add   a,a
      ld    e,a
      xor   a
      sub   c
      sub   c
      ld    c,a

      ld    (hl),d
      inc   l
      inc   d
      ld    a,b
      or    a
      jr    z,@PosSlope@Xindep@Done
      add   a,c
@PosSlope@Xindep@Loop:
      jp    p,@PosSlope@Xindep@Stay
      add   a,e
      ld    (hl),d
      inc   l
      ld    (hl),d
      inc   l
@PosSlope@Xindep@Stay:
      inc   d
      add   a,c
      djnz  @PosSlope@Xindep@Loop
@PosSlope@Xindep@Done:
      ld    (hl),d
      ret

@PosSlope@Yindep:
      ld    a,c
      add   a,a
      ld    e,a
      xor   a
      sub   b
      sub   b
      ld    b,c
      ld    c,a

      ld    (hl),d
      inc   l
      inc   d
      ld    (hl),d
      dec   d
      ld    a,b
      or    a
      ret   z
      add   a,c
@PosSlope@Yindep@Loop:
      jp    p,@PosSlope@Yindep@Stay
      inc   d
      add   a,e
@PosSlope@Yindep@Stay:
      inc   l
      ld    (hl),d
      inc   l
      inc   d
      ld    (hl),d
      dec   d
      add   a,c
      djnz  @PosSlope@Yindep@Loop
      ret

@NegSlope:
      neg
      ld    b,a
      cp    c
      jr    c,@NegSlope@Yindep
;NegSlope@Xindep:
      ld    a,b
      add   a,a
      ld    e,a
      xor   a
      sub   c
      sub   c
      ld    c,a
      inc   d

      inc   l
      ld    (hl),d
      dec   l
      ld    a,b
      or    a
      jr    z,@NegSlope@Xindep@Done
      dec   d
      add   a,c
@NegSlope@Xindep@Loop:
      jp    p,@NegSlope@Xindep@Stay
      add   a,e
      ld    (hl),d
      inc   l
      inc   l
      inc   l
      ld    (hl),d
      dec   l
@NegSlope@Xindep@Stay:
      dec   d
      add   a,c
      djnz  @NegSlope@Xindep@Loop
@NegSlope@Xindep@Done:
      ld    (hl),d
      inc   l
      ret

@NegSlope@Yindep:
      ld    a,c
      add   a,a
      ld    e,a
      xor   a
      sub   b
      sub   b
      ld    b,c
      ld    c,a

      ld    (hl),d
      inc   l
      inc   d
      ld    (hl),d
      dec   d
      ld    a,b
      or    a
      ret   z
      add   a,c
@NegSlope@Yindep@Loop:
      jp    p,@NegSlope@Yindep@Stay
      dec   d
      add   a,e
@NegSlope@Yindep@Stay:
      inc   l
      ld    (hl),d
      inc   l
      inc   d
      ld    (hl),d
      dec   d
      add   a,c
      djnz  @NegSlope@Yindep@Loop
      ret

function(ThreeD@MultAxDE):
      ld    b,-1
      add   a,a
      jr    nc,@NotNeg
      push  de
      ld    hl,0
      sbc   hl,de
      add   hl,hl
      ex    de,hl
      sbc   hl,hl
      jr    c,$+4
      inc   b
      inc   l
      adc   a,0
      add   hl,hl
      rla
      jr    c,$+4
      add   hl,de
      adc   a,b
      add   hl,hl
      rla
      jr    c,$+4
      add   hl,de
      adc   a,b
      add   hl,hl
      rla
      jr    c,$+4
      add   hl,de
      adc   a,b
      add   hl,hl
      rla
      jr    c,$+4
      add   hl,de
      adc   a,b
      add   hl,hl
      rla
      jr    c,$+4
      add   hl,de
      adc   a,b
      add   hl,hl
      rla
      jr    c,$+4
      add   hl,de
      adc   a,b
      add   hl,hl
      rla
      jr    c,$+4
      add   hl,de
      adc   a,b
      add   hl,de
      adc   a,b
      ld    l,h
      ld    h,a
      pop   de
      ret
@NotNeg:
      sla   e
      rl    d
      sbc   hl,hl
      jr    c,$+4
      inc   b
      inc   l
      adc   a,0
      add   hl,hl
      rla
      jr    nc,$+4
      add   hl,de
      adc   a,b
      add   hl,hl
      rla
      jr    nc,$+4
      add   hl,de
      adc   a,b
      add   hl,hl
      rla
      jr    nc,$+4
      add   hl,de
      adc   a,b
      add   hl,hl
      rla
      jr    nc,$+4
      add   hl,de
      adc   a,b
      add   hl,hl
      rla
      jr    nc,$+4
      add   hl,de
      adc   a,b
      add   hl,hl
      rla
      jr    nc,$+4
      add   hl,de
      adc   a,b
      add   hl,hl
      rla
      jr    nc,$+4
      add   hl,de
      adc   a,b
      ld    l,h
      ld    h,a
      ret

; For these functions:
;  Multiply by input in DE (16 bits)
;  Output to HL

#define MULT32_0A add hl,hl \ adc a,a
#define MULT32_1A add hl,hl \ adc a,a \ add hl,de \ adc a,b
#define MULT32_0B add hl,hl \ rl c \ rl b
#define MULT32_1B add hl,hl \ rl c \ rl b \ add hl,de \ jr nc,$+3 \ inc bc

; 0000110010001110
function(ThreeD@SinTheta):
      xor   a
      ld    b,a
      sla   e
      rl    d
      jr    nc,@NotNeg
      ld    hl,1
      sbc   hl,de
      ld    d,h
      ld    e,l
      MULT32_1A
      MULT32_0A
      MULT32_0A
      ld    c,a
      MULT32_1B
      MULT32_0B
      MULT32_0B
      MULT32_0B
      MULT32_1B
      MULT32_1B
      MULT32_1B
      MULT32_0B
      ld    a,b
      cpl
      ld    b,a
      ld    a,c
      cpl
      ld    c,a
      ld    a,h
      or    l
      jr    z,@Ret
      rl    h
      ret   c
@Ret  inc   bc
      ret
@NotNeg:
      ld    h,d   ;
      ld    l,e   ;1
      MULT32_1A
      MULT32_0A
      MULT32_0A
      ld    c,a
      MULT32_1B
      MULT32_0B
      MULT32_0B
      MULT32_0B
      MULT32_1B
      MULT32_1B
      MULT32_1B
      MULT32_0B
      rl    h
      ret   nc
      inc   bc
      ret

; 0111111101100010
function(ThreeD@CosTheta):
      xor   a
      ld    b,a
      sla   e
      rl    d
      jr    nc,@NotNeg
      ld    hl,1
      sbc   hl,de
      ld    d,h
      ld    e,l
      MULT32_1A
      MULT32_1A
      MULT32_1A
      MULT32_1A
      MULT32_1A
      MULT32_1A
      ld    c,a
      MULT32_0B
      MULT32_1B
      MULT32_1B
      MULT32_0B
      MULT32_0B
      MULT32_0B
      MULT32_1B
      MULT32_0B
      ld    a,b
      cpl
      ld    b,a
      ld    a,c
      cpl
      ld    c,a
      ld    a,h
      or    l
      jr    z,@Ret
      rl    h
      ret   c
@Ret  inc   bc
      ret
@NotNeg:
      ld    h,d   ;
      ld    l,e   ;1
      MULT32_1A
      MULT32_1A
      MULT32_1A
      MULT32_1A
      MULT32_1A
      MULT32_1A
      ld    c,a
      MULT32_0B
      MULT32_1B
      MULT32_1B
      MULT32_0B
      MULT32_0B
      MULT32_0B
      MULT32_1B
      MULT32_0B
      rl    h
      ret   nc
      inc   bc
      ret

;Inputs: DE is A, HL is B:
;Outputs:
; DE = A*cos - B*sin
; HL = A*sin + B*cos
RotatePositive:
      push  de
      push  hl
      call  ThreeD@CosTheta
      pop   de
      push  de
      push  bc
      call  ThreeD@SinTheta
      pop   hl
      or    a
      sbc   hl,bc
      pop   de
      push  hl
      call  ThreeD@CosTheta
      pop   hl
      pop   de
      push  hl
      push  bc
      call  ThreeD@SinTheta
      pop   hl
      add   hl,bc
      pop   de
      ret

;Inputs: DE is A, HL is B:
;Outputs:
; DE =  A*cos + B*sin
; HL = -A*sin + B*cos
RotateNegative:
      push  de
      push  hl
      call  ThreeD@CosTheta
      pop   de
      push  de
      push  bc
      call  ThreeD@SinTheta
      pop   hl
      add   hl,bc
      pop   de
      push  hl
      call  ThreeD@CosTheta
      pop   hl
      pop   de
      push  hl
      push  bc
      call  ThreeD@SinTheta
      pop   hl
      or    a
      sbc   hl,bc
      pop   de
      ret

function(ThreeD@RotatePoint):
      push  hl
      push  de
      ld    e,a
      ex    af,af'
      ld    a,(RotYX)
      ld    d,a
      MultByE
      push  de
      ld    a,(RotYY)
      ld    d,a
      ld    e,b
      MultByE
      push  de
      ld    a,(RotYZ)
      ld    d,a
      ld    e,c
      MultByE
      pop   hl
      add   hl,de
      pop   de
      add   hl,de
      add   hl,hl
      ld    de,$0080 ;for rounding
      add   hl,de
      push  hl
      ex    af,af'
      ld    e,a
      ex    af,af'
      ld    a,(RotXX)
      ld    d,a
      MultByE
      push  de
      ld    a,(RotXY)
      ld    d,a
      ld    e,b
      MultByE
      push  de
      ld    a,(RotXZ)
      ld    d,a
      ld    e,c
      MultByE
      pop   hl
      add   hl,de
      pop   de
      add   hl,de
      add   hl,hl
      ld    de,$0080 ;for rounding
      add   hl,de
      push  hl
      ex    af,af'
      ld    e,a
      ex    af,af'
      ld    a,(RotZX)
      ld    d,a
      MultByE
      push  de
      ld    a,(RotZY)
      ld    d,a
      ld    e,b
      MultByE
      push  de
      ld    a,(RotZZ)
      ld    d,a
      ld    e,c
      MultByE
      pop   hl
      add   hl,de
      pop   de
      add   hl,de
      add   hl,hl
      ld    de,$0080 ;for rounding
      add   hl,de

      ld    l,h
      ld    h,(DivTable >> 8) + 1
      ld    a,(hl)
      dec   h
      ld    c,(hl)
      pop   de
      call  ThreeD@MultAxDE
      ld    a,c
      ld    c,h
      pop   de
      call  ThreeD@MultAxDE
      ld    a,h
      pop   de
      pop   hl
      bit   grfSplit,(iy + sGrFlags)
      jr    z,@NotSplit
      sra   c
      sra   a
      sub   16
@NotSplit:
      add   a,32
      ld    (de),a
      inc   e
      ld    a,c
      add   a,48
      ld    (de),a
      inc   e
      ret
