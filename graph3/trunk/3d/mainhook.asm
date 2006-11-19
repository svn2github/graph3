;OmniCalc uses the first 20 bytes of appBackupScreen.

PlatypiFill	= appBackupScreen +  29
_Flags	= appBackupScreen +  30
_RotXX	= appBackupScreen +  31
_RotXY	= appBackupScreen +  33
_RotXZ	= appBackupScreen +  35
_RotYX	= appBackupScreen +  37
_RotYY	= appBackupScreen +  39
_RotYZ	= appBackupScreen +  41
_RotZX	= appBackupScreen +  43
_RotZY	= appBackupScreen +  45
_RotZZ	= appBackupScreen +  47
_xMin       = appBackupScreen +  49
_xMax       = appBackupScreen +  58
_yMin       = appBackupScreen +  67
_yMax       = appBackupScreen +  76
_zMin       = appBackupScreen +  85
_zMax       = appBackupScreen +  94
_X		= appBackupScreen + 103
_Y		= appBackupScreen + 112
XAxis		= appBackupScreen + 121
YAxis		= appBackupScreen + 122
ZAxis		= appBackupScreen + 123

_Z		= appBackupScreen + 130
_deltaX     = appBackupScreen + 139
_deltaY     = appBackupScreen + 148
_rangeZ     = appBackupScreen + 157
_zTemp	= appBackupScreen + 166

RotXX       = appBackupScreen + 180
RotXY       = appBackupScreen + 181
RotXZ       = appBackupScreen + 182
RotYX       = appBackupScreen + 183
RotYY       = appBackupScreen + 184
RotYZ       = appBackupScreen + 185
RotZX       = appBackupScreen + 186
RotZY       = appBackupScreen + 187
RotZZ       = appBackupScreen + 188

_counterPtr = appBackupScreen + 190
_counterX   = appBackupScreen + 192
_counterY   = appBackupScreen + 193

SaveX1TMode	= appBackupScreen + 197
SaveMode	= appBackupScreen + 198
_XYZTable   = appBackupScreen + 200

_XYTable    = $8800 ; on _saveSScreen; must be aligned

;	set	0,(iy + plotFlags)
;	does not restore from freesavex
;0,(iy+$28)??
; set 2,(iy+3) : graph cursor
; 8E63 is weird: should be 0

function(GraphHook):
	push	af
	push	bc
	bcall	_getBaseVer
	cp	2
	jr	nc,@above112
	cp	1
	jr	nz,@below112
	ld	a,b
	cp	13
	jr	c,@below112
@above112:
	ld	a,$01		; more than 15mhz would be too fast...
	bcall	_SetExSpeed
@below112:
	pop	bc
	pop	af

	call	CheckGraphMode
	jp	z,@Allow

	or	a
	jr	nz,@Not0
	call	@Key
	xor	a
	ret
@Not0:

#if 0
	cp	1
	jr	nz,@Not1
	bit	0,(iy + asm_Flag1)
	jr	z,@Allow
	res	curOn,(iy + curFlags)
	bcall	_GrBufCpy
	ld	hl,(winBtm)
	dec	l
	bit	grfSplit,(iy + sGrFlags)
	jr	z,@Split1
	ld	l,3
@Split1:
	ld	h,0
	ld	(curRow),hl
	set	6,(iy + $1D)
	set	7,(iy + $1D)
	ld	a,'X'
	bcall	_PutC
	ld	a,'='
	bcall	_PutC
	bcall	_EraseEOL

	bcall	_SetNumWindow
	bcall	$4753
	ld	(editSym),hl
	bcall	$4969
	bcall	_CanAlphIns
	res	4,(iy + $0C)
	set	0,(iy + $11)
	res	5,(iy + $0D)
	;ld	hl,$4572
	;ld	(cxErrorEP),hl
	or	$FF
	ret
@Not1:
#endif

	cp	3
	jr	nz,@Not3
	or	$FF
	ret
@Not3:

	cp	4
	jr	nz,@Not4
	set	2,(iy + smartFlags)
	xor	a
	ret
@Not4:

	cp	6
	jr	nz,@Not6
	call	@Key
	cp	kGraph
	jr	z,@Allow
	cp	kTrace
	jr	z,@Allow
	jp	Graph@cxMain
@Not6:

	cp	7
	jp	z,DoGraph
@Allow:
	xor	a
	ret

@Key:
	ld	a,(_Flags)
	rra
	ld	a,b
	jr	nc,@NotAnythingCursory
	cp	kGraph
	jr	nz,@NotGraph
	res	0,(iy + asm_Flag1)
      res	2,(iy + graphFlags)	;disable cursor
      xor	a
      ld	(curGStyle),a
      ret
@NotGraph:
	cp	kTrace
	jr	nz,@NotTrace
	res	1,(iy + asm_Flag1)	; is zooming?
@ZoomCommon:
      ld	a,$51
      ld	(curGStyle),a
      ld	a,b
	set	0,(iy + asm_Flag1)
	ret
@NotTrace:
	cp	kZIn
	jr	nz,@NotZIn
	set	1,(iy + asm_Flag1)	; is zooming?
	res	2,(iy + asm_Flag1)	; is zooming out?
	jr	@ZoomCommon
@NotZIn:
	cp	kZOut
	jr	nz,@NotZOut
	set	1,(iy + asm_Flag1)	; is zooming?
	set	2,(iy + asm_Flag1)	; is zooming out?
	jr	@ZoomCommon
@NotZOut:

@NotAnythingCursory:
	cp	kStd
	jr	nz,@NotStd
	ld	hl,Strings@StandardView
	ld	de,_RotXX
	ld	bc,Strings@ZStandardEnd - Strings@StandardView
	ldir
	set	graphDraw,(iy + graphFlags)
	ret
@NotStd:
	cp	kYes
	jr	nz,@NotYes
	ld	(kbdKey),a
@NotYes:
	ret

function(DoGraph):
	bit	vertSplit,(iy + sGrFlags)
	jr	z,@NotVertSplit
	bcall	_ForceFullScreen
@NotVertSplit:

	ld	a,(kbdKey)
	cp	kYes
	jr	nz,@NotYes
	ld    hl,PlatypusEgg
	ld    de,_XYZTable
	ld    bc,RES*RES*3
	ldir
	ld	hl,Strings@ViewPlatypus
      ld	de,_RotXX
      ld	bc,18
      ldir
      ld	hl,_Flags
      set	grfNoAxis,(hl)
      res	grfLabel,(hl)
      set	7,(hl)	; redraw after they change apps
      set	1,(hl)	; we desire platypi to be full
	jp    @ItsEaster
@NotYes:

	call  LookupEntry
	ld    a,(de)
	inc   de
	cp    4
	jr    nz,@NotEaster
	ld    a,(de)
	inc   de
	or    a
	jr    nz,@NotEaster
	ld    a,(de)
	inc   de
	cp    'K'
	jr    nz,@NotEaster
	ld    a,(de)
	inc   de
	cp    'I'
	jr    nz,@NotEaster
	ld    a,(de)
	inc   de
	cp    'R'
	jr    nz,@NotEaster
	ld    a,(de)
	cp    'K'
	jr    nz,@NotEaster
	ld    hl,EasterEgg
	ld    de,_XYZTable
	ld    bc,RES*RES*3
	ldir
	ld	hl,Strings@ViewEaster
      ld	de,_RotXX
      ld	bc,18
      ldir
      ld	hl,_Flags
      set	grfNoAxis,(hl)
      res	grfLabel,(hl)
	jp    @ItsEaster
@NotEaster:

	ld	hl,_Flags
	ld	a,(flags + graphFlags)
	rrca
	and	%10000000
	or	(hl)
	ld	(hl),a
	add	a,a
	jp	nc,@FillIn

	ld	hl,Strings@StandardView
	ld	de,_RotXX
	ld	bc,18
	ldir

	ld	a,(_Flags)
	rrca			;function selected?
	jp	nc,@ItsEaster

	bcall	_SaveOScreen
	ld    hl,$0007
	ld    (curRow),hl
	set   textInverse,  (iy + textFlags)
	res	appTextSave,  (iy + appFlags)
	ld    hl,Strings@Calculating
	ld    de,OP1
	push  de
	ld    bc,16
	ldir
	pop   hl
	bcall _PutS
	ld	a,' '
	bcall	_PutMap
	set	appTextSave,  (iy + appFlags)
	res   textInverse,  (iy + textFlags)

	ld    hl,_xMin
	call  @WindowHelper
	ld    a,RES - 1
	bcall _SetXXOP2
	bcall _FPDiv
	ld    hl,OP1
	ld    de,_deltaX
	call  Mov9

	ld    hl,_yMin
	call  @WindowHelper
	ld    a,RES - 1
	bcall _SetXXOP2
	bcall _FPDiv
	ld    hl,OP1
	ld    de,_deltaY
	call  Mov9

	ld    hl,_zMin
	call  @WindowHelper
	ld    a,80
	bcall _SetXXOP2
	bcall _FPDiv
	bcall _FPRecip
	ld    hl,OP1
	ld    de,_rangeZ
	call  Mov9

	ld    hl,_zMin
	rst   20h
	bcall _InvOP1S
	ld    hl,OP1
	ld    de,_zTemp
	call  Mov9

	ld    hl,_yMin
	rst   20h
	bcall _StoY
	ld    hl,_XYZTable
	ld    (_counterPtr),hl
	ld    a,RES
	ld    (_counterY),a
@YLoop:
	ld    hl,_xMin
	rst   20h
	bcall _StoX
	ld    a,RES
	ld    (_counterX),a
@XLoop:
	ld    hl,Strings@EntryName
	rst   20h
	AppOnErr(GraphErrorHandler)
	bcall _ParseInp
	AppOffErr
	bcall	_CkOP1Real
	ld	a,$80
	jr	nz,@OutRange
	ld    hl,_zTemp
	call  Mov9ToOP2
	rst   30h   ;op1 += op2
	ld    hl,_rangeZ
	call  Mov9ToOP2
	bcall _FPMult
	call	ConvertOP1
@OutRange:
	push  af
	bcall _RclX
	ld    hl,_deltaX
	call  Mov9ToOP2
	rst   30h   ;op1 += op2
	bcall _StoX
	pop   af
	ld    hl,(_counterPtr)
	inc   hl
	inc   hl
	ld    (hl),a
	inc   hl
	ld    (_counterPtr),hl
	ld    hl,_counterX
	dec   (hl)
	jp    nz,@XLoop
	bcall _RclY
	ld    hl,_deltaY
	call  Mov9ToOP2
	rst   30h   ;op1 += op2
	bcall _StoY
	ld    hl,_counterY
	dec   (hl)
	jp    nz,@YLoop

	ld    hl,_xMin
	push	hl
	call  @WindowHelper
	bcall	_OP2Set2
	bcall	_FPDiv
	pop	hl
	call	Mov9ToOP2
	rst	30h	;op1 += op2
	ld	hl,OP1
	ld	de,_X
	call	Mov9
	ld    hl,_yMin
	push	hl
	call  @WindowHelper
	bcall	_OP2Set2
	bcall	_FPDiv
	pop	hl
	call	Mov9ToOP2
	rst	30h	;op1 += op2
	ld	hl,OP1
	ld	de,_Y
	call	Mov9

	ld	hl,_Flags
	res	7,(hl)
	res	graphDraw,(iy + graphFlags)
	bcall	_RstrOScreen

@FillIn:
	ld    hl,_XYZTable
	ld    e,-40
@FillInLoopY:
	ld    d,-40
@FillInLoopX:
	ld    (hl),d
	inc   hl
	ld    (hl),e
	inc   hl
	inc   hl
	ld    a,d
	add   a,8
	ld    d,a
	cp    40+1
	jp    m,@FillInLoopX
	ld    a,e
	add   a,8
	ld    e,a
	cp    40+1
	jp    m,@FillInLoopY

@ItsEaster:
	; calculate positions of the axes
	ld	hl,_xMin
	push	hl
	call	@WindowHelper
	rst	08h	;op2 = op1
	pop	hl
	rst	20h
	bcall	_InvOP1S
	bcall	_FPDiv
	ld    a,80
	bcall _SetXXOP2
	bcall _FPMult
	call	ConvertOP1
	ld	(XAxis),a

	ld	hl,_yMin
	push	hl
	call	@WindowHelper
	rst	08h	;op2 = op1
	pop	hl
	rst	20h
	bcall	_InvOP1S
	bcall	_FPDiv
	ld    a,80
	bcall _SetXXOP2
	bcall _FPMult
	call	ConvertOP1
	ld	(YAxis),a

	ld	hl,_zMin
	push	hl
	call	@WindowHelper
	rst	08h	;op2 = op1
	pop	hl
	rst	20h
	bcall	_InvOP1S
	bcall	_FPDiv
	ld    a,80
	bcall _SetXXOP2
	bcall _FPMult
	call	ConvertOP1
	ld	(ZAxis),a

	call	DrawGraph
	or	$FF
	ret

@WindowHelper:
	call  Mov9ToOP2
	rst   20h
	bcall _FPSub
	bcall	_CkOP1FP0
	jr	z,@WindowHelperBad
	bcall _CkOP1Pos
	ret   z
@WindowHelperBad:
	ld    a,E_GraphRange & $7F
	bjump _JError

function(GraphErrorHandler):
	cp    E_Break
	jr    z,@DoError
	cp    E_NonReal
	ld	a,$80
	jp    c,DoGraph@OutRange
	jr    nz,@DoError
	ld    a,(OP1)
	and   $1F
	cp    CplxObj
	ld	a,$80
	jp    nz,DoGraph@OutRange
@DoError:
	ld	a,tun
	ld	($9654),a	; seems to contain name of variable causing an error
	ld    a,(errNo)
	;and   $7F
	bjump _JError

function(Mov9ToOP2):
	ld    de,OP2
Mov9:
	ldi \ ldi \ ldi
	ldi \ ldi \ ldi
	ldi \ ldi \ ldi
	ret

function(ConvertOP1):
	ld    hl,OP1
	ld    a,(hl)
	add   a,a
	ld    a,$80
	ret	c
	inc   hl
	ld    a,(hl)
	cp    $81
	jr    c,@InRange
	ld    a,$80
	ret	nz
	inc   hl
	ld    a,$80       ;NOTE: This acts as the flag ($80) as well as BCD 80
	cp    (hl)
	ret	c
	jr    nz,@InRange
	ld    b,6
@CheckHighEndLoop:
	inc   hl
	ld    a,(hl)
	or    a
	ld    a,$80
	ret	nz
	djnz  @CheckHighEndLoop
@InRange:
	bcall _ConvOP1
	ld	a,e
	sub	40
	ret

#include "3d/graph.asm"
#include "3d/input.asm"
#include "3d/data.asm"

; Thanks to Matt Shepcar for this line routine!

LineBegin:
.org saveSScreen
function(Line):
	ld	a,h
	cp	d
	jr	nc,@Ordered
	ex	de,hl
@Ordered:
	ld	c,e
	ld	b,d
	push	bc
	push	hl
	call	FindPixel
	ld	de,plotSScreen
	add	hl,de
	pop	de
	pop	bc
;Connected:
	push	hl
	ld	h,c
	ld	c,a
	ld	a,d
	sub	b
	ld	b,a
	ld	a,e
	jr	nz,@NotPoint
	cp	h
	jr	nz,@NotPoint
	pop	hl
	ld	a,(hl)
	or	c
	ld	(hl),a
	ret
@NotPoint:
	sub	h
	ld	de,12
	jr	nc,@PositiveY
	neg
	ld	de,-12
@PositiveY:
	cp	b
	jr	nc,@Steep
	add	a,a
	ld	(@Line2sm+1),a
	ld	h,a
	xor	a
	sub	b
	sub	b
	ld	(@Line1sm+1),a
	ld	a,h
	sub	b
	pop	hl
@LoopGentle:
	ex	af,af'
	ld	a,(hl)
	or	c
	ld	(hl),a
	rrc	c
	jr	nc,$+3
	inc	hl
	ex	af,af'
	jp	m,@Line2sm
@Line1sm:
	add	a,0
	add	hl,de
@Line2sm:
	add	a,0
	djnz	@LoopGentle
	ld	a,(hl)
	or	c
	ld	(hl),a
	ret
@Steep:
	ld	h,a
	xor	a
	sub	h
	sub	h
	ld	(@Line3sm+1),a
	ld	a,b
	add	a,b
	ld	(@Line4sm+1),a
	sub	h
	ld	b,h
	pop	hl
@LoopSteep:
	ex	af,af'
	ld	a,(hl)
	or	c
	ld	(hl),a
	add	hl,de
	ex	af,af'
	jp	m,@Line4sm
@Line3sm:
	add	a,0
	rrc	c
	jr	nc,$+3
	inc	hl
@Line4sm:
	add	a,0
	djnz	@LoopSteep
	ld	a,(hl)
	or	c
	ld	(hl),a
	ret
LineSize = $ - Line

.end
