function(ThreeD@Strings):
@Title:
	.db "Graph�  v1.9",6,6,"graph3.sf.net",0
@Email:
	.db "Kirk  Meyer  &  Martin  Warmer",0

@ZoomMenu:
	.db 1		;how many headers?
	.db 3 	;how many choices under 1st header?
	.db $5E	;string table entry for "ZOOM"
	.db 0,kZIn
	.db 0,kZOut	;these are 2-byte keypresses of each entry
	.db 0,kStd

@PlatypiMenu:
	.db 1
	.db 2
	.db $FF
	.db 0,kYes
	.db 0,kNoWay
@Platypi:
	.db "SAVE THE PLATYPI",0

@Installed1:
	.db "Press  Y=  twice  to  enter",0
@Installed2:
	.db "3D  or  DiffEqu  mode.",0

@Removed1:
	.db "Graph�",6,"has",6,"been",0
@Removed2:
	.db "uninstalled.",0

@Uninstall:
	.db "the",6,"APP",6
	.db "Please",6,"uninstall",0

@NotAvailable1:
	.db "Not",6,"available",0
@NotAvailable2:
	.db "in",6,"3D",6,"mode.",0

@AnyKey:
	.db "Press",6,"any",6,"key...",0
@GraphName:
	.db AppVarObj,"Graph3",0
@EntryName:
	.db EquObj,tVarEqu,tY0+1,0
@Graph3:
	.db "Z(X,Y)",0
@Z1:
	.db "Z�=",0
@Calculating:
	.db " PLEASE WAIT...",0
@AppvarInit:
	.db %10100011	;REDRAW LABEL NOAXES NOCOORD # # FILL SEL	; +  0 (2)
@StandardView:
	.dw  -5604, 15396,     0
	.dw   5266,  1917,-15396
	.dw -14467, -5266, -5604
	.db $80,$81,$10,$00,$00,$00,$00,$00,$00 ;Xmin	; +  6 (8))		THESE OFFSETS ARE WRONG
	.db $00,$81,$10,$00,$00,$00,$00,$00,$00 ;Xmax
	.db $80,$81,$10,$00,$00,$00,$00,$00,$00 ;Ymin
	.db $00,$81,$10,$00,$00,$00,$00,$00,$00 ;Ymax
	.db $80,$81,$10,$00,$00,$00,$00,$00,$00 ;Zmin
	.db $00,$81,$10,$00,$00,$00,$00,$00,$00 ;Zmax
	.db $00,$80,$00,$00,$00,$00,$00,$00,$00 ;X
	.db $00,$80,$00,$00,$00,$00,$00,$00,$00 ;Y
@ZStandardEnd:
	.db 0,0,0
	.db 0,0,0,0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,0,0,0,0,0,0
	.dw 13							; + 78 + (RES*RES)
	.db tLParen,'X',tCube,'Y',tSub,'Y',tCube,'X',tRParen,tDiv,"390"
@AppvarInitSize = $ - @AppvarInit

@ViewXAxis:
	.dw      0, 16384,     0
	.dw      0,     0,-16384
	.dw -16384,     0,     0
@ViewYAxis:
	.dw -16384,     0,     0
	.dw      0,     0,-16384
	.dw      0,-16384,     0
@ViewZAxis:
	.dw  16384,     0,     0
	.dw      0,-16384,     0
	.dw      0,     0,-16384
@ViewEaster:
	.dw  16384,     0,     0
	.dw      0, 16384,     0
	.dw      0,     0, 16384
@ViewPlatypus:
	.dw $2D64,$018F,$D2F4
	.dw $F57A,$3E8C,$F792
	.dw $2BD1,$0D6A,$2CA3

EasterEgg:
.db $D8,$F2,$00,$DD,$F2,$00,$4C,$4E,$80,$4C,$4E,$80,$DD,$FA,$00,$E5,$F2,$00,$4C,$4E,$80,$4C,$4E,$80,$DD,$FF,$00,$EC,$0E,$00,$4C,$4E,$80
.db $D8,$0E,$00,$DD,$0E,$00,$4C,$4E,$80,$4C,$4E,$80,$DD,$01,$00,$EC,$F2,$00,$4C,$4E,$80,$4C,$4E,$80,$DD,$06,$00,$E5,$0E,$00,$4C,$4E,$80
.db $4C,$4E,$80,$4C,$4E,$80,$F1,$F2,$00,$F6,$F2,$00,$4C,$4E,$80,$4C,$4E,$80,$FB,$F2,$00,$00,$F2,$00,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80
.db $4C,$4E,$80,$4C,$4E,$80,$F1,$0E,$00,$F6,$0E,$00,$4C,$4E,$80,$4C,$4E,$80,$FB,$0E,$00,$00,$0E,$00,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80
.db $00,$F2,$00,$0F,$F9,$00,$4C,$4E,$80,$4C,$4E,$80,$00,$01,$00,$0B,$FC,$00,$4C,$4E,$80,$4C,$4E,$80,$05,$04,$00,$09,$02,$00,$4C,$4E,$80
.db $00,$F7,$00,$0F,$FE,$00,$4C,$4E,$80,$4C,$4E,$80,$00,$06,$00,$0F,$FF,$00,$4C,$4E,$80,$4C,$4E,$80,$0A,$0E,$00,$0F,$0E,$00,$4C,$4E,$80
.db $4C,$4E,$80,$4C,$4E,$80,$14,$F2,$00,$19,$F2,$00,$4C,$4E,$80,$4C,$4E,$80,$19,$FA,$00,$21,$F2,$00,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80
.db $4C,$4E,$80,$4C,$4E,$80,$14,$0E,$00,$19,$0E,$00,$4C,$4E,$80,$4C,$4E,$80,$19,$01,$00,$28,$F2,$00,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80
.db $19,$FF,$00,$28,$0E,$00,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80
.db $19,$06,$00,$21,$0E,$00,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80
.db $4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80,$4C,$4E,$80

PlatypusEgg:
.db 'L','N',$80, 'L','N',$80, -19,-35, 40, -16,-37, 40,  -8,-39, 40,   0,-40, 40,   8,-39, 40,  16,-37, 40,  19,-35, 40, 'L','N',$80, 'L','N',$80
.db 'L','N',$80, -28,-28, 40, -24,-32, 40, -16,-32, 22,  -8,-32, 17,   0,-32, 16,   8,-32, 17,  16,-32, 22,  24,-32, 40,  28,-28, 40, 'L','N',$80
.db -35,-19, 40, -32,-24, 40, -24,-24, 19, -16,-24, 12,  -8,-24,  9,   0,-24,  8,   8,-24,  9,  16,-24, 12,  24,-24, 19,  32,-24, 40,  35,-19, 40
.db -37,-16, 40, -32,-16, 22, -24,-16, 12, -16,-16,  7,  -8,-16,  4,   0,-16,  3,   8,-16,  4,  16,-16,  7,  24,-16, 12,  32,-16, 22,  37,-16, 40
.db -39, -8, 40, -32, -8, 17, -24, -8,  9, -16, -8,  4,  -8, -8,  2,   0, -8,  1,   8, -8,  2,  16, -8,  4,  24, -8,  9,  32, -8, 17,  39, -8, 40
.db -40,  0, 40, -32,  0, 16, -24,  0,  8, -16,  0,  3,  -8,  0, -3,   0,  0, -4,   8,  0, -3,  16,  0,  3,  24,  0,  8,  32,  0, 16,  40,  0, 40
.db -39,  8, 40, -32,  8, 17, -24,  8,  9, -16,  8,  0,  -8,  8,-19,   0,  8,-20,   8,  8,-19,  16,  8,  0,  24,  8,  9,  32,  8, 17,  39,  8, 40
.db -37, 16, 40, -32, 16, 22, -24, 16, 12, -16, 16,-34,  -8, 16,-38,   0, 16,-40,   8, 16,-38,  16, 16,-34,  24, 16, 12,  32, 16, 22,  37, 16, 40
.db -35, 19, 40, -32, 24, 40, -24, 24, 19, -16, 24,-34,  -8, 24,-38,   0, 24,-40,   8, 24,-38,  16, 24,-34,  24, 24, 19,  32, 24, 40,  35, 19, 40
.db 'L','N',$80, -28, 28, 40, -24, 32, 40, -16, 32, 22,  -8, 32, 17,   0, 32, 16,   8, 32, 17,  16, 32, 22,  24, 32, 40,  28, 28, 40, 'L','N',$80
.db 'L','N',$80, 'L','N',$80, -19, 35, 40, -16, 37, 40,  -8, 39, 40,   0, 40, 40,   8, 39, 40,  16, 37, 40,  19, 35, 40, 'L','N',$80, 'L','N',$80

PlatypusFlags:
.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.db $00,$00,$00,$FF,$00,$00,$FF,$00,$00,$00
.db $00,$00,$00,$00,$FF,$FF,$00,$00,$00,$00
.db $00,$00,$00,$FF,$FF,$FF,$FF,$00,$00,$00
.db $00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00
.db $00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00
.db $00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00
.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00

.pad (($ - 1) | $FF) + 1
BitTable:
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
.db %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001

DivTable:
.db	 56,	 56,	 56,	 56,	 56,	 56,	 55,	 55
.db	 55,	 55,	 55,	 55,	 54,	 54,	 54,	 54
.db	 54,	 54,	 53,	 53,	 53,	 53,	 53,	 53
.db	 52,	 52,	 52,	 52,	 52,	 52,	 51,	 51
.db	 51,	 51,	 51,	 51,	 50,	 50,	 50,	 50
.db	 50,	 50,	 49,	 49,	 49,	 49,	 49,	 49
.db	 49,	 48,	 48,	 48,	 48,	 48,	 48,	 47
.db	 47,	 47,	 47,	 47,	 47,	 46,	 46,	 46
.db	 46,	 46,	 46,	 45,	 45,	 45,	 45,	 45
.db	 45,	 44,	 44,	 44,	 44,	 44,	 44,	 43
.db	 43,	 43,	 43,	 43,	 43,	 42,	 42,	 42
.db	 42,	 42,	 42,	 41,	 41,	 41,	 41,	 41
.db	 41,	 40,	 40,	 40,	 40,	 40,	 40,	 39
.db	 39,	 39,	 39,	 39,	 39,	 38,	 38,	 38
.db	 38,	 38,	 38,	 37,	 37,	 37,	 37,	 37
.db	 37,	 36,	 36,	 36,	 36,	 36,	 36,	 35
.db	 77,	 76,	 76,	 76,	 76,	 76,	 76,	 75
.db	 75,	 75,	 75,	 75,	 75,	 74,	 74,	 74
.db	 74,	 74,	 74,	 73,	 73,	 73,	 73,	 73
.db	 73,	 72,	 72,	 72,	 72,	 72,	 72,	 71
.db	 71,	 71,	 71,	 71,	 71,	 70,	 70,	 70
.db	 70,	 70,	 70,	 69,	 69,	 69,	 69,	 69
.db	 69,	 68,	 68,	 68,	 68,	 68,	 68,	 67
.db	 67,	 67,	 67,	 67,	 67,	 66,	 66,	 66
.db	 66,	 66,	 66,	 66,	 66,	 66,	 66,	 66
.db	 66,	 66,	 65,	 65,	 65,	 65,	 65,	 65
.db	 64,	 64,	 64,	 64,	 64,	 64,	 63,	 63
.db	 63,	 63,	 63,	 63,	 62,	 62,	 62,	 62
.db	 62,	 62,	 61,	 61,	 61,	 61,	 61,	 61
.db	 60,	 60,	 60,	 60,	 60,	 60,	 59,	 59
.db	 59,	 59,	 59,	 59,	 58,	 58,	 58,	 58
.db	 58,	 58,	 57,	 57,	 57,	 57,	 57,	 57
.db	 84,	 84,	 84,	 84,	 83,	 83,	 83,	 83
.db	 82,	 82,	 82,	 82,	 81,	 81,	 81,	 81
.db	 80,	 80,	 80,	 80,	 79,	 79,	 79,	 79
.db	 78,	 78,	 78,	 78,	 77,	 77,	 77,	 77
.db	 76,	 76,	 76,	 76,	 75,	 75,	 75,	 75
.db	 74,	 74,	 74,	 74,	 74,	 73,	 73,	 73
.db	 73,	 72,	 72,	 72,	 72,	 71,	 71,	 71
.db	 71,	 70,	 70,	 70,	 70,	 69,	 69,	 69
.db	 69,	 68,	 68,	 68,	 68,	 67,	 67,	 67
.db	 67,	 66,	 66,	 66,	 66,	 65,	 65,	 65
.db	 65,	 64,	 64,	 64,	 64,	 63,	 63,	 63
.db	 63,	 62,	 62,	 62,	 62,	 61,	 61,	 61
.db	 61,	 60,	 60,	 60,	 60,	 59,	 59,	 59
.db	 59,	 58,	 58,	 58,	 58,	 57,	 57,	 57
.db	 57,	 56,	 56,	 56,	 56,	 55,	 55,	 55
.db	 55,	 54,	 54,	 54,	 54,	 53,	 53,	 53
.db	115,	115,	115,	114,	114,	114,	114,	113
.db	113,	113,	113,	112,	112,	112,	112,	111
.db	111,	111,	111,	110,	110,	110,	110,	109
.db	109,	109,	109,	108,	108,	108,	108,	107
.db	107,	107,	107,	106,	106,	106,	106,	105
.db	105,	105,	105,	104,	104,	104,	104,	103
.db	103,	103,	103,	102,	102,	102,	102,	101
.db	101,	101,	101,	100,	100,	100,	100,	 99
.db	 99,	 99,	 99,	 99,	 99,	 99,	 99,	 99
.db	 98,	 98,	 98,	 98,	 97,	 97,	 97,	 97
.db	 96,	 96,	 96,	 96,	 95,	 95,	 95,	 95
.db	 94,	 94,	 94,	 94,	 93,	 93,	 93,	 93
.db	 92,	 92,	 92,	 92,	 91,	 91,	 91,	 91
.db	 90,	 90,	 90,	 90,	 89,	 89,	 89,	 89
.db	 88,	 88,	 88,	 88,	 87,	 87,	 87,	 87
.db	 86,	 86,	 86,	 86,	 85,	 85,	 85,	 85

MulTable:
.db	  0,	  0,	  0,	  0,	  0,	  0,	  0,	  0
.db	  0,	  0,	  0,	  0,	  0,	  0,	  0,	  0
.db	  0,	  0,	  0,	  0,	  0,	  0,	  0,	  1
.db	  1,	  1,	  1,	  1,	  1,	  1,	  1,	  1
.db	  2,	  2,	  2,	  2,	  2,	  2,	  2,	  2
.db	  3,	  3,	  3,	  3,	  3,	  3,	  4,	  4
.db	  4,	  4,	  4,	  5,	  5,	  5,	  5,	  5
.db	  6,	  6,	  6,	  6,	  7,	  7,	  7,	  7
.db	  8,	  8,	  8,	  8,	  9,	  9,	  9,	  9
.db	 10,	 10,	 10,	 10,	 11,	 11,	 11,	 12
.db	 12,	 12,	 13,	 13,	 13,	 14,	 14,	 14
.db	 15,	 15,	 15,	 16,	 16,	 16,	 17,	 17
.db	 18,	 18,	 18,	 19,	 19,	 19,	 20,	 20
.db	 21,	 21,	 21,	 22,	 22,	 23,	 23,	 24
.db	 24,	 24,	 25,	 25,	 26,	 26,	 27,	 27
.db	 28,	 28,	 29,	 29,	 30,	 30,	 31,	 31
.db	 32,	 31,	 31,	 30,	 30,	 29,	 29,	 28
.db	 28,	 27,	 27,	 26,	 26,	 25,	 25,	 24
.db	 24,	 24,	 23,	 23,	 22,	 22,	 21,	 21
.db	 21,	 20,	 20,	 19,	 19,	 19,	 18,	 18
.db	 18,	 17,	 17,	 16,	 16,	 16,	 15,	 15
.db	 15,	 14,	 14,	 14,	 13,	 13,	 13,	 12
.db	 12,	 12,	 11,	 11,	 11,	 10,	 10,	 10
.db	 10,	  9,	  9,	  9,	  9,	  8,	  8,	  8
.db	  8,	  7,	  7,	  7,	  7,	  6,	  6,	  6
.db	  6,	  5,	  5,	  5,	  5,	  5,	  4,	  4
.db	  4,	  4,	  4,	  3,	  3,	  3,	  3,	  3
.db	  3,	  2,	  2,	  2,	  2,	  2,	  2,	  2
.db	  2,	  1,	  1,	  1,	  1,	  1,	  1,	  1
.db	  1,	  1,	  0,	  0,	  0,	  0,	  0,	  0
.db	  0,	  0,	  0,	  0,	  0,	  0,	  0,	  0
.db	  0,	  0,	  0,	  0,	  0,	  0,	  0,	  0
.db	  0,	  0,	  2,	  4,	  8,	 12,	 18,	 24
.db	 32,	 40,	 50,	 60,	 72,	 84,	 98,	112
.db	-128,	-112,	-94,	-76,	-56,	-36,	-14,	  8
.db	 32,	 56,	 82,	108,	-120,	-92,	-62,	-32
.db	  0,	 32,	 66,	100,	-120,	-84,	-46,	 -8
.db	 32,	 72,	114,	-100,	-56,	-12,	 34,	 80
.db	-128,	-80,	-30,	 20,	 72,	124,	-78,	-24
.db	 32,	 88,	-110,	-52,	  8,	 68,	-126,	-64
.db	  0,	 64,	-126,	-60,	  8,	 76,	-110,	-40
.db	 32,	104,	-78,	 -4,	 72,	-108,	-30,	 48
.db	-128,	-48,	 34,	116,	-56,	 28,	114,	-56
.db	 32,	120,	-46,	 44,	-120,	-28,	 66,	-96
.db	  0,	 96,	-62,	 36,	-120,	-20,	 82,	-72
.db	 32,	-120,	-14,	 92,	-56,	 52,	-94,	 16
.db	-128,	-16,	 98,	-44,	 72,	-68,	 50,	-88
.db	 32,	-104,	 18,	-116,	  8,	-124,	  2,	-128
.db	  0,	-128,	  2,	-124,	  8,	-116,	 18,	-104
.db	 32,	-88,	 50,	-68,	 72,	-44,	 98,	-16
.db	-128,	 16,	-94,	 52,	-56,	 92,	-14,	-120
.db	 32,	-72,	 82,	-20,	-120,	 36,	-62,	 96
.db	  0,	-96,	 66,	-28,	-120,	 44,	-46,	120
.db	 32,	-56,	114,	 28,	-56,	116,	 34,	-48
.db	-128,	 48,	-30,	-108,	 72,	 -4,	-78,	104
.db	 32,	-40,	-110,	 76,	  8,	-60,	-126,	 64
.db	  0,	-64,	-126,	 68,	  8,	-52,	-110,	 88
.db	 32,	-24,	-78,	124,	 72,	 20,	-30,	-80
.db	-128,	 80,	 34,	-12,	-56,	-100,	114,	 72
.db	 32,	 -8,	-46,	-84,	-120,	100,	 66,	 32
.db	  0,	-32,	-62,	-92,	-120,	108,	 82,	 56
.db	 32,	  8,	-14,	-36,	-56,	-76,	-94,	-112
.db	-128,	112,	 98,	 84,	 72,	 60,	 50,	 40
.db	 32,	 24,	 18,	 12,	  8,	  4,	  2,	  0
