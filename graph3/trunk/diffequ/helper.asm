;Temporary file for function shared between 3d and DEQ

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
