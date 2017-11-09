;Constants
up_or_down =	2
left = 			0
right = 		1
down = 			2
up = 			3
head_gfx = 		4
tail_gfx = 		8
apple_gfx = 	12
start_dir = 	right

key_up = 		8
key_right = 	4
key_down = 		2
key_left = 		1


VARLOC 0x00
:move_dir = 	0x00
:move_pos = 	0x01
; hi		0x02
:move_x = 	0x03
:move_y = 	0x04
:head_dir = 	0x05
:head_pos =	0x06
; hi		0x07
h_dir_old = 0x08
tail_dir = 	0x09
tail_pos = 	0x0A
; hi		0x0B
apple_pos = 0x0C
; hi		0x0D
has_eaten = 0x0E

Start_pos = 0x7A0F

Char = 		0x7000
Screen = 	0x7800
Keys = 		0x7C00
Rand = 		0x7E00


ORIGIN = 	0x8000

==Init==
SEI
JSR Copy_chars
LDA #start_dir
STA head_dir
STA tail_dir
LDA #Start_pos.LO
STA head_pos
LDA #Start_pos.HI
STA head_pos+1
LDA #Start_pos.LO-5
STA tail_pos
LDA #Start_pos.HI
STA tail_pos+1


LDA #0x05
STA (head_pos),y
LDA #0x15
STA Start_pos-1
STA Start_pos-2
STA Start_pos-3
STA Start_pos-4
LDA #0x08
STA (tail_pos),y
JSR New_apple
CLI
JMP InfLoop



==Copy_chars==	;Copies the 32 characters into Character Memory
LDX #0x00
{
	LDA Gfx,x
	STA Char,x
	INX
	BEQ BR
	JMP RE
}
RTS

==Game_over==
SEI

==InfLoop==
JMP InfLoop


==Move==
LDA move_dir
AND #up_or_down
BEQ SK
;Move Vertical
{
	LDA move_pos
	AND #0b11100000
	STA move_y
	LDA move_pos
	AND #0b00011111
	STA move_x
	LDA move_dir
	CMP #up
	BEQ SK
	;Move down
	{
		LDA move_y
		CLC
		ADC #0b00100000
		ORA move_x
		STA move_pos
		LDA move_pos+1
		AND #0b11111100
		STA move_y
		LDA move_pos+1
		AND #0b00000011
		ADC #0
		AND #0b00000011
		ORA move_y
		STA move_pos+1
		JMP BR,SK
	}
	;Move up
	{
		LDA move_y
		SEC
		SBC #0b00100000
		AND #0b11100000
		ORA move_x
		STA move_pos
		LDA move_pos+1
		AND #0b11111100
		STA move_y
		LDA move_pos+1
		AND #0b00000011
		SBC #0
		AND #0b00000011
		ORA move_y
		STA move_pos+1	
	}
	JMP BR,SK
}
;Move Horizontal
{
	LDA move_pos
	ORA #0b11100000
	STA move_x
	LDA move_pos
	AND #0b11100000
	STA move_pos
	LDA move_dir
	CMP #left
	BEQ SK
	;Move right
	{
		INC move_x
		JMP BR,SK
	}
	;Move left
	{
		DEC move_x
	}
	LDA move_x
	AND #0b00011111
	ORA move_pos
	STA move_pos
}
RTS




==Frame==
LDA head_dir
STA h_dir_old 
JSR Key_check
JSR Head_move
JSR Tail_move

RTI

==Key_check==
LDA head_dir
AND #up_or_down
BEQ SK
;Up/Down
{
	LDA Keys
	AND #key_left
	BEQ SK
	{
		LDA #left
		STA head_dir
		JMP BR,SK
	}
	LDA Keys
	AND #key_right
	BEQ BR,SK
	{
		LDA #right
		STA head_dir
	}
	JMP BR,SK
}
{
	LDA Keys
	AND #key_down
	BEQ SK
	{
		LDA #down
		STA head_dir
		JMP BR,SK
	}
	LDA Keys
	AND #key_up
	BEQ BR
	{
		LDA #up
		STA head_dir
	}
}
RTS

==Head_move==
LDA h_dir_old
CLC
ROL A
ROL A
ORA #0x10
ORA head_dir
STA (head_pos),y
LDA head_pos
STA move_pos
LDA head_pos+1
STA move_pos+1
LDA head_dir
STA move_dir
JSR Move
LDA move_pos
STA head_pos
LDA move_pos+1
STA head_pos+1
LDA (head_pos),y
AND #0b11111100
CMP #apple_gfx
BNE SK
;Eat apple
{
	LDA #0x04
	STA has_eaten
	JSR New_apple
	JMP BR,SK
}
CMP #0x00
BEQ SK
; Collision
{
	JMP Game_over
}
LDA #head_gfx
ORA head_dir
STA (head_pos),y
RTS

==Tail_move==
LDA has_eaten

BEQ SK
;has eaten
{
	DEC has_eaten
	JMP BR,SK
}
; Not eaten, move tail
{
	LDA (tail_pos),y
	AND #0b11111100
	CMP #apple_gfx
	BEQ SK
	{
		LDA #0x00
		STA (tail_pos),y
	}
	LDA tail_dir
	STA move_dir
	LDA tail_pos
	STA move_pos
	LDA tail_pos+1
	STA move_pos+1
	JSR Move
	LDA move_pos
	STA tail_pos
	LDA move_pos+1
	STA tail_pos+1
	LDA (tail_pos),y
	AND #0b00000011
	STA tail_dir
	LDA (tail_pos),y
	AND #0b11111100
	CMP #apple_gfx
	BEQ SK
	{
		LDA tail_dir
		ORA #tail_gfx
		STA (tail_pos),y
	}
}
RTS

==New_apple==
LDA Rand
STA apple_pos
LDA Rand
AND #0b00000011
ORA #Screen.HI
STA apple_pos+1
LDA #0x12
LDA (apple_pos),y
AND #0b00000011
ORA #apple_gfx
STA (apple_pos),y
RTS


@NPAGE
==Gfx==
DATA gfx.hex

==Nmi==
RTI

@0xFFFA
DATA Nmi
DATA Init
DATA Frame
