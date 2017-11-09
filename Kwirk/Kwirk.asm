@0x00
VAR new_frame
VAR pos[2]
VAR pos_new[2]
VAR dir
VAR right
VAR back
VAR left
VAR tile[2]
VAR tile_in_dir[2]
VAR front_left[2]
VAR marked_tile[2]
VAR can_move
VAR all_covering
VAR is_last_row
VAR clockwise
VAR check_dir
VAR level
VAR lvl_data[2]
VAR lvl_bin
VAR lvl_bcd[2]
VAR image[2]
VAR cheat

Char_mem	= 0x7000
Screen		= 0x7800
Print_char 	= 0x7C01
Cursor_x	= 0x7C02
Cursor_y	= 0x7C03
Joypad		= 0x7D01
Rand 		= 0x7E00
Pitch		= 0x7F00
Volume		= 0x7F01
Delay		= 0x7F02
Play		= 0x7F03


;Constants
mask_arrow	= 0b00001111
key_up		= 0b00000001
key_right	= 0b00000010
key_down	= 0b00000100
key_left	= 0b00001000
key_w		= 0b00010000
key_d		= 0b00100000
key_s		= 0b01000000
key_a		= 0b10000000
key_cheat	= 0b10110000

north		= 1
east		= 2
south		= 4
west		= 8

empty 		= 0x80
is_block	= 0x90
face		= 0xB0
flipper_n	= 0xB1
flipper_e	= 0xB2
stairs		= 0xB3
flipper_s	= 0xB4
brick		= 0xB7
flipper_w	= 0xB8

success		= 25







ORIGIN = 	0x8000
==Init==
SEI
LDY #0
JSR Copy_chars
LDA #Title.LO
STA image
LDA #Title.HI
STA image+1
JSR Draw_Image
LDA #90
JSR Wait
JSR All_Brick
JSR Load_level
JMP Step

==Copy_chars==
LDX #0x00
{
	LDA Char,x
	STA Char_mem+1024,x
	LDA Char+256,x
	STA Char_mem+1280,x
	LDA Char+512,x
	STA Char_mem+1536,x
	LDA Char+768,x
	STA Char_mem+1792,x
	INX
	BEQ BR
	JMP RE
}
RTS




==Step==
JSR Wait_for_key
LDA cheat
BEQ SK
{
	JSR Sound_Success
	LDA #0
	STA cheat
	LDA #5
	JSR Wait
	JSR Next_level
	JMP Step 
}

LDA (pos_new),y
CMP #empty
BNE SK
{
	JSR Move_player
	JMP Step
}

CMP #is_block
BCS SK
{
	JSR Try_move
	BEQ SK
	{
		JSR Move_player
		JSR Sound_Move
		JMP Step
	}
	JSR Sound_Bump
	JMP Step
}
CMP #stairs
BNE SK
{
	JSR Sound_Success
	JSR Next_level
	JMP Step
}
CMP #flipper_n
BNE SK
{
	LDA dir
	CMP #south
	BEQ SK
	{
		AND #east
		STA clockwise
		LDX #south
		JSR Try_rotate_flipper
		BNE SK
		{
			JSR Sound_Bump
			JMP Step
		}
		LDA (pos_new),y
		CMP #empty
		BEQ SK
		{
			LDA pos_new
			STA tile
			LDA pos_new+1
			STA tile+1
			LDA dir
			JSR Get_tile
			LDA tile
			STA pos_new
			LDA tile+1
			STA pos_new+1
		}
		JSR Sound_Move
		JSR Move_player
	}
	JMP Step
}
CMP #flipper_e
BNE SK
{
	LDA dir
	CMP #west
	BEQ SK
	{
		AND #south
		STA clockwise
		LDX #west
		JSR Try_rotate_flipper
		BNE SK
		{
			JSR Sound_Bump
			JMP Step
		}
		LDA (pos_new),y
		CMP #empty
		BEQ SK
		{
			LDA pos_new
			STA tile
			LDA pos_new+1
			STA tile+1
			LDA dir
			JSR Get_tile
			LDA tile
			STA pos_new
			LDA tile+1
			STA pos_new+1
		}
		JSR Sound_Move
		JSR Move_player
	}
	JMP Step
}
CMP #flipper_s
BNE SK
{
	LDA dir
	CMP #north
	BEQ SK
	{
		AND #west
		STA clockwise
		LDX #north
		JSR Try_rotate_flipper
		BNE SK
		{
			JSR Sound_Bump
			JMP Step
		}
		LDA (pos_new),y
		CMP #empty
		BEQ SK
		{
			LDA pos_new
			STA tile
			LDA pos_new+1
			STA tile+1
			LDA dir
			JSR Get_tile
			LDA tile
			STA pos_new
			LDA tile+1
			STA pos_new+1
		}
		JSR Sound_Move
		JSR Move_player
	}
	JMP Step
}
CMP #flipper_w
BNE SK
{
	LDA dir
	CMP #east
	BEQ SK
	{
		AND #north
		STA clockwise
		LDX #east
		JSR Try_rotate_flipper
		BNE SK
		{
			JSR Sound_Bump
			JMP Step
		}
		LDA (pos_new),y
		CMP #empty
		BEQ SK
		{
			LDA pos_new
			STA tile
			LDA pos_new+1
			STA tile+1
			LDA dir
			JSR Get_tile
			LDA tile
			STA pos_new
			LDA tile+1
			STA pos_new+1
		}
		JSR Sound_Move
		JSR Move_player
	}
	JMP Step
}	
JMP Step


==Wait_for_key==
LDA Joypad
CMP #key_cheat
BNE SK
{
	LDA #1
	STA cheat
	RTS
}
LDA Joypad
AND #key_s
BEQ SK
{
	JSR Load_level
}
LDA Joypad
AND #mask_arrow
BEQ	Wait_for_key
{
	CMP #key_up
	BNE SK
	{
		JSR Set_dir_north
		RTS
	}
	CMP #key_right
	BNE SK
	{
		JSR Set_dir_east
		RTS
	}
	CMP #key_down
	BNE SK
	{
		JSR Set_dir_south
		RTS
	}
	CMP #key_left
	BNE SK
	{
		JSR Set_dir_west
		RTS
	}
}
JMP Wait_for_key

==Move_player==
LDA #empty
STA (pos),y
LDA #face
STA (pos_new),y
LDA pos_new
STA pos
LDA pos_new+1
STA pos+1

LDA #4
JSR Wait
RTS


==Set_dir_north==
LDA #north
STA dir
LDA #east
STA right
LDA #south
STA back
LDA #west
STA left
LDA pos
SEC
SBC #32
STA pos_new
LDA pos+1
SBC #0
STA pos_new+1
RTS

==Set_dir_east==
LDA #east
STA dir
LDA #south
STA right
LDA #west
STA back
LDA #north
STA left
LDA pos
CLC
ADC #1
STA pos_new
LDA pos+1
STA pos_new+1
RTS


==Set_dir_south==
LDA #south
STA dir
LDA #west
STA right
LDA #north
STA back
LDA #east
STA left
LDA pos
CLC
ADC #0x20
STA pos_new
LDA pos+1
ADC #0
STA pos_new+1
RTS

==Set_dir_west==
LDA #west
STA dir
LDA #north
STA right
LDA #east
STA back
LDA #south
STA left
LDA pos
SEC
SBC #1
STA pos_new
LDA pos+1
STA pos_new+1
RTS

==Try_move==
JSR Get_bottom_right
JSR Check_move_block
LDA can_move
BEQ SK
{
	JSR Move_block
	LDA #1
	RTS
}

LDA #0
RTS

==Get_bottom_right==
LDA pos_new
STA tile
LDA pos_new+1
STA tile+1
LDA dir
{	
	CMP #north
	BNE SK
	{
		LDA (tile),y
		AND #east
		BNE BR,BR
		INC tile
		JMP RE
	}
	CMP #east
	BNE SK
	{
		LDA (tile),y
		AND #south
		BNE BR,BR
		CLC
		LDA tile
		ADC #32
		STA tile
		LDA tile+1
		ADC #0
		STA tile+1
		JMP RE
	}
	CMP #south
	BNE SK
	{
		LDA (tile),y
		AND #west
		BNE BR,BR
		DEC tile
		JMP RE
	}
	CMP #west
	BNE SK
	{
		LDA (tile),y
		AND #north
		BNE BR,BR
		SEC
		LDA tile
		SBC #32
		STA tile
		LDA tile+1
		SBC #0
		STA tile+1
		JMP RE
	}
}
LDA tile
STA marked_tile
LDA tile+1
STA marked_tile+1
RTS

==Check_move_block==
LDA #1
STA can_move
STA all_covering
Check_loop:
{
	JSR Get_tile_in_dir
	LDA (tile),y
	AND dir
	BEQ SK
	{
		LDA (tile_in_dir),y
		ORA #0b00010000
		CMP #0b10010000
		BEQ SK
		{
			LDA #0
			STA can_move
		}
	}
	LDA (tile_in_dir),y
	AND #0b11110000
	CMP #0b10010000
	BEQ SK
	{
		LDA #0
		STA all_covering
	}
	
	LDA (tile),y
	AND left
	BNE	SK						
	{							;If not at left end of tile
		LDA left
		JSR Get_tile
		JMP Check_loop
	}
	{							;If at left end of tiles
		LDA (marked_tile),y
		AND dir
		BNE SK
		{						;Not yet at front left corner
			;
			LDA marked_tile
			STA tile
			LDA marked_tile+1
			STA tile+1
			;
			LDA dir
			JSR Get_tile
			LDA tile
			STA marked_tile
			LDA tile+1
			STA marked_tile+1
			JMP Check_loop
		}
		LDA tile
		STA marked_tile
		LDA tile+1
		STA marked_tile+1
	}
}
RTS


==Move_block==
LDA (marked_tile),y
AND back
STA is_last_row


Move_block_loop:
{
	JSR Get_tile_in_dir
	LDA all_covering
	BEQ SK
	{
		LDA #0x80				;empty tile
		STA (tile_in_dir),y
		JMP BR,SK
	}
	{
		LDA (tile_in_dir),y
		AND #0b10010000
		STA (tile_in_dir),y
		LDA (tile),y
		AND #0b11101111
		ORA (tile_in_dir),y
		STA (tile_in_dir),y	
	}
	
	LDA (tile),y
	TAX
	AND #0b10010000				;Only empty or hole
	STA (tile),y			
	
	TXA
	AND right
	BNE	SK						
	{							;If not at right end of tiles
		LDA right
		JSR Get_tile
		JMP Move_block_loop
	}
	{							;If at right end of tiles
		LDA is_last_row
		BNE SK
		{	
			;Not yet at back right corner
			LDA marked_tile
			STA tile
			LDA marked_tile+1
			STA tile+1
			
			LDA back
			JSR Get_tile
			LDA (tile),y
			AND back
			STA is_last_row
			
			LDA tile
			STA marked_tile
			LDA tile+1
			STA marked_tile+1
			
			JMP Move_block_loop
		}
	}
}
RTS



==Get_tile==
CMP #east
BNE SK
{
	INC tile
	RTS
}
CMP #south
BNE SK
{
	LDA tile
	CLC
	ADC #32
	STA tile
	LDA tile+1
	ADC #0
	STA tile+1
	RTS
}
CMP #west
BNE SK
{
	DEC tile
	RTS
}
CMP #north
BNE SK
{
	LDA tile
	SEC
	SBC #32
	STA tile
	LDA tile+1
	SBC #0
	STA tile+1
	RTS
}
RTS



==Block_can_move==
JSR Get_tile_in_dir
LDA (tile_in_dir),y
CMP #0x10
BCS SK
{
	LDA #0
	STA all_covering
}
LDA (tile),y
AND dir
BEQ SK
{
	LDA (tile_in_dir),y
	AND #0b01101111
	BEQ SK
	{
		LDA #0
		STA can_move
	}
}
RTS


==Get_tile_in_dir==
LDA dir
CMP #north
BNE SK
{
	LDA tile
	SEC
	SBC #32
	STA tile_in_dir
	LDA tile+1
	SBC #0
	STA tile_in_dir+1
	RTS
}
CMP #east
BNE SK
{
	LDA tile
	CLC
	ADC #1
	STA tile_in_dir
	LDA tile+1
	STA tile_in_dir+1
	RTS
}
CMP #south
BNE SK
{
	LDA tile
	CLC
	ADC #32
	STA tile_in_dir
	LDA tile+1
	ADC #0
	STA tile_in_dir+1
	RTS
}
CMP #west
BNE SK
{
	LDA tile
	SEC
	SBC #1
	STA tile_in_dir
	LDA tile+1
	STA tile_in_dir+1
	RTS
}
LDA #0x4D
STA Screen
RTS

==Try_rotate_flipper==
LDA pos_new
STA tile
LDA pos_new+1
STA tile+1
TXA
JSR Get_tile
LDA tile
STA marked_tile
LDA tile+1
STA marked_tile+1
JSR Check_can_rotate
LDA can_move
BNE SK
{
	LDA #0
	RTS
}
JSR Rotate_flipper
LDA #1
RTS



==Check_can_rotate==
LDA #0
STA can_move
LDA #north
STA check_dir
{
	AND (marked_tile),y
	BEQ SK
	{	
		LDA check_dir
		JSR Rotate_acc
		JSR Get_tile
		LDA (tile),y
		AND #0b00001111
		BEQ SK
		{	
			LDA check_dir
			JSR Rotate_acc
			ORA #0xB0
			CMP (tile),y
			BEQ BR
			RTS
		}
		LDA check_dir
		JSR Get_tile
		LDA (tile),y
		AND #0b00001111
		BEQ SK
		{
			RTS
		}
		LDA marked_tile
		STA tile
		LDA marked_tile+1
		STA tile+1
	}
	ASL check_dir
	LDA check_dir
	CMP #0x10
	BCC RE
}


LDA #1
STA can_move
RTS

==Rotate_flipper==
LDA marked_tile
STA tile
LDA marked_tile+1
STA tile+1
LDA #north
STA check_dir
{
	AND (marked_tile),y
	BEQ SK
	{	
		LDA check_dir
		JSR Get_tile
		LDA #empty
		STA (tile),y
		LDA marked_tile
		STA tile
		LDA marked_tile+1
		STA tile+1
	}
	ASL check_dir
	LDA check_dir
	CMP #0x10
	BNE RE
}
LDA (marked_tile),y
AND #0x0F
JSR Rotate_acc
ORA #0xA0
STA (marked_tile),y
LDA #north
STA check_dir
{
	AND (marked_tile),y
	BEQ SK
	{	
		LDA check_dir
		JSR Get_tile
		LDA check_dir
		ORA #0xB0
		STA (tile),y
		LDA marked_tile
		STA tile
		LDA marked_tile+1
		STA tile+1
	}
	ASL check_dir
	LDA check_dir
	CMP #0x10
	BNE RE
}
RTS

==Rotate_acc==
LDX clockwise
BEQ SK
{
	ASL A
	CMP #0x10
	BCC SK
	{
		AND #0b00001111
		ORA #0b00000001
	}
	RTS
}
CLC
ROR A
BCC SK
{
	ORA #0b00001000
}
RTS

==Load_level==
CLC
LDA #Level.HI
ADC level
STA lvl_data+1
LDA #8
STA Cursor_y

LDY #0x00
{
	LDA #8
	STA Cursor_x
	{
		LDA (lvl_data),y
		CMP #face
		BNE SK
		{
			JSR Cursor_to_pos
			LDA #face
		}
		STA Print_char
		INY
		LDA Cursor_x
		CMP #24
		BNE RE
	}
	INC Cursor_y
	LDA Cursor_y
	CMP #24
	BNE RE
}
JSR Write_level
RTS

==Cursor_to_pos==
LDA Cursor_y
ASL A
ASL A
ASL A
ASL A
ASL A
ORA Cursor_x
STA pos
LDA Cursor_y
AND #0b00011000
CLC
ROR A
ROR A
ROR A
ORA #Screen.HI
STA pos+1
RTS

==Next_level==
INC level
LDA level
CMP #success
BNE	SK
{
	LDA #Success.LO
	STA image
	LDA #Success.HI
	STA image+1
	JSR Draw_Image
	{
		JMP RE
	}
}
JSR Load_level
RTS

==Write_level==

SED				; Switch to decimal mode
LDA #0			; Ensure the result is clear
STA lvl_bcd
STA lvl_bcd+1
LDA level
STA lvl_bin
INC lvl_bin
LDX #8			; The number of source bits
{
	ASL lvl_bin		; Shift out one bit
	LDA lvl_bcd	; And add into result
	ADC lvl_bcd
	STA lvl_bcd
	LDA lvl_bcd+1	; propagating any carry
	ADC lvl_bcd+1
	STA lvl_bcd+1
	DEX			; And repeat for next bit
	BNE RE
	CLD			; Back to binary
}

LDA #0x4C
STA Screen+76
STA Screen+80
LDA #0x45
STA Screen+77
STA Screen+79
LDA #0x56
STA Screen+78
LDA #0x20
STA Screen+81
LDA lvl_bcd
AND #0b11110000
CLC
ROR A
ROR A
ROR A
ROR A
ORA #0x30
STA Screen+82
LDA lvl_bcd
AND #0b00001111
ORA #0x30
STA Screen+83
RTS

==Wait==
CLI
STA new_frame
{
	LDA new_frame
	BEQ BR
	JMP RE
}
RTS

==Frame==
DEC new_frame
RTI

==All_Brick==
LDX #0
STX Cursor_x
STX Cursor_y
LDA #brick
{
	STA Print_char
	STA Print_char
	STA Print_char
	STA Print_char
	INX
	BNE RE
}
RTS

==Draw_Image==
LDY #0
STY Cursor_x
STY Cursor_y
LDX #4
{
	{
		LDA (image),y
		STA Print_char
		INY
		BNE RE
	}
	INC image+1
	DEX
	BNE RE
}
RTS

==Sound_Bump==
LDA #40
STA Pitch
LDA #5
STA Play
LDA #5
JSR Wait
RTS

==Sound_Move==
LDA #50
STA Pitch
LDA #3
STA Play
LDA #70
STA Pitch
LDA #3
STA Delay
STA Play
LDA #0
STA Delay
RTS

==Sound_Success==
LDA #60
STA Pitch
LDA #5
STA Play
LDA #61
STA Pitch
LDA #5
STA Delay
STA Play
LDA #62
STA Pitch
LDA #10
STA Delay
LDA #5
STA Play
LDA #63
STA Pitch
LDA #15
STA Delay
STA Play
LDA #0
STA Delay
RTS

==Nmi==
RTI

@NPAGE
==Char==
DATA Char.hex

@NPAGE
==Title==
DATA Title.hex
==Success==
DATA Success.hex

@NPAGE
==Level==
DATA Levels/Level0.hex
DATA Levels/Level1.hex
DATA Levels/Level2.hex
DATA Levels/Level3.hex
DATA Levels/Level4.hex
DATA Levels/Level5.hex
DATA Levels/Level6.hex
DATA Levels/Level7.hex
DATA Levels/Level8.hex
DATA Levels/Level9.hex
DATA Levels/Level10.hex
DATA Levels/Level11.hex
DATA Levels/Level12.hex
DATA Levels/Level13.hex
DATA Levels/Level14.hex
DATA Levels/Level15.hex
DATA Levels/Level16.hex
DATA Levels/Level17.hex
DATA Levels/Level18.hex
DATA Levels/Level19.hex
DATA Levels/Level20.hex
DATA Levels/Level21.hex
DATA Levels/Level22.hex
DATA Levels/Level23.hex
DATA Levels/Level24.hex

@0xFFFA
DATA Nmi
DATA Init
DATA Frame