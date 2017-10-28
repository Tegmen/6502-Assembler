Screen = 0x7800
Char = 0x7000

ORIGIN = 0x8000


==Init==
LDX #0x00
{
	LDA Gfx,x
	STA Char,x
	INX
	BEQ >
	JMP <
}
LDA #0x09
STA Screen
LDA #0x15
STA Screen+1
STA Screen+2
STA Screen+3
STA Screen+4
LDA #0x05
STA Screen+5
LDA #0x0F
STA Screen+9

JMP InfLoop

==InfLoop==
JMP InfLoop

@NPAGE
==Gfx==
DATA gfx.hex


==Frame== 
RTI

==Nmi==
RTI

@0xFFFA
DATA Nmi
DATA Init
DATA Frame
