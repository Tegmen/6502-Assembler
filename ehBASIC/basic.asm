
; Enhanced BASIC to assemble under 6502 simulator, 0xver 2.22

; 0xE7E1 0xE7CF 0xE7C6 0xE7D3 0xE7D1 0xE7D5 0xE7CF 0xE81E 0xE825

; 2.00	new revision numbers start here
; 2.01	fixed LCASE0x() and UCASE0x()
; 2.02	new get value routine done
; 2.03	changed RND() to galoise method
; 2.04	fixed SPC()
; 2.05	new get value routine fixed
; 2.06	changed USR() code
; 2.07	fixed STR0x()
; 2.08	changed INPUT and READ to remove need for 0x00 start to input buffer
; 2.09	fixed RND()
; 2.10	integrated missed changes from an earlier version
; 2.20	added ELSE to IF .. THEN and fixed IF .. GOTO statement.LO> to cause error
; 2.21	fixed IF .. THEN RETURN to not cause error
; 2.22	fixed RND() breaking the get byte routine

; zero page use ..

lab_warm 	= 0x00		; BASIC warm start entry point
wrmjpl 		= lab_warm+1; BASIC warm start vector jump low byte
wrmjph 		= lab_warm+2; BASIC warm start vector jump high byte

usrjmp		= 0x0A		; USR function JMP address
usrjpl		= usrjmp+1	; USR function JMP vector low byte
usrjph		= usrjmp+2	; USR function JMP vector high byte
nullct		= 0x0D		; nulls output after each line
tPos		= 0x0E		; BASIC terminal position byte
tWidth		= 0x0F		; BASIC terminal width byte
iclim		= 0x10		; input column limit
itempl		= 0x11		; temporary integer low byte
itemph		= itempl+1	; temporary integer high byte

nums_1		= itempl	; number to bin/hex string convert MSB
nums_2		= nums_1+1	; number to bin/hex string convert
nums_3		= nums_1+2	; number to bin/hex string convert LSB

srchc		= 0x5B		; search character
temp3		= srchc		; temp byte used in number routines
scnquo		= 0x5C		; scan-between-quotes flag
asrch		= scnquo	; alt search character

xoaw_l		= srchc		; eXclusive OR, OR and AND word low byte
xoaw_h		= scnquo	; eXclusive OR, OR and AND word high byte

ibptr		= 0x5D		; input buffer pointer
dimcnt		= ibptr		; # of dimensions
tindx		= ibptr		; token index

defdim		= 0x5E		; default DIM flag
dtypef		= 0x5F		; data type flag, 0xFF=string, 0x00=numeric
oquote		= 0x60		; open quote flag (b7) (Flag: DATA scan; LIST quote; memory)
gclctd		= 0x60		; garbage collected flag
sufnxf		= 0x61		; subscript/FNX flag, 1xxx xxx = FN(0xxx xxx)
imode		= 0x62		; input mode flag, 0x00=INPUT, 0x80=READ

cflag		= 0x63		; comparison evaluation flag

tabsiz		= 0x64		; TAB step size (was input flag)

next_s		= 0x65		; next descriptor stack address

						; these two bytes form a word pointer to the item
						; currently on top of the descriptor stack
last_sl		= 0x66		; last descriptor stack address low byte
last_sh		= 0x67		; last descriptor stack address high byte (always 0x00)

des_sk		= 0x68		; descriptor stack start address (temp strings)

;			= 0x70		; End of descriptor stack

ut1_pl		= 0x71		; utility pointer 1 low byte
ut1_ph		= ut1_pl+1	; utility pointer 1 high byte
ut2_pl		= 0x73		; utility pointer 2 low byte
ut2_ph		= ut2_pl+1	; utility pointer 2 high byte

temp_2		= ut1_pl	; temp byte for block move	

fact_1		= 0x75		; FAC temp mantissa1
fact_2		= fact_1+1	; FAC temp mantissa2
fact_3		= fact_2+1	; FAC temp mantissa3

dims_l		= fact_2	; array dimension size low byte
dims_h		= fact_3	; array dimension size high byte

tempb		= 0x78		; temp page 0 byte

smeml		= 0x79		; start of mem low byte		(Start-of-Basic)
smemh		= smeml+1	; start of mem high byte	(Start-of-Basic)
svarl		= 0x7B		; start of vars low byte	(Start-of-Variables)
svarh		= svarl+1	; start of vars high byte	(Start-of-Variables)
sarryl		= 0x7D		; var mem end low byte		(Start-of-Arrays)
sarryh		= sarryl+1	; var mem end high byte		(Start-of-Arrays)
earryl		= 0x7F		; array mem end low byte	(End-of-Arrays)
earryh		= earryl+1	; array mem end high byte	(End-of-Arrays)
sstorl		= 0x81		; string storage low byte	(String storage (moving down))
sstorh		= sstorl+1	; string storage high byte	(String storage (moving down))
sutill		= 0x83		; string utility ptr low byte
sutilh		= sutill+1	; string utility ptr high byte
ememl		= 0x85		; end of mem low byte		(Limit-of-memory)
ememh		= ememl+1	; end of mem high byte		(Limit-of-memory)
clinel		= 0x87		; current line low byte		(Basic line number)
clineh		= clinel+1	; current line high byte	(Basic line number)
blinel		= 0x89		; break line low byte		(Previous Basic line number)
blineh		= blinel+1	; break line high byte		(Previous Basic line number)

cpntrl		= 0x8B		; continue pointer low byte
cpntrh		= cpntrl+1	; continue pointer high byte

dlinel		= 0x8D		; current DATA line low byte
dlineh		= dlinel+1	; current DATA line high byte

dptrl		= 0x8F		; DATA pointer low byte
dptrh		= dptrl+1	; DATA pointer high byte

rdptrl		= 0x91		; read pointer low byte
rdptrh		= rdptrl+1	; read pointer high byte

varnm1		= 0x93		; current var name 1st byte
varnm2		= varnm1+1	; current var name 2nd byte

cvaral		= 0x95		; current var address low byte
cvarah		= cvaral+1	; current var address high byte

frnxtl		= 0x97		; var pointer for FOR/NEXT low byte
frnxth		= frnxtl+1	; var pointer for FOR/NEXT high byte

tidx1		= frnxtl	; temp line index

lvarpl		= frnxtl	; let var pointer low byte
lvarph		= frnxth	; let var pointer high byte

prstk		= 0x99		; precedence stacked flag

comp_f		= 0x9B		; compare function flag, bits 0,1 and 2 used
					; bit 2 set if >
					; bit 1 set if =
					; bit 0 set if <

func_l		= 0x9C		; function pointer low byte
func_h		= func_l+1	; function pointer high byte

garb_l		= func_l	; garbage collection working pointer low byte
garb_h		= func_h	; garbage collection working pointer high byte

des_2l		= 0x9E		; string descriptor_2 pointer low byte
des_2h		= des_2l+1	; string descriptor_2 pointer high byte

g_step		= 0xA0		; garbage collect step size

fnxjmp		= 0xA1		; jump vector for functions
fnxjpl		= fnxjmp+1	; functions jump vector low byte
fnxjph		= fnxjmp+2	; functions jump vector high byte

g_indx		= fnxjpl	; garbage collect temp index

fac2_r		= 0xA3		; FAC2 rounding byte

adatal		= 0xA4		; array data pointer low byte
adatah		= adatal+1	; array data pointer high  byte

nbendl		= adatal	; new block end pointer low byte
nbendh		= adatah	; new block end pointer high  byte

obendl		= 0xA6		; old block end pointer low byte
obendh		= obendl+1	; old block end pointer high  byte

numexp		= 0xA8		; string to float number exponent count
expcnt		= 0xA9		; string to float exponent count

numbit		= numexp	; bit count for array element calculations

numdpf		= 0xAA		; string to float decimal point flag
expneg		= 0xAB		; string to float eval exponent -ve flag

astrtl		= numdpf	; array start pointer low byte
astrth		= expneg	; array start pointer high  byte

histrl		= numdpf	; highest string low byte
histrh		= expneg	; highest string high  byte

baslnl		= numdpf	; BASIC search line pointer low byte
baslnh		= expneg	; BASIC search line pointer high  byte

fvar_l		= numdpf	; find/found variable pointer low byte
fvar_h		= expneg	; find/found variable pointer high  byte

ostrtl		= numdpf	; old block start pointer low byte
ostrth		= expneg	; old block start pointer high  byte

vrschl		= numdpf	; variable search pointer low byte
vrschh		= expneg	; variable search pointer high  byte

fac1_e		= 0xAC		; FAC1 exponent
fac1_1		= fac1_e+1	; FAC1 mantissa1
fac1_2		= fac1_e+2	; FAC1 mantissa2
fac1_3		= fac1_e+3	; FAC1 mantissa3
fac1_s		= fac1_e+4	; FAC1 sign (b7)

str_ln		= fac1_e	; string length
str_pl		= fac1_1	; string pointer low byte
str_ph		= fac1_2	; string pointer high byte

des_pl		= fac1_2	; string descriptor pointer low byte
des_ph		= fac1_3	; string descriptor pointer high byte

mids_l		= fac1_3	; MID0x string temp length byte

negnum		= 0xB1		; string to float eval -ve flag
numcon		= 0xB1		; series evaluation constant count

fac1_o		= 0xB2		; FAC1 overflow byte

fac2_e		= 0xB3		; FAC2 exponent
fac2_1		= fac2_e+1	; FAC2 mantissa1
fac2_2		= fac2_e+2	; FAC2 mantissa2
fac2_3		= fac2_e+3	; FAC2 mantissa3
fac2_s		= fac2_e+4	; FAC2 sign (b7)

fac_sc		= 0xB8		; FAC sign comparison, Acc#1 vs #2
fac1_r		= 0xB9		; FAC1 rounding byte

ssptr_l		= fac_sc	; string start pointer low byte
ssptr_h		= fac1_r	; string start pointer high byte

sdescr		= fac_sc	; string descriptor pointer

csidx		= 0xBA		; line crunch save index
asptl		= csidx	; array size/pointer low byte
aspth		= 0xBB		; array size/pointer high byte

btmpl		= asptl	; BASIC pointer temp low byte
btmph		= aspth	; BASIC pointer temp low byte

cptrl		= asptl	; BASIC pointer temp low byte
cptrh		= aspth	; BASIC pointer temp low byte

sendl		= asptl	; BASIC pointer temp low byte
sendh		= aspth	; BASIC pointer temp low byte

lab_igby	= 0xBC		; get next BASIC byte subroutine

Lab_gbyt	= 0x00C2		; get current BASIC byte subroutine
bpntrl		= 0xC3		; BASIC execute (get byte) pointer low byte
bpntrh		= bpntrl+1	; BASIC execute (get byte) pointer high byte

;			= 0xD7		; end of get BASIC char subroutine

rbyte4		= 0xD8		; extra PRNG byte
rbyte1		= rbyte4+1	; most significant PRNG byte
rbyte2		= rbyte4+2	; middle PRNG byte
rbyte3		= rbyte4+3	; least significant PRNG byte

nmibase		= 0xDC		; NMI handler enabled/setup/triggered flags
						; bit	function
						; ===	========
						; 7	interrupt enabled
						; 6	interrupt setup
						; 5	interrupt happened
;			= 0xDD		; NMI handler addr low byte
;			= 0xDE		; NMI handler addr high byte
irqbase		= 0xDF		; IRQ handler enabled/setup/triggered flags
;			= 0xE0		; IRQ handler addr low byte
;			= 0xE1		; IRQ handler addr high byte

;			= 0xDE		; unused
;			= 0xDF		; unused
;			= 0xE0		; unused
;			= 0xE1		; unused
;			= 0xE2		; unused
;			= 0xE3		; unused
;			= 0xE4		; unused
;			= 0xE5		; unused
;			= 0xE6		; unused
;			= 0xE7		; unused
;			= 0xE8		; unused
;			= 0xE9		; unused
;			= 0xEA		; unused
;			= 0xEB		; unused
;			= 0xEC		; unused
;			= 0xED		; unused
;			= 0xEE		; unused

decss			= 0xEF		; number to decimal string start
decssp1		= decss+1	; number to decimal string start

;			= 0xFF		; decimal string end

; token values needed for BASIC

; primary command tokens (can start a statement)

tk_END		= 0x80			; END token
tk_FOR		= tk_END+1		; FOR token
tk_NEXT		= tk_FOR+1		; NEXT token
tk_DATA		= tk_NEXT+1		; DATA token
tk_INPUT	= tk_DATA+1		; INPUT token
tk_DIM		= tk_INPUT+1	; DIM token
tk_READ		= tk_DIM+1		; READ token
tk_LET		= tk_READ+1		; LET token
tk_DEC		= tk_LET+1		; DEC token
tk_GOTO		= tk_DEC+1		; GOTO token
tk_RUN		= tk_GOTO+1		; RUN token
tk_IF		= tk_RUN+1		; IF token
tk_RESTORE	= tk_IF+1		; RESTORE token
tk_GOSUB	= tk_RESTORE+1	; GOSUB token
tk_RETIRQ	= tk_GOSUB+1	; RETIRQ token
tk_RETNMI	= tk_RETIRQ+1	; RETNMI token
tk_RETURN	= tk_RETNMI+1	; RETURN token
tk_REM		= tk_RETURN+1	; REM token
tk_STOP		= tk_REM+1		; STOP token
tk_ON		= tk_STOP+1		; ON token
tk_NULL		= tk_ON+1		; NULL token
tk_INC		= tk_NULL+1		; INC token
tk_WAIT		= tk_INC+1		; WAIT token
tk_LOAD		= tk_WAIT+1		; LOAD token
tk_SAVE		= tk_LOAD+1		; SAVE token
tk_DEF		= tk_SAVE+1		; DEF token
tk_POKE		= tk_DEF+1		; POKE token
tk_DOKE		= tk_POKE+1		; DOKE token
tk_CALL		= tk_DOKE+1		; CALL token
tk_DO		= tk_CALL+1		; DO token
tk_LOOP		= tk_DO+1		; LOOP token
tk_PRINT	= tk_LOOP+1		; PRINT token
tk_CONT		= tk_PRINT+1	; CONT token
tk_LIST		= tk_CONT+1		; LIST token
tk_CLEAR	= tk_LIST+1		; CLEAR token
tk_NEW		= tk_CLEAR+1	; NEW token
tk_WIDTH	= tk_NEW+1		; WIDTH token
tk_GET		= tk_WIDTH+1	; GET token
tk_SWAP		= tk_GET+1		; SWAP token
tk_BITSET	= tk_SWAP+1		; BITSET token
tk_BITCLR	= tk_BITSET+1	; BITCLR token
tk_IRQ		= tk_BITCLR+1	; IRQ token
tk_NMI		= tk_IRQ+1		; NMI token

; secondary command tokens, can't start a statement

tk_TAB		= tk_NMI+1		; TAB token
tk_ELSE		= tk_TAB+1		; ELSE token
tk_TO		= tk_ELSE+1		; TO token
tk_FN		= tk_TO+1		; FN token
tk_SPC		= tk_FN+1		; SPC token
tk_THEN		= tk_SPC+1		; THEN token
tk_NOT		= tk_THEN+1		; NOT token
tk_STEP		= tk_NOT+1		; STEP token
tk_UNTIL	= tk_STEP+1		; UNTIL token
tk_WHILE	= tk_UNTIL+1	; WHILE token
tk_OFF		= tk_WHILE+1	; OFF token

; opperator tokens

tk_PLUS		= tk_OFF+1		; + token
tk_MINUS	= tk_PLUS+1		; - token
tk_MUL		= tk_MINUS+1	; * token
tk_DIV		= tk_MUL+1		; / token
tk_POWER	= tk_DIV+1		; ^ token
tk_AND		= tk_POWER+1	; AND token
tk_EOR		= tk_AND+1		; EOR token
tk_OR		= tk_EOR+1		; OR token
tk_RSHIFT	= tk_OR+1		; RSHIFT token
tk_LSHIFT	= tk_RSHIFT+1	; LSHIFT token
tk_GT		= tk_LSHIFT+1	; > token
tk_EQUAL	= tk_GT+1		; = token
tk_LT		= tk_EQUAL+1	; < token

; functions tokens

tk_SGN		= tk_LT+1		; SGN token
tk_INT		= tk_SGN+1		; INT token
tk_ABS		= tk_INT+1		; ABS token
tk_USR		= tk_ABS+1		; USR token
tk_FRE		= tk_USR+1		; FRE token
tk_POS		= tk_FRE+1		; POS token
tk_SQR		= tk_POS+1		; SQR token
tk_RND		= tk_SQR+1		; RND token
tk_LOG		= tk_RND+1		; LOG token
tk_EXP		= tk_LOG+1		; EXP token
tk_COS		= tk_EXP+1		; COS token
tk_SIN		= tk_COS+1		; SIN token
tk_TAN		= tk_SIN+1		; TAN token
tk_ATN		= tk_TAN+1		; ATN token
tk_PEEK		= tk_ATN+1		; PEEK token
tk_DEEK		= tk_PEEK+1		; DEEK token
tk_SADD		= tk_DEEK+1		; SADD token
tk_LEN		= tk_SADD+1		; LEN token
tk_STRS		= tk_LEN+1		; STR0x token
tk_VAL		= tk_STRS+1		; VAL token
tk_ASC		= tk_VAL+1		; ASC token
tk_UCASES	= tk_ASC+1		; UCASE0x token
tk_LCASES	= tk_UCASES+1	; LCASE0x token
tk_CHRS		= tk_LCASES+1	; CHR0x token
tk_HEXS		= tk_CHRS+1		; HEX0x token
tk_BINS		= tk_HEXS+1		; BIN0x token
tk_BITTST	= tk_BINS+1		; BITTST token
tk_MAX		= tk_BITTST+1	; MAX token
tk_MIN		= tk_MAX+1		; MIN token
tk_PI		= tk_MIN+1		; PI token
tk_TWOPI	= tk_PI+1		; TWOPI token
tk_VPTR		= tk_TWOPI+1	; VARPTR token
tk_LEFTS	= tk_VPTR+1		; LEFT0x token
tk_RIGHTS	= tk_LEFTS+1	; RIGHT0x token
tk_MIDS		= tk_RIGHTS+1	; MID0x token

; offsets from a base of X or Y

plus_0		= 0x00		; X or Y plus 0
plus_1		= 0x01		; X or Y plus 1
plus_2		= 0x02		; X or Y plus 2
plus_3		= 0x03		; X or Y plus 3

Lab_stak	= 0x0100	; stack bottom, no offset

Lab_skfe	= Lab_stak+0xFE
					; flushed stack address
Lab_skff	= Lab_stak+0xFF
					; flushed stack address

Ccflag		= 0x0300	; BASIC CTRL-C flag, 00 = enabled, 01 = dis
Ccbyte		= Ccflag+1	; BASIC CTRL-C byte
Ccnull		= Ccbyte+1	; BASIC CTRL-C byte timeout

Vec_cc		= Ccnull+1	; ctrl c check vector

Vec_in		= Vec_cc+2	; input vector
Vec_out		= Vec_in+2	; output vector
Vec_ld		= Vec_out+2	; load vector
Vec_sv		= Vec_ld+2	; save vector

; Ibuffs can now be anywhere in RAM, ensure that the max length is < 0x80

Ibuffs		= Vec_sv+0x16
					; start of input buffer after IRQ/NMI code
Ibuffe		= Ibuffs+0x47; end of input buffer

Ram_base	= 0x0400	; start of user RAM (set as needed, should be page aligned)
Ram_top		= 0x5000	; end of user RAM+1 (set as needed, should be page aligned)

; This start can be changed to suit your system

ORIGIN = 	0x5000

; For convenience, put jump here to reset location so it can be
; run from the load address.

JMP	Res_vec
        
; BASIC cold start entry point

; new page 2 initialisation, copy block to Ccflag on

==Lab_cold==
	;LDY	#Pg2_tabe-Pg2_tabs-1
					; byte count-1
	LDY		#4
==Lab_2d13==
	LDA	Pg2_tabs,Y		; get byte
	STA	Ccflag,Y		; store in page 2
	DEY				; decrement count
	BPL	Lab_2d13		; loop if not done

	LDX	#0xFF			; set byte
	STX	clineh		; set current line high byte (set immediate mode)
	TXS				; reset stack pointer

	LDA	#0x4C			; code for JMP
	STA	fnxjmp		; save for jump vector for functions

; copy block from Lab_2CEE to 0x00BC - 0x00D3

	LDX	#StrTab.LO-Lab_2CEE.LO	; set byte count
==Lab_2D4E==
	LDA	Lab_2CEE-1,X	; get byte from table
	STA	lab_igby-1,X	; save byte in page zero
	DEX				; decrement count
	BNE	Lab_2D4E		; loop if not all done

; copy block from StrTab to 0x0000 - 0x0012

==Lab_GMEM==
	LDX	#EndTab.LO-StrTab.LO-1	; set byte count-1
==TabLoop==
	LDA	StrTab,X		; get byte from table
	STA	plus_0,X		; save byte in page zero
	DEX				; decrement count
	BPL	TabLoop		; loop if not all done

; set-up start values

	LDA	#0x00			; clear A
	STA	nmibase		; clear NMI handler enabled flag
	STA	irqbase		; clear IRQ handler enabled flag
	STA	fac1_o		; clear FAC1 overflow byte
	STA	last_sh		; clear descriptor stack top item pointer high byte

	LDA	#0x0E			; set default tab size
	STA	tabsiz		; save it
	LDA	#0x03			; set garbage collect step size for descriptor stack
	STA	g_step		; save it
	LDX	#des_sk		; descriptor stack start
	STX	next_s		; set descriptor stack pointer
	JSR	Lab_CRLF		; print CR/LF
	LDA	#Lab_MSZM.LO		; point to memory size message (low addr)
	LDY	#Lab_MSZM.HI		; point to memory size message (high addr)
	JSR	Lab_18C3		; print null terminated string from memory
	JSR	Lab_INLN		; print "? " and get BASIC input
	STX	bpntrl		; set BASIC execute pointer low byte
	STY	bpntrh		; set BASIC execute pointer high byte
	JSR	Lab_gbyt		; get last byte back

	BNE	Lab_2DAA		; branch if not null (user typed something)

	LDY	#0x00			; else clear Y
					; character was null so get memory size the hard way
					; we get here with Y=0 and itempl/h = Ram_base
==Lab_2D93==
	INC	itempl		; increment temporary integer low byte
	BNE	Lab_2D99		; branch if no overflow

	INC	itemph		; increment temporary integer high byte
	LDA	itemph		; get high byte
	CMP	#Ram_top.HI		; compare with top of RAM+1
	BEQ	Lab_2DB6		; branch if match (end of user RAM)

==Lab_2D99==
	LDA	#0x55			; set test byte
	STA	(itempl),Y		; save via temporary integer
	CMP	(itempl),Y		; compare via temporary integer
	BNE	Lab_2DB6		; branch if fail

	ASL A				; shift test byte left (now 0xAA)
	STA	(itempl),Y		; save via temporary integer
	CMP	(itempl),Y		; compare via temporary integer
	BEQ	Lab_2D93		; if ok go do next byte

	BNE	Lab_2DB6		; branch if fail

==Lab_2DAA==
	JSR	Lab_2887		; get FAC1 from string
	LDA	fac1_e		; get FAC1 exponent
	CMP	#0x98			; compare with exponent = 2^24
	BCS	Lab_GMEM		; if too large go try again

	JSR	Lab_F2FU		; save integer part of FAC1 in temporary integer
					; (no range check)

==Lab_2DB6==
	LDA	itempl		; get temporary integer low byte
	LDY	itemph		; get temporary integer high byte
	CPY	#Ram_base.LO+1	; compare with start of RAM+0x100 high byte
	BCC	Lab_GMEM		; if too small go try again


; uncomment these lines if you want to check on the high limit of memory. Note if
; Ram_top is set too low then this will fail. default is ignore it and assume the
; users know what they're doing!

;	CPY	#Ram_top.HI		; compare with top of RAM high byte
;	BCC	MEM_OK		; branch if < RAM top

;	BNE	Lab_GMEM		; if too large go try again
					; else was = so compare low bytes
;	CMP	#Ram_top.LO		; compare with top of RAM low byte
;	BEQ	MEM_OK		; branch if = RAM top

;	BCS	Lab_GMEM		; if too large go try again

;MEM_OK
	STA	ememl			; set end of mem low byte
	STY	ememh			; set end of mem high byte
	STA	sstorl		; set bottom of string space low byte
	STY	sstorh		; set bottom of string space high byte

	LDY	#Ram_base.LO		; set start addr low byte
	LDX	#Ram_base.HI		; set start addr high byte
	STY	smeml			; save start of mem low byte
	STX	smemh			; save start of mem high byte

; this line is only needed if Ram_base is not 0xxx00

;	LDY	#0x00			; clear Y
	TYA				; clear A
	STA	(smeml),Y		; clear first byte
	INC	smeml			; increment start of mem low byte

; these two lines are only needed if Ram_base is 0xxxFF

;	BNE	Lab_2E05		; branch if no rollover

;	INC	smemh			; increment start of mem high byte
==Lab_2E05==
	JSR	Lab_CRLF		; print CR/LF
	JSR	Lab_1463		; do "NEW" and "CLEAR"
	LDA	ememl			; get end of mem low byte
	SEC				; set carry for subtract
	SBC	smeml			; subtract start of mem low byte
	TAX				; copy to X
	LDA	ememh			; get end of mem high byte
	SBC	smemh			; subtract start of mem high byte
	JSR	Lab_295E		; print XA as unsigned integer (bytes free)
	LDA	#Lab_SMSG.LO		; point to sign-on message (low addr)
	LDY	#Lab_SMSG.HI		; point to sign-on message (high addr)
	JSR	Lab_18C3		; print null terminated string from memory
	LDA	#Lab_1274.LO		; warm start vector low byte
	LDY	#Lab_1274.HI		; warm start vector high byte
	STA	wrmjpl		; save warm start vector low byte
	STY	wrmjph		; save warm start vector high byte
	JMP	0x0001		; go do warm start

; open up space in memory
; move (ostrtl)-(obendl) to new block ending at (nbendl)

; nbendl,nbendh - new block end address (A/Y)
; obendl,obendh - old block end address
; ostrtl,ostrth - old block start address

; returns with ..

; nbendl,nbendh - new block start address (high byte - 0x100)
; obendl,obendh - old block start address (high byte - 0x100)
; ostrtl,ostrth - old block start address (unchanged)

==Lab_11CF==
	JSR	Lab_121F		; check available memory, "Out of memory" error if no room
					; addr to check is in AY (low/high)
	STA	earryl		; save new array mem end low byte
	STY	earryh		; save new array mem end high byte

; open up space in memory
; move (ostrtl)-(obendl) to new block ending at (nbendl)
; don't set array end

==Lab_11D6==
	SEC				; set carry for subtract
	LDA	obendl		; get block end low byte
	SBC	ostrtl		; subtract block start low byte
	TAY				; copy MOD(block length/0x100) byte to Y
	LDA	obendh		; get block end high byte
	SBC	ostrth		; subtract block start high byte
	TAX				; copy block length high byte to X
	INX				; +1 to allow for count=0 exit
	TYA				; copy block length low byte to A
	BEQ	Lab_120A		; branch if length low byte=0

					; block is (X-1)*256+Y bytes, do the Y bytes first

	SEC				; set carry for add + 1, two's complement
	EOR	#0xFF			; invert low byte for subtract
	ADC	obendl		; add block end low byte

	STA	obendl		; save corrected old block end low byte
	BCS	Lab_11F3		; branch if no underflow

	DEC	obendh		; else decrement block end high byte
	SEC				; set carry for add + 1, two's complement
==Lab_11F3==
	TYA				; get MOD(block length/0x100) byte
	EOR	#0xFF			; invert low byte for subtract
	ADC	nbendl		; add destination end low byte
	STA	nbendl		; save modified new block end low byte
	BCS	Lab_1203		; branch if no underflow

	DEC	nbendh		; else decrement block end high byte
	BCC	Lab_1203		; branch always

==Lab_11FF==
	LDA	(obendl),Y		; get byte from source
	STA	(nbendl),Y		; copy byte to destination
==Lab_1203==
	DEY				; decrement index
	BNE	Lab_11FF		; loop until Y=0

					; now do Y=0 indexed byte
	LDA	(obendl),Y		; get byte from source
	STA	(nbendl),Y		; save byte to destination
==Lab_120A==
	DEC	obendh		; decrement source pointer high byte
	DEC	nbendh		; decrement destination pointer high byte
	DEX				; decrement block count
	BNE	Lab_1203		; loop until count = 0x0

	RTS

; check room on stack for A bytes
; stack too deep? do OM error

==Lab_1212==
	STA	tempb			; save result in temp byte
	TSX				; copy stack
	CPX	tempb			; compare new "limit" with stack
	BCC	Lab_OMER		; if stack < limit do "Out of memory" error then warm start

	RTS

; check available memory, "Out of memory" error if no room
; addr to check is in AY (low/high)

==Lab_121F==
	CPY	sstorh		; compare bottom of string mem high byte
	BCC	Lab_124B		; if less then exit (is ok)

	BNE	Lab_1229		; skip next test if greater (tested <)

					; high byte was =, now do low byte
	CMP	sstorl		; compare with bottom of string mem low byte
	BCC	Lab_124B		; if less then exit (is ok)

					; addr is > string storage ptr (oops!)
==Lab_1229==
	PHA				; push addr low byte
	LDX	#0x08			; set index to save adatal to expneg inclusive
	TYA				; copy addr high byte (to push on stack)

					; save misc numeric work area
==Lab_122D==
	PHA				; push byte
	LDA	adatal-1,X		; get byte from adatal to expneg ( ,0x00 not pushed)
	DEX				; decrement index
	BPL	Lab_122D		; loop until all done

	JSR	Lab_GARB		; garbage collection routine

					; restore misc numeric work area
	LDX	#0x00			; clear the index to restore bytes
==Lab_1238==
	PLA				; pop byte
	STA	adatal,X		; save byte to adatal to expneg
	INX				; increment index
	CPX	#0x08			; compare with end + 1
	BMI	Lab_1238		; loop if more to do

	PLA				; pop addr high byte
	TAY				; copy back to Y
	PLA				; pop addr low byte
	CPY	sstorh		; compare bottom of string mem high byte
	BCC	Lab_124B		; if less then exit (is ok)

	BNE	Lab_OMER		; if greater do "Out of memory" error then warm start

					; high byte was =, now do low byte
	CMP	sstorl		; compare with bottom of string mem low byte
	BCS	Lab_OMER		; if >= do "Out of memory" error then warm start

					; ok exit, carry clear
==Lab_124B==
	RTS

; do "Out of memory" error then warm start

==Lab_OMER==
	LDX	#0x0C			; error code 0x0C ("Out of memory" error)

; do error #X, then warm start

==Lab_XERR==
	JSR	Lab_CRLF		; print CR/LF

	LDA	Lab_BAER,X		; get error message pointer low byte
	LDY	Lab_BAER+1,X	; get error message pointer high byte
	JSR	Lab_18C3		; print null terminated string from memory

	JSR	Lab_1491		; flush stack and clear continue flag
	LDA	#Lab_EMSG.LO		; point to " Error" low addr
	LDY	#Lab_EMSG.HI		; point to " Error" high addr
==Lab_1269==
	JSR	Lab_18C3		; print null terminated string from memory
	LDY	clineh		; get current line high byte
	INY				; increment it
	BEQ	Lab_1274		; go do warm start (was immediate mode)

					; else print line number
	JSR	Lab_2953		; print " in line [LINE #]"

; BASIC warm start entry point
; wait for Basic command

==Lab_1274==
					; clear ON IRQ/NMI bytes
	LDA	#0x00			; clear A
	STA	irqbase		; clear enabled byte
	STA	nmibase		; clear enabled byte
	LDA	#Lab_RMSG.LO		; point to "Ready" message low byte
	LDY	#Lab_RMSG.HI		; point to "Ready" message high byte

	JSR	Lab_18C3		; go do print string

; wait for Basic command (no "Ready")

==Lab_127D==
	JSR	Lab_1357		; call for BASIC input
==Lab_1280==
	STX	bpntrl		; set BASIC execute pointer low byte
	STY	bpntrh		; set BASIC execute pointer high byte
	JSR	Lab_gbyt		; scan memory
	BEQ	Lab_127D		; loop while null

; got to interpret input line now ..

	LDX	#0xFF			; current line to null value
	STX	clineh		; set current line high byte
	BCC	Lab_1295		; branch if numeric character (handle new BASIC line)

					; no line number .. immediate mode
	JSR	Lab_13A6		; crunch keywords into Basic tokens
	JMP	Lab_15F6		; go scan and interpret code

; handle new BASIC line

==Lab_1295==
	JSR	Lab_GFPN		; get fixed-point number into temp integer
	JSR	Lab_13A6		; crunch keywords into Basic tokens
	STY	ibptr			; save index pointer to end of crunched line
	JSR	Lab_SSLN		; search BASIC for temp integer line number
	BCC	Lab_12E6		; branch if not found

					; aroooogah! line # already exists! delete it
	LDY	#0x01			; set index to next line pointer high byte
	LDA	(baslnl),Y		; get next line pointer high byte
	STA	ut1_ph		; save it
	LDA	svarl			; get start of vars low byte
	STA	ut1_pl		; save it
	LDA	baslnh		; get found line pointer high byte
	STA	ut2_ph		; save it
	LDA	baslnl		; get found line pointer low byte
	DEY				; decrement index
	SBC	(baslnl),Y		; subtract next line pointer low byte
	CLC				; clear carry for add
	ADC	svarl			; add start of vars low byte
	STA	svarl			; save new start of vars low byte
	STA	ut2_pl		; save destination pointer low byte
	LDA	svarh			; get start of vars high byte
	ADC	#0xFF			; -1 + carry
	STA	svarh			; save start of vars high byte
	SBC	baslnh		; subtract found line pointer high byte
	TAX				; copy to block count
	SEC				; set carry for subtract
	LDA	baslnl		; get found line pointer low byte
	SBC	svarl			; subtract start of vars low byte
	TAY				; copy to bytes in first block count
	BCS	Lab_12D0		; branch if overflow

	INX				; increment block count (correct for =0 loop exit)
	DEC	ut2_ph		; decrement destination high byte
==Lab_12D0==
	CLC				; clear carry for add
	ADC	ut1_pl		; add source pointer low byte
	BCC	Lab_12D8		; branch if no overflow

	DEC	ut1_ph		; else decrement source pointer high byte
	CLC				; clear carry

					; close up memory to delete old line
==Lab_12D8==
	LDA	(ut1_pl),Y		; get byte from source
	STA	(ut2_pl),Y		; copy to destination
	INY				; increment index
	BNE	Lab_12D8		; while <> 0 do this block

	INC	ut1_ph		; increment source pointer high byte
	INC	ut2_ph		; increment destination pointer high byte
	DEX				; decrement block count
	BNE	Lab_12D8		; loop until all done

					; got new line in buffer and no existing same #
==Lab_12E6==
	LDA	Ibuffs		; get byte from start of input buffer
	BEQ	Lab_1319		; if null line just go flush stack/vars and exit

					; got new line and it isn't empty line
	LDA	ememl			; get end of mem low byte
	LDY	ememh			; get end of mem high byte
	STA	sstorl		; set bottom of string space low byte
	STY	sstorh		; set bottom of string space high byte
	LDA	svarl			; get start of vars low byte	(end of BASIC)
	STA	obendl		; save old block end low byte
	LDY	svarh			; get start of vars high byte	(end of BASIC)
	STY	obendh		; save old block end high byte
	ADC	ibptr			; add input buffer pointer	(also buffer length)
	BCC	Lab_1301		; branch if no overflow from add

	INY				; else increment high byte
==Lab_1301==
	STA	nbendl		; save new block end low byte	(move to, low byte)
	STY	nbendh		; save new block end high byte
	JSR	Lab_11CF		; open up space in memory
					; old start pointer ostrtl,ostrth set by the find line call
	LDA	earryl		; get array mem end low byte
	LDY	earryh		; get array mem end high byte
	STA	svarl			; save start of vars low byte
	STY	svarh			; save start of vars high byte
	LDY	ibptr			; get input buffer pointer	(also buffer length)
	DEY				; adjust for loop type
==Lab_1311==
	LDA	Ibuffs-4,Y		; get byte from crunched line
	STA	(baslnl),Y		; save it to program memory
	DEY				; decrement count
	CPY	#0x03			; compare with first byte-1
	BNE	Lab_1311		; continue while count <> 3

	LDA	itemph		; get line # high byte
	STA	(baslnl),Y		; save it to program memory
	DEY				; decrement count
	LDA	itempl		; get line # low byte
	STA	(baslnl),Y		; save it to program memory
	DEY				; decrement count
	LDA	#0xFF			; set byte to allow chain rebuild. if you didn't set this
					; byte then a zero already here would stop the chain rebuild
					; as it would think it was the [EOT] marker.
	STA	(baslnl),Y		; save it to program memory

==Lab_1319==
	JSR	Lab_1477		; reset execution to start, clear vars and flush stack
	LDX	smeml			; get start of mem low byte
	LDA	smemh			; get start of mem high byte
	LDY	#0x01			; index to high byte of next line pointer
==Lab_1325==
	STX	ut1_pl		; set line start pointer low byte
	STA	ut1_ph		; set line start pointer high byte
	LDA	(ut1_pl),Y		; get it
	BEQ	Lab_133E		; exit if end of program

; rebuild chaining of Basic lines

	LDY	#0x04			; point to first code byte of line
					; there is always 1 byte + [EOL] as null entries are deleted
==Lab_1330==
	INY				; next code byte
	LDA	(ut1_pl),Y		; get byte
	BNE	Lab_1330		; loop if not [EOL]

	SEC				; set carry for add + 1
	TYA				; copy end index
	ADC	ut1_pl		; add to line start pointer low byte
	TAX				; copy to X
	LDY	#0x00			; clear index, point to this line's next line pointer
	STA	(ut1_pl),Y		; set next line pointer low byte
	TYA				; clear A
	ADC	ut1_ph		; add line start pointer high byte + carry
	INY				; increment index to high byte
	STA	(ut1_pl),Y		; save next line pointer low byte
	BCC	Lab_1325		; go do next line, branch always, carry clear


==Lab_133E==
	JMP	Lab_127D		; else we just wait for Basic command, no "Ready"

; print "? " and get BASIC input

==Lab_INLN==
	JSR	Lab_18E3		; print "?" character
	JSR	Lab_18E0		; print " "
	BNE	Lab_1357		; call for BASIC input and return

; receive line from keyboard

					; 0x08 as delete key (BACKSPACE on standard keyboard)
==Lab_134B==
	JSR	Lab_PRNA		; go print the character
	DEX				; decrement the buffer counter (delete)
	DATA	0x2C			; make LDX into BIT abs

; call for BASIC input (main entry point)

==Lab_1357==
	LDX	#0x00			; clear BASIC line buffer pointer
==Lab_1359==
	JSR	V_inpt		; call scan input device
	BCC	Lab_1359		; loop if no byte

	BEQ	Lab_1359		; loop until valid input (ignore NULLs)

	CMP	#0x07			; compare with [BELL]
	BEQ	Lab_1378		; branch if [BELL]

	CMP	#0x0D			; compare with [CR]
	BEQ	Lab_1384		; do CR/LF exit if [CR]

	CPX	#0x00			; compare pointer with 0x00
	BNE	Lab_1374		; branch if not empty

; next two lines ignore any non print character and [SPACE] if input buffer empty

	CMP	#0x21			; compare with [SP]+1
	BCC	Lab_1359		; if < ignore character

==Lab_1374==
	CMP	#0x08			; compare with [BACKSPACE] (delete last character)
	BEQ	Lab_134B		; go delete last character

==Lab_1378==
	CPX	#Ibuffe.LO-Ibuffs.LO	; compare character count with max
	BCS	Lab_138E		; skip store and do [BELL] if buffer full

	STA	Ibuffs,X		; else store in buffer
	INX				; increment pointer
==Lab_137F==
	JSR	Lab_PRNA		; go print the character
	BNE	Lab_1359		; always loop for next character

==Lab_1384==
	JMP	Lab_1866		; do CR/LF exit to BASIC

; announce buffer full

==Lab_138E==
	LDA	#0x07			; [BELL] character into A
	BNE	Lab_137F		; go print the [BELL] but ignore input character
					; branch always

; crunch keywords into Basic tokens
; position independent buffer version ..
; faster, dictionary search version ....

==Lab_13A6==
	LDY	#0xFF			; set save index (makes for easy math later)

	SEC				; set carry for subtract
	LDA	bpntrl		; get basic execute pointer low byte
	SBC	#Ibuffs.LO		; subtract input buffer start pointer
	TAX				; copy result to X (index past line # if any)

	STX	oquote		; clear open quote/DATA flag
==Lab_13AC==
	LDA	Ibuffs,X		; get byte from input buffer
	BEQ	Lab_13EC		; if null save byte then exit

	CMP	#0x5F			; compare with "_"
	BCS	Lab_13EC		; if >= go save byte then continue crunching

	CMP	#0x3c			; compare with "<"
	BCS	Lab_13CC		; if >= go crunch now

	CMP	#0x30			; compare with "0"
	BCS	Lab_13EC		; if >= go save byte then continue crunching

	STA	scnquo		; save buffer byte as search character
	CMP	#0x22			; is it quote character?
	BEQ	Lab_1410		; branch if so (copy quoted string)

	CMP	#0x2A			; compare with "*"
	BCC	Lab_13EC		; if < go save byte then continue crunching

					; else crunch now
==Lab_13CC==
	BIT	oquote		; get open quote/DATA token flag
	BVS	Lab_13EC		; branch if b6 of oquote set (was DATA)
					; go save byte then continue crunching

	STX	tempb			; save buffer read index
	STY	csidx			; copy buffer save index
	LDY	#Tab_1STC.LO		; get keyword first character table low address
	STY	ut2_pl		; save pointer low byte
	LDY	#Tab_1STC.HI		; get keyword first character table high address
	STY	ut2_ph		; save pointer high byte
	LDY	#0x00			; clear table pointer

==Lab_13D0==
	CMP	(ut2_pl),Y		; compare with keyword first character table byte
	BEQ	Lab_13D1		; go do word_table_chr if match

	BCC	Lab_13EA		; if < keyword first character table byte go restore
					; Y and save to crunched

	INY				; else increment pointer
	BNE	Lab_13D0		; and loop (branch always)

; have matched first character of some keyword

==Lab_13D1==
	TYA				; copy matching index
	ASL A				; *2 (bytes per pointer)
	TAX				; copy to new index
	LDA	Tab_CHRT,X		; get keyword table pointer low byte
	STA	ut2_pl		; save pointer low byte
	LDA	Tab_CHRT+1,X	; get keyword table pointer high byte
	STA	ut2_ph		; save pointer high byte

	LDY	#0xFF			; clear table pointer (make -1 for start)

	LDX	tempb			; restore buffer read index

==Lab_13D6==
	INY				; next table byte
	LDA	(ut2_pl),Y		; get byte from table
==Lab_13D8==
	BMI	Lab_13EA		; all bytes matched so go save token

	INX				; next buffer byte
	CMP	Ibuffs,X		; compare with byte from input buffer
	BEQ	Lab_13D6		; go compare next if match

	BNE	Lab_1417		; branch if >< (not found keyword)

==Lab_13EA==
	LDY	csidx			; restore save index

					; save crunched to output
==Lab_13EC==
	INX				; increment buffer index (to next input byte)
	INY				; increment save index (to next output byte)
	STA	Ibuffs,Y		; save byte to output
	CMP	#0x00			; set the flags, set carry
	BEQ	Lab_142A		; do exit if was null [EOL]

					; A holds token or byte here
	SBC	#0x3A			; subtract ":" (carry set by CMP #00)
	BEQ	Lab_13FF		; branch if it was ":" (is now 0x00)

					; A now holds token-0x3A
	CMP	#tk_DATA-0x3A	; compare with DATA token - 0x3A
	BNE	Lab_1401		; branch if not DATA

					; token was : or DATA
==Lab_13FF==
	STA	oquote		; save token-0x3A (clear for ":", tk_DATA-0x3A for DATA)
==Lab_1401==
	EOR	#tk_REM-0x3A		; effectively subtract REM token offset
	BNE	Lab_13AC		; If wasn't REM then go crunch rest of line

	STA	asrch			; else was REM so set search for [EOL]

					; loop for REM, "..." etc.
==Lab_1408==
	LDA	Ibuffs,X		; get byte from input buffer
	BEQ	Lab_13EC		; branch if null [EOL]

	CMP	asrch			; compare with stored character
	BEQ	Lab_13EC		; branch if match (end quote)

					; entry for copy string in quotes, don't crunch
==Lab_1410==
	INY				; increment buffer save index
	STA	Ibuffs,Y		; save byte to output
	INX				; increment buffer read index
	BNE	Lab_1408		; loop while <> 0 (should never be 0!)

					; not found keyword this go
==Lab_1417==
	LDX	tempb			; compare has failed, restore buffer index (start byte!)

					; now find the end of this word in the table
==Lab_141B==
	LDA	(ut2_pl),Y		; get table byte
	PHP				; save status
	INY				; increment table index
	PLP				; restore byte status
	BPL	Lab_141B		; if not end of keyword go do next

	LDA	(ut2_pl),Y		; get byte from keyword table
	BNE	Lab_13D8		; go test next word if not zero byte (end of table)

					; reached end of table with no match
	LDA	Ibuffs,X		; restore byte from input buffer
	BPL	Lab_13EA		; branch always (all bytes in buffer are 0x00-0x7F)
					; go save byte in output and continue crunching

					; reached [EOL]
==Lab_142A==
	INY				; increment pointer
	INY				; increment pointer (makes it next line pointer high byte)
	STA	Ibuffs,Y		; save [EOL] (marks [EOT] in immediate mode)
	INY				; adjust for line copy
	INY				; adjust for line copy
	INY				; adjust for line copy
	DEC	bpntrl		; allow for increment (change if buffer starts at 0xxxFF)
	RTS

; search Basic for temp integer line number from start of mem

==Lab_SSLN==
	LDA	smeml			; get start of mem low byte
	LDX	smemh			; get start of mem high byte

; search Basic for temp integer line number from AX
; returns carry set if found
; returns baslnl/baslnh pointer to found or next higher (not found) line

; old 541 new 507

==Lab_SHLN==
	LDY	#0x01			; set index
	STA	baslnl		; save low byte as current
	STX	baslnh		; save high byte as current
	LDA	(baslnl),Y		; get pointer high byte from addr
	BEQ	Lab_145F		; pointer was zero so we're done, do 'not found' exit

	LDY	#0x03			; set index to line # high byte
	LDA	(baslnl),Y		; get line # high byte
	DEY				; decrement index (point to low byte)
	CMP	itemph		; compare with temporary integer high byte
	BNE	Lab_1455		; if <> skip low byte check

	LDA	(baslnl),Y		; get line # low byte
	CMP	itempl		; compare with temporary integer low byte
==Lab_1455==
	BCS	Lab_145E		; else if temp < this line, exit (passed line#)

==Lab_1456==
	DEY				; decrement index to next line ptr high byte
	LDA	(baslnl),Y		; get next line pointer high byte
	TAX				; copy to X
	DEY				; decrement index to next line ptr low byte
	LDA	(baslnl),Y		; get next line pointer low byte
	BCC	Lab_SHLN		; go search for line # in temp (itempl/itemph) from AX
					; (carry always clear)

==Lab_145E==
	BEQ	Lab_1460		; exit if temp = found line #, carry is set

==Lab_145F==
	CLC				; clear found flag
==Lab_1460==
	RTS

; perform NEW

==Lab_NEW==
	BNE	Lab_1460		; exit if not end of statement (to do syntax error)

==Lab_1463==
	LDA	#0x00			; clear A
	TAY				; clear Y
	STA	(smeml),Y		; clear first line, next line pointer, low byte
	INY				; increment index
	STA	(smeml),Y		; clear first line, next line pointer, high byte
	CLC				; clear carry
	LDA	smeml			; get start of mem low byte
	ADC	#0x02			; calculate end of BASIC low byte
	STA	svarl			; save start of vars low byte
	LDA	smemh			; get start of mem high byte
	ADC	#0x00			; add any carry
	STA	svarh			; save start of vars high byte

; reset execution to start, clear vars and flush stack

==Lab_1477==
	CLC				; clear carry
	LDA	smeml			; get start of mem low byte
	ADC	#0xFF			; -1
	STA	bpntrl		; save BASIC execute pointer low byte
	LDA	smemh			; get start of mem high byte
	ADC	#0xFF			; -1+carry
	STA	bpntrh		; save BASIC execute pointer high byte

; "CLEAR" command gets here

==Lab_147A==
	LDA	ememl			; get end of mem low byte
	LDY	ememh			; get end of mem high byte
	STA	sstorl		; set bottom of string space low byte
	STY	sstorh		; set bottom of string space high byte
	LDA	svarl			; get start of vars low byte
	LDY	svarh			; get start of vars high byte
	STA	sarryl		; save var mem end low byte
	STY	sarryh		; save var mem end high byte
	STA	earryl		; save array mem end low byte
	STY	earryh		; save array mem end high byte
	JSR	Lab_161A		; perform RESTORE command

; flush stack and clear continue flag

==Lab_1491==
	LDX	#des_sk		; set descriptor stack pointer
	STX	next_s		; save descriptor stack pointer
	PLA				; pull return address low byte
	TAX				; copy return address low byte
	PLA				; pull return address high byte
	STX	Lab_skfe		; save to cleared stack
	STA	Lab_skff		; save to cleared stack
	LDX	#0xFD			; new stack pointer
	TXS				; reset stack
	LDA	#0x00			; clear byte
	STA	cpntrh		; clear continue pointer high byte
	STA	sufnxf		; clear subscript/FNX flag
==Lab_14A6==
	RTS

; perform CLEAR

==Lab_CLEAR==
	BEQ	Lab_147A		; if no following token go do "CLEAR"

					; else there was a following token (go do syntax error)
	RTS

; perform LIST [n][-m]
; bigger, faster version (a _lot_ faster)

==Lab_LIST==
	BCC	Lab_14BD		; branch if next character numeric (LIST n..)

	BEQ	Lab_14BD		; branch if next character [NULL] (LIST)

	CMP	#tk_MINUS		; compare with token for -
	BNE	Lab_14A6		; exit if not - (LIST -m)

					; LIST [[n][-m]]
					; this bit sets the n , if present, as the start and end
==Lab_14BD==
	JSR	Lab_GFPN		; get fixed-point number into temp integer
	JSR	Lab_SSLN		; search BASIC for temp integer line number
					; (pointer in baslnl/baslnh)
	JSR	Lab_gbyt		; scan memory
	BEQ	Lab_14D4		; branch if no more characters

					; this bit checks the - is present
	CMP	#tk_MINUS		; compare with token for -
	BNE	Lab_1460		; return if not "-" (will be Syntax error)

					; LIST [n]-m
					; the - was there so set m as the end value
	JSR	0x00BC		; increment and scan memory
	JSR	Lab_GFPN		; get fixed-point number into temp integer
	BNE	Lab_1460		; exit if not ok

==Lab_14D4==
	LDA	itempl		; get temporary integer low byte
	ORA	itemph		; OR temporary integer high byte
	BNE	Lab_14E2		; branch if start set

	LDA	#0xFF			; set for -1
	STA	itempl		; set temporary integer low byte
	STA	itemph		; set temporary integer high byte
==Lab_14E2==
	LDY	#0x01			; set index for line
	STY	oquote		; clear open quote flag
	JSR	Lab_CRLF		; print CR/LF
	LDA	(baslnl),Y		; get next line pointer high byte
					; pointer initially set by search at Lab_14BD
	BEQ	Lab_152B		; if null all done so exit
	JSR	Lab_1629		; do CRTL-C check vector

	INY				; increment index for line
	LDA	(baslnl),Y		; get line # low byte
	TAX				; copy to X
	INY				; increment index
	LDA	(baslnl),Y		; get line # high byte
	CMP	itemph		; compare with temporary integer high byte
	BNE	Lab_14FF		; branch if no high byte match

	CPX	itempl		; compare with temporary integer low byte
	BEQ	Lab_1501		; branch if = last line to do (< will pass next branch)

==Lab_14FF==				; else ..
	BCS	Lab_152B		; if greater all done so exit

==Lab_1501==
	STY	tidx1			; save index for line
	JSR	Lab_295E		; print XA as unsigned integer
	LDA	#0x20			; space is the next character
==Lab_1508==
	LDY	tidx1			; get index for line
	AND	#0x7F			; mask top out bit of character
==Lab_150C==
	JSR	Lab_PRNA		; go print the character
	CMP	#0x22			; was it " character
	BNE	Lab_1519		; branch if not

					; we are either entering or leaving a pair of quotes
	LDA	oquote		; get open quote flag
	EOR	#0xFF			; toggle it
	STA	oquote		; save it back
==Lab_1519==
	INY				; increment index
	LDA	(baslnl),Y		; get next byte
	BNE	Lab_152E		; branch if not [EOL] (go print character)
	TAY				; else clear index
	LDA	(baslnl),Y		; get next line pointer low byte
	TAX				; copy to X
	INY				; increment index
	LDA	(baslnl),Y		; get next line pointer high byte
	STX	baslnl		; set pointer to line low byte
	STA	baslnh		; set pointer to line high byte
	BNE	Lab_14E2		; go do next line if not [EOT]
					; else ..
==Lab_152B==
	RTS

==Lab_152E==
	BPL	Lab_150C		; just go print it if not token byte

					; else was token byte so uncrunch it (maybe)
	BIT	oquote		; test the open quote flag
	BMI	Lab_150C		; just go print character if open quote set

	LDX	#Lab_KEYT.HI		; get table address high byte
	ASL A				; *2
	ASL A				; *4
	BCC	Lab_152F		; branch if no carry

	INX				; else increment high byte
	CLC				; clear carry for add
==Lab_152F==
	ADC	#Lab_KEYT.LO		; add low byte
	BCC	Lab_1530		; branch if no carry

	INX				; else increment high byte
==Lab_1530==
	STA	ut2_pl		; save table pointer low byte
	STX	ut2_ph		; save table pointer high byte
	STY	tidx1			; save index for line
	LDY	#0x00			; clear index
	LDA	(ut2_pl),Y		; get length
	TAX				; copy length
	INY				; increment index
	LDA	(ut2_pl),Y		; get 1st character
	DEX				; decrement length
	BEQ	Lab_1508		; if no more characters exit and print

	JSR	Lab_PRNA		; go print the character
	INY				; increment index
	LDA	(ut2_pl),Y		; get keyword address low byte
	PHA				; save it for now
	INY				; increment index
	LDA	(ut2_pl),Y		; get keyword address high byte
	LDY	#0x00
	STA	ut2_ph		; save keyword pointer high byte
	PLA				; pull low byte
	STA	ut2_pl		; save keyword pointer low byte
==Lab_1540==
	LDA	(ut2_pl),Y		; get character
	DEX				; decrement character count
	BEQ	Lab_1508		; if last character exit and print

	JSR	Lab_PRNA		; go print the character
	INY				; increment index
	BNE	Lab_1540		; loop for next character

; perform FOR

==Lab_FOR==
	LDA	#0x80			; set FNX
	STA	sufnxf		; set subscript/FNX flag
	JSR	Lab_LET		; go do LET
	PLA				; pull return address
	PLA				; pull return address
	LDA	#0x10			; we need 16d bytes !
	JSR	Lab_1212		; check room on stack for A bytes
	JSR	Lab_SNBS		; scan for next BASIC statement ([:] or [EOL])
	CLC				; clear carry for add
	TYA				; copy index to A
	ADC	bpntrl		; add BASIC execute pointer low byte
	PHA				; push onto stack
	LDA	bpntrh		; get BASIC execute pointer high byte
	ADC	#0x00			; add carry
	PHA				; push onto stack
	LDA	clineh		; get current line high byte
	PHA				; push onto stack
	LDA	clinel		; get current line low byte
	PHA				; push onto stack
	LDA	#tk_TO		; get "TO" token
	JSR	Lab_SCCA		; scan for CHR0x(A) , else do syntax error then warm start
	JSR	Lab_CTNM		; check if source is numeric, else do type mismatch
	JSR	Lab_EVNM		; evaluate expression and check is numeric,
					; else do type mismatch
	LDA	fac1_s		; get FAC1 sign (b7)
	ORA	#0x7F			; set all non sign bits
	AND	fac1_1		; and FAC1 mantissa1
	STA	fac1_1		; save FAC1 mantissa1
	LDA	#Lab_159F.LO		; set return address low byte
	LDY	#Lab_159F.HI		; set return address high byte
	STA	ut1_pl		; save return address low byte
	STY	ut1_ph		; save return address high byte
	JMP	Lab_1B66		; round FAC1 and put on stack (returns to next instruction)

==Lab_159F==
	LDA	#Lab_259C.LO		; set 1 pointer low addr (default step size)
	LDY	#Lab_259C.HI		; set 1 pointer high addr
	JSR	Lab_UFAC		; unpack memory (AY) into FAC1
	JSR	Lab_gbyt		; scan memory
	CMP	#tk_STEP		; compare with STEP token
	BNE	Lab_15B3		; jump if not "STEP"

					;.was step so ..
	JSR	0x00BC		; increment and scan memory
	JSR	Lab_EVNM		; evaluate expression and check is numeric,
					; else do type mismatch
==Lab_15B3==
	JSR	Lab_27CA		; return A=FF,C=1/-ve A=01,C=0/+ve
	STA	fac1_s		; set FAC1 sign (b7)
					; this is +1 for +ve step and -1 for -ve step, in NEXT we
					; compare the FOR value and the TO value and return +1 if
					; FOR > TO, 0 if FOR = TO and -1 if FOR < TO. the value
					; here (+/-1) is then compared to that result and if they
					; are the same (+ve and FOR > TO or -ve and FOR < TO) then
					; the loop is done
	JSR	Lab_1B5B		; push sign, round FAC1 and put on stack
	LDA	frnxth		; get var pointer for FOR/NEXT high byte
	PHA				; push on stack
	LDA	frnxtl		; get var pointer for FOR/NEXT low byte
	PHA				; push on stack
	LDA	#tk_FOR		; get FOR token
	PHA				; push on stack

; interpreter inner loop

==Lab_15C2==
	JSR	Lab_1629		; do CRTL-C check vector
	LDA	bpntrl		; get BASIC execute pointer low byte
	LDY	bpntrh		; get BASIC execute pointer high byte

	LDX	clineh		; continue line is 0xFFxx for immediate mode
					; (0x00xx for RUN from immediate mode)
	INX				; increment it (now 0x00 if immediate mode)
	BEQ	Lab_15D1		; branch if null (immediate mode)

	STA	cpntrl		; save continue pointer low byte
	STY	cpntrh		; save continue pointer high byte
==Lab_15D1==
	LDY	#0x00			; clear index
	LDA	(bpntrl),Y		; get next byte
	BEQ	Lab_15DC		; branch if null [EOL]

	CMP	#0x3A			; compare with ":"
	BEQ	Lab_15F6		; branch if = (statement separator)

==Lab_15D9==
	JMP	Lab_SNER		; else syntax error then warm start

					; have reached [EOL]
==Lab_15DC==
	LDY	#0x02			; set index
	LDA	(bpntrl),Y		; get next line pointer high byte
	CLC				; clear carry for no "BREAK" message
	BEQ	Lab_1651		; if null go to immediate mode (was immediate or [EOT]
					; marker)

	INY				; increment index
	LDA	(bpntrl),Y		; get line # low byte
	STA	clinel		; save current line low byte
	INY				; increment index
	LDA	(bpntrl),Y		; get line # high byte
	STA	clineh		; save current line high byte
	TYA				; A now = 4
	ADC	bpntrl		; add BASIC execute pointer low byte
	STA	bpntrl		; save BASIC execute pointer low byte
	BCC	Lab_15F6		; branch if no overflow

	INC	bpntrh		; else increment BASIC execute pointer high byte
==Lab_15F6==
	JSR	0x00BC		; increment and scan memory

==Lab_15F9==
	JSR	Lab_15FF		; go interpret BASIC code from (bpntrl)

==Lab_15FC==
	JMP	Lab_15C2		; loop

; interpret BASIC code from (bpntrl)

==Lab_15FF==
	BEQ	Lab_1628		; exit if zero [EOL]

==Lab_1602==
	ASL A				; *2 bytes per vector and normalise token
	BCS	Lab_1609		; branch if was token

	JMP	Lab_LET		; else go do implied LET

==Lab_1609==
	CMP	#(tk_TAB-0x80)*2	; compare normalised token * 2 with TAB
	BCS	Lab_15D9		; branch if A>=TAB (do syntax error then warm start)
					; only tokens before TAB can start a line
	TAY				; copy to index
	LDA	Lab_CTBL+1,Y	; get vector high byte
	PHA				; onto stack
	LDA	Lab_CTBL,Y		; get vector low byte
	PHA				; onto stack
	JMP	0x00BC		; jump to increment and scan memory
					; then "return" to vector

; CTRL-C check jump. this is called as a subroutine but exits back via a jump if a
; key press is detected.

==Lab_1629==
	JMP	(Vec_cc)		; ctrl c check vector

; if there was a key press it gets back here ..

==Lab_1636==
	CMP	#0x03			; compare with CTRL-C

; perform STOP

==Lab_STOP==
	BCS	Lab_163B		; branch if token follows STOP
					; else just END
; END

==Lab_END==
	CLC				; clear the carry, indicate a normal program end
==Lab_163B==
	BNE	Lab_167A		; if wasn't CTRL-C or there is a following byte return

	LDA	bpntrh		; get the BASIC execute pointer high byte
	EOR	#Ibuffs.HI		; compare with buffer address high byte (Cb unchanged)
	BEQ	Lab_164F		; branch if the BASIC pointer is in the input buffer
					; (can't continue in immediate mode)

					; else ..
	EOR	#Ibuffs.HI		; correct the bits
	LDY	bpntrl		; get BASIC execute pointer low byte
	STY	cpntrl		; save continue pointer low byte
	STA	cpntrh		; save continue pointer high byte
==Lab_1647==
	LDA	clinel		; get current line low byte
	LDY	clineh		; get current line high byte
	STA	blinel		; save break line low byte
	STY	blineh		; save break line high byte
==Lab_164F==
	PLA				; pull return address low
	PLA				; pull return address high
==Lab_1651==
	BCC	Lab_165E		; if was program end just do warm start

					; else ..
	LDA	#Lab_BMSG.LO		; point to "Break" low byte
	LDY	#Lab_BMSG.HI		; point to "Break" high byte
	JMP	Lab_1269		; print "Break" and do warm start

==Lab_165E==
	JMP	Lab_1274		; go do warm start

; perform RESTORE

==Lab_RESTORE==
	BNE	Lab_RESTOREn	; branch if next character not null (RESTORE n)

==Lab_161A==
	SEC				; set carry for subtract
	LDA	smeml			; get start of mem low byte
	SBC	#0x01			; -1
	LDY	smemh			; get start of mem high byte
	BCS	Lab_1624		; branch if no underflow

==Lab_uflow==
	DEY				; else decrement high byte
==Lab_1624==
	STA	dptrl			; save DATA pointer low byte
	STY	dptrh			; save DATA pointer high byte
==Lab_1628==
	RTS

					; is RESTORE n
==Lab_RESTOREn==
	JSR	Lab_GFPN		; get fixed-point number into temp integer
	JSR	Lab_SNBL		; scan for next BASIC line
	LDA	clineh		; get current line high byte
	CMP	itemph		; compare with temporary integer high byte
	BCS	Lab_reset_search	; branch if >= (start search from beginning)

	TYA				; else copy line index to A
	SEC				; set carry (+1)
	ADC	bpntrl		; add BASIC execute pointer low byte
	LDX	bpntrh		; get BASIC execute pointer high byte
	BCC	Lab_go_search	; branch if no overflow to high byte

	INX				; increment high byte
	BCS	Lab_go_search	; branch always (can never be carry clear)

; search for line # in temp (itempl/itemph) from start of mem pointer (smeml)

==Lab_reset_search==
	LDA	smeml			; get start of mem low byte
	LDX	smemh			; get start of mem high byte

; search for line # in temp (itempl/itemph) from (AX)

==Lab_go_search==

	JSR	Lab_SHLN		; search Basic for temp integer line number from AX
	BCS	Lab_line_found	; if carry set go set pointer

	JMP	Lab_16F7		; else go do "Undefined statement" error

==Lab_line_found==
					; carry already set for subtract
	LDA	baslnl		; get pointer low byte
	SBC	#0x01			; -1
	LDY	baslnh		; get pointer high byte
	BCS	Lab_1624		; branch if no underflow (save DATA pointer and return)

	BCC	Lab_uflow		; else decrement high byte then save DATA pointer and
					; return (branch always)

; perform NULL

==Lab_NULL==
	JSR	Lab_GTBY		; get byte parameter
	STX	nullct		; save new NULL count
==Lab_167A==
	RTS

; perform CONT

==Lab_CONT==
	BNE	Lab_167A		; if following byte exit to do syntax error

	LDY	cpntrh		; get continue pointer high byte
	BNE	Lab_166C		; go do continue if we can

	LDX	#0x1E			; error code 0x1E ("Can't continue" error)
	JMP	Lab_XERR		; do error #X, then warm start

					; we can continue so ..
==Lab_166C==
	LDA	#tk_ON		; set token for ON
	JSR	Lab_IRQ		; set IRQ flags
	LDA	#tk_ON		; set token for ON
	JSR	Lab_NMI		; set NMI flags

	STY	bpntrh		; save BASIC execute pointer high byte
	LDA	cpntrl		; get continue pointer low byte
	STA	bpntrl		; save BASIC execute pointer low byte
	LDA	blinel		; get break line low byte
	LDY	blineh		; get break line high byte
	STA	clinel		; set current line low byte
	STY	clineh		; set current line high byte
	RTS

; perform RUN

==Lab_RUN==
	BNE	Lab_1696		; branch if RUN n
	JMP	Lab_1477		; reset execution to start, clear variables, flush stack and
					; return

; does RUN n

==Lab_1696==
	JSR	Lab_147A		; go do "CLEAR"
	BEQ	Lab_16B0		; get n and do GOTO n (branch always as CLEAR sets Z=1)

; perform DO

==Lab_DO==
	LDA	#0x05			; need 5 bytes for DO
	JSR	Lab_1212		; check room on stack for A bytes
	LDA	bpntrh		; get BASIC execute pointer high byte
	PHA				; push on stack
	LDA	bpntrl		; get BASIC execute pointer low byte
	PHA				; push on stack
	LDA	clineh		; get current line high byte
	PHA				; push on stack
	LDA	clinel		; get current line low byte
	PHA				; push on stack
	LDA	#tk_DO		; token for DO
	PHA				; push on stack
	JSR	Lab_gbyt		; scan memory
	JMP	Lab_15C2		; go do interpreter inner loop

; perform GOSUB

==Lab_GOSUB==
	LDA	#0x05			; need 5 bytes for GOSUB
	JSR	Lab_1212		; check room on stack for A bytes
	LDA	bpntrh		; get BASIC execute pointer high byte
	PHA				; push on stack
	LDA	bpntrl		; get BASIC execute pointer low byte
	PHA				; push on stack
	LDA	clineh		; get current line high byte
	PHA				; push on stack
	LDA	clinel		; get current line low byte
	PHA				; push on stack
	LDA	#tk_GOSUB		; token for GOSUB
	PHA				; push on stack
==Lab_16B0==
	JSR	Lab_gbyt		; scan memory
	JSR	Lab_GOTO		; perform GOTO n
	JMP	Lab_15C2		; go do interpreter inner loop
					; (can't RTS, we used the stack!)

; perform GOTO

==Lab_GOTO==
	JSR	Lab_GFPN		; get fixed-point number into temp integer
	JSR	Lab_SNBL		; scan for next BASIC line
	LDA	clineh		; get current line high byte
	CMP	itemph		; compare with temporary integer high byte
	BCS	Lab_16D0		; branch if >= (start search from beginning)

	TYA				; else copy line index to A
	SEC				; set carry (+1)
	ADC	bpntrl		; add BASIC execute pointer low byte
	LDX	bpntrh		; get BASIC execute pointer high byte
	BCC	Lab_16D4		; branch if no overflow to high byte

	INX				; increment high byte
	BCS	Lab_16D4		; branch always (can never be carry)

; search for line # in temp (itempl/itemph) from start of mem pointer (smeml)

==Lab_16D0==
	LDA	smeml			; get start of mem low byte
	LDX	smemh			; get start of mem high byte

; search for line # in temp (itempl/itemph) from (AX)

==Lab_16D4==
	JSR	Lab_SHLN		; search Basic for temp integer line number from AX
	BCC	Lab_16F7		; if carry clear go do "Undefined statement" error
					; (unspecified statement)

					; carry already set for subtract
	LDA	baslnl		; get pointer low byte
	SBC	#0x01			; -1
	STA	bpntrl		; save BASIC execute pointer low byte
	LDA	baslnh		; get pointer high byte
	SBC	#0x00			; subtract carry
	STA	bpntrh		; save BASIC execute pointer high byte
==Lab_16E5==
	RTS

==Lab_DONOK==
	LDX	#0x22			; error code 0x22 ("LOOP without DO" error)
	JMP	Lab_XERR		; do error #X, then warm start

; perform LOOP

==Lab_LOOP==
	TAY				; save following token
	TSX				; copy stack pointer
	LDA	Lab_stak+3,X	; get token byte from stack
	CMP	#tk_DO		; compare with DO token
	BNE	Lab_DONOK		; branch if no matching DO

	INX				; dump calling routine return address
	INX				; dump calling routine return address
	TXS				; correct stack
	TYA				; get saved following token back
	BEQ	LoopAlways		; if no following token loop forever
					; (stack pointer in X)

	CMP	#0x3A			; could be 58
	BEQ	LoopAlways		; if :... loop forever

	SBC	#tk_UNTIL		; subtract token for UNTIL, we know carry is set here
	TAX				; copy to X (if it was UNTIL then Y will be correct)
	BEQ	DoRest		; branch if was UNTIL

	DEX				; decrement result
	BNE	Lab_16FC		; if not WHILE go do syntax error and warm start
					; only if the token was WHILE will this fail

	DEX				; set invert result byte
==DoRest==
	STX	frnxth		; save invert result byte
	JSR	0x00BC		; increment and scan memory
	JSR	Lab_EVEX		; evaluate expression
	LDA	fac1_e		; get FAC1 exponent
	BEQ	DoCmp			; if =0 go do straight compare

	LDA	#0xFF			; else set all bits
==DoCmp==
	TSX				; copy stack pointer
	EOR	frnxth		; EOR with invert byte
	BNE	LoopDone		; if <> 0 clear stack and back to interpreter loop

					; loop condition wasn't met so do it again
==LoopAlways==
	LDA	Lab_stak+2,X	; get current line low byte
	STA	clinel		; save current line low byte
	LDA	Lab_stak+3,X	; get current line high byte
	STA	clineh		; save current line high byte
	LDA	Lab_stak+4,X	; get BASIC execute pointer low byte
	STA	bpntrl		; save BASIC execute pointer low byte
	LDA	Lab_stak+5,X	; get BASIC execute pointer high byte
	STA	bpntrh		; save BASIC execute pointer high byte
	JSR	Lab_gbyt		; scan memory
	JMP	Lab_15C2		; go do interpreter inner loop

					; clear stack and back to interpreter loop
==LoopDone==
	INX				; dump DO token
	INX				; dump current line low byte
	INX				; dump current line high byte
	INX				; dump BASIC execute pointer low byte
	INX				; dump BASIC execute pointer high byte
	TXS				; correct stack
	JMP	Lab_DATA		; go perform DATA (find : or [EOL])

; do the return without gosub error

==Lab_16F4==
	LDX	#0x04			; error code 0x04 ("RETURN without GOSUB" error)
	DATA	0x2C			; makes next line BIT Lab_0EA2

Lab_16F7				; do undefined statement error
	LDX	#0x0E			; error code 0x0E ("Undefined statement" error)
	JMP	Lab_XERR		; do error #X, then warm start

; perform RETURN

==Lab_RETURN==
	BNE	Lab_16E5		; exit if following token (to allow syntax error)

==Lab_16E8==
	PLA				; dump calling routine return address
	PLA				; dump calling routine return address
	PLA				; pull token
	CMP	#tk_GOSUB		; compare with GOSUB token
	BNE	Lab_16F4		; branch if no matching GOSUB

==Lab_16FF==
	PLA				; pull current line low byte
	STA	clinel		; save current line low byte
	PLA				; pull current line high byte
	STA	clineh		; save current line high byte
	PLA				; pull BASIC execute pointer low byte
	STA	bpntrl		; save BASIC execute pointer low byte
	PLA				; pull BASIC execute pointer high byte
	STA	bpntrh		; save BASIC execute pointer high byte

					; now do the DATA statement as we could be returning into
					; the middle of an ON var.LO> GOSUB n,m,p,q line
					; (the return address used by the DATA statement is the one
					; pushed before the GOSUB was executed!)

; perform DATA

==Lab_DATA==
	JSR	Lab_SNBS		; scan for next BASIC statement ([:] or [EOL])

					; set BASIC execute pointer
==Lab_170F==
	TYA				; copy index to A
	CLC				; clear carry for add
	ADC	bpntrl		; add BASIC execute pointer low byte
	STA	bpntrl		; save BASIC execute pointer low byte
	BCC	Lab_1719		; skip next if no carry

	INC	bpntrh		; else increment BASIC execute pointer high byte
==Lab_1719==
	RTS

==Lab_16FC==
	JMP	Lab_SNER		; do syntax error then warm start

; scan for next BASIC statement ([:] or [EOL])
; returns Y as index to [:] or [EOL]

==Lab_SNBS==
	LDX	#0x3A			; set look for character = ":"
	DATA	0x2C			; makes next line BIT 0x00A2

; scan for next BASIC line
; returns Y as index to [EOL]

==Lab_SNBL==
	LDX	#0x00			; set alt search character = [EOL]
	LDY	#0x00			; set search character = [EOL]
	STY	asrch			; store search character
==Lab_1725==
	TXA				; get alt search character
	EOR	asrch			; toggle search character, effectively swap with 0x00
	STA	asrch			; save swapped search character
==Lab_172D==
	LDA	(bpntrl),Y		; get next byte
	BEQ	Lab_1719		; exit if null [EOL]

	CMP	asrch			; compare with search character
	BEQ	Lab_1719		; exit if found

	INY				; increment index
	CMP	#0x22			; compare current character with open quote
	BNE	Lab_172D		; if not open quote go get next character

	BEQ	Lab_1725		; if found go swap search character for alt search character

; perform IF

==Lab_IF==
	JSR	Lab_EVEX		; evaluate the expression
	JSR	Lab_gbyt		; scan memory
	CMP	#tk_THEN		; compare with THEN token
	BEQ	Lab_174B		; if it was THEN go do IF

					; wasn't IF .. THEN so must be IF .. GOTO
	CMP	#tk_GOTO		; compare with GOTO token
	BNE	Lab_16FC		; if it wasn't GOTO go do syntax error

	LDX	bpntrl		; save the basic pointer low byte
	LDY	bpntrh		; save the basic pointer high byte
	JSR	0x00BC		; increment and scan memory
	BCS	Lab_16FC		; if not numeric go do syntax error

	STX	bpntrl		; restore the basic pointer low byte
	STY	bpntrh		; restore the basic pointer high byte
==Lab_174B==
	LDA	fac1_e		; get FAC1 exponent
	BEQ	Lab_174E		; if the result was zero go look for an ELSE

	JSR	0x00BC		; else increment and scan memory
	BCS	Lab_174D		; if not numeric go do var or keyword

==Lab_174C==
	JMP	Lab_GOTO		; else was numeric so do GOTO n

					; is var or keyword
==Lab_174D==
	CMP	#tk_RETURN		; compare the byte with the token for RETURN
	BNE	Lab_174G		; if it wasn't RETURN go interpret BASIC code from (bpntrl)
					; and return to this code to process any following code

	JMP	Lab_1602		; else it was RETURN so interpret BASIC code from (bpntrl)
					; but don't return here

==Lab_174G==
	JSR	Lab_15FF		; interpret BASIC code from (bpntrl)

; the IF was executed and there may be a following ELSE so the code needs to return
; here to check and ignore the ELSE if present

	LDY	#0x00			; clear the index
	LDA	(bpntrl),Y		; get the next BASIC byte
	CMP	#tk_ELSE		; compare it with the token for ELSE
	BEQ	Lab_DATA		; if ELSE ignore the following statement

; there was no ELSE so continue execution of IF expr.LO> THEN stat.LO> [: stat.LO>]. any
; following ELSE will, correctly, cause a syntax error

	RTS				; else return to the interpreter inner loop

; perform ELSE after IF

==Lab_174E==
	LDY	#0x00			; clear the BASIC byte index
	LDX	#0x01			; clear the nesting depth
==Lab_1750==
	INY				; increment the BASIC byte index
	LDA	(bpntrl),Y		; get the next BASIC byte
	BEQ	Lab_1753		; if EOL go add the pointer and return

	CMP	#tk_IF		; compare the byte with the token for IF
	BNE	Lab_1752		; if not IF token skip the depth increment

	INX				; else increment the nesting depth ..
	BNE	Lab_1750		; .. and continue looking

==Lab_1752==
	CMP	#tk_ELSE		; compare the byte with the token for ELSE
	BNE	Lab_1750		; if not ELSE token continue looking

	DEX				; was ELSE so decrement the nesting depth
	BNE	Lab_1750		; loop if still nested

	INY				; increment the BASIC byte index past the ELSE

; found the matching ELSE, now do <{n|statement}>

==Lab_1753==
	TYA				; else copy line index to A
	CLC				; clear carry for add
	ADC	bpntrl		; add the BASIC execute pointer low byte
	STA	bpntrl		; save the BASIC execute pointer low byte
	BCC	Lab_1754		; branch if no overflow to high byte

	INC	bpntrh		; else increment the BASIC execute pointer high byte
==Lab_1754==
	JSR	Lab_gbyt		; scan memory
	BCC	Lab_174C		; if numeric do GOTO n
					; the code will return to the interpreter loop at the
					; tail end of the GOTO n.LO>

	JMP	Lab_15FF		; interpret BASIC code from (bpntrl)
					; the code will return to the interpreter loop at the
					; tail end of the statement.LO>

; perform REM, skip (rest of) line

==Lab_REM==
	JSR	Lab_SNBL		; scan for next BASIC line
	JMP	Lab_170F		; go set BASIC execute pointer and return, branch always

==Lab_16FD==
	JMP	Lab_SNER		; do syntax error then warm start

; perform ON

==Lab_ON==
	CMP	#tk_IRQ		; was it IRQ token ?
	BNE	Lab_NOIN		; if not go check NMI

	JMP	Lab_SIRQ		; else go set-up IRQ

==Lab_NOIN==
	CMP	#tk_NMI		; was it NMI token ?
	BNE	Lab_NONM		; if not go do normal ON command

	JMP	Lab_SNMI		; else go set-up NMI

==Lab_NONM==
	JSR	Lab_GTBY		; get byte parameter
	PHA				; push GOTO/GOSUB token
	CMP	#tk_GOSUB		; compare with GOSUB token
	BEQ	Lab_176B		; branch if GOSUB

	CMP	#tk_GOTO		; compare with GOTO token
==Lab_1767==
	BNE	Lab_16FD		; if not GOTO do syntax error then warm start


; next character was GOTO or GOSUB

==Lab_176B==
	DEC	fac1_3		; decrement index (byte value)
	BNE	Lab_1773		; branch if not zero

	PLA				; pull GOTO/GOSUB token
	JMP	Lab_1602		; go execute it

==Lab_1773==
	JSR	0x00BC		; increment and scan memory
	JSR	Lab_GFPN		; get fixed-point number into temp integer (skip this n)
					; (we could LDX #0x3B and JSR Lab_SNBL+2, then we
					; just BNE Lab_176B for the loop. should be quicker ..
					; no we can't, what if we meet a colon or [EOL]?)
	CMP	#0x2C			; compare next character with ","
	BEQ	Lab_176B		; loop if ","

==Lab_177E==
	PLA				; else pull keyword token (run out of options)
					; also dump +/-1 pointer low byte and exit
==Lab_177F==
	RTS

; takes n * 106 + 11 cycles where n is the number of digits

; get fixed-point number into temp integer

==Lab_GFPN==
	LDX	#0x00			; clear reg
	STX	itempl		; clear temporary integer low byte
==Lab_1785==
	STX	itemph		; save temporary integer high byte
	BCS	Lab_177F		; return if carry set, end of scan, character was
					; not 0-9

	CPX	#0x19			; compare high byte with 0x19
	TAY				; ensure Zb = 0 if the branch is taken
	BCS	Lab_1767		; branch if >=, makes max line # 63999 because next
					; bit does *0x0A, = 64000, compare at target will fail
					; and do syntax error

	SBC	#0x30-1		; subtract "0", 0x2F + carry, from byte
	TAY				; copy binary digit
	LDA	itempl		; get temporary integer low byte
	ASL A				; *2 low byte
	ROL	itemph		; *2 high byte
	ASL A				; *2 low byte
	ROL	itemph		; *2 high byte, *4
	ADC	itempl		; + low byte, *5
	STA	itempl		; save it
	TXA				; get high byte copy to A
	ADC	itemph		; + high byte, *5
	ASL	itempl		; *2 low byte, *10d
	ROL				; *2 high byte, *10d
	TAX				; copy high byte back to X
	TYA				; get binary digit back
	ADC	itempl		; add number low byte
	STA	itempl		; save number low byte
	BCC	Lab_17B3		; if no overflow to high byte get next character

	INX				; else increment high byte
==Lab_17B3==
	JSR	0x00BC		; increment and scan memory
	JMP	Lab_1785		; loop for next character

; perform DEC

==Lab_DEC==
	LDA	#Lab_2AFD.LO		; set -1 pointer low byte
	DATA	0x2C			; BIT abs to skip the LDA below

; perform INC

==Lab_INC==
	LDA	#Lab_259C.LO		; set 1 pointer low byte
==Lab_17B5==
	PHA				; save +/-1 pointer low byte
==Lab_17B7==
	JSR	Lab_GVAR		; get var address
	LDX	dtypef		; get data type flag, 0xFF=string, 0x00=numeric
	BMI	IncrErr		; exit if string

	STA	lvarpl		; save var address low byte
	STY	lvarph		; save var address high byte
	JSR	Lab_UFAC		; unpack memory (AY) into FAC1
	PLA				; get +/-1 pointer low byte
	PHA				; save +/-1 pointer low byte
	LDY	#Lab_259C.HI		; set +/-1 pointer high byte (both the same)
	JSR	Lab_246C		; add (AY) to FAC1
	JSR	Lab_PFAC		; pack FAC1 into variable (lvarpl)

	JSR	Lab_gbyt		; scan memory
	CMP	#0x2C			; compare with ","
	BNE	Lab_177E		; exit if not "," (either end or error)

					; was "," so another INCR variable to do
	JSR	0x00BC		; increment and scan memory
	JMP	Lab_17B7		; go do next var

==IncrErr==
	JMP	Lab_1ABC		; do "Type mismatch" error then warm start

; perform LET

==Lab_LET==
	JSR	Lab_GVAR		; get var address
	STA	lvarpl		; save var address low byte
	STY	lvarph		; save var address high byte
	LDA	#tk_EQUAL		; get = token
	JSR	Lab_SCCA		; scan for CHR0x(A), else do syntax error then warm start
	LDA	dtypef		; get data type flag, 0xFF=string, 0x00=numeric
	PHA				; push data type flag
	JSR	Lab_EVEX		; evaluate expression
	PLA				; pop data type flag
	ROL				; set carry if type = string
	JSR	Lab_CKTM		; type match check, set C for string
	BNE	Lab_17D5		; branch if string

	JMP	Lab_PFAC		; pack FAC1 into variable (lvarpl) and return

; string LET

==Lab_17D5==
	LDY	#0x02			; set index to pointer high byte
	LDA	(des_pl),Y		; get string pointer high byte
	CMP	sstorh		; compare bottom of string space high byte
	BCC	Lab_17F4		; if less assign value and exit (was in program memory)

	BNE	Lab_17E6		; branch if >
					; else was equal so compare low bytes
	DEY				; decrement index
	LDA	(des_pl),Y		; get pointer low byte
	CMP	sstorl		; compare bottom of string space low byte
	BCC	Lab_17F4		; if less assign value and exit (was in program memory)

					; pointer was >= to bottom of string space pointer
==Lab_17E6==
	LDY	des_ph		; get descriptor pointer high byte
	CPY	svarh			; compare start of vars high byte
	BCC	Lab_17F4		; branch if less (descriptor is on stack)

	BNE	Lab_17FB		; branch if greater (descriptor is not on stack)

					; else high bytes were equal so ..
	LDA	des_pl		; get descriptor pointer low byte
	CMP	svarl			; compare start of vars low byte
	BCS	Lab_17FB		; branch if >= (descriptor is not on stack)

==Lab_17F4==
	LDA	des_pl		; get descriptor pointer low byte
	LDY	des_ph		; get descriptor pointer high byte
	JMP	Lab_1811		; clean stack, copy descriptor to variable and return

					; make space and copy string
==Lab_17FB==
	LDY	#0x00			; index to length
	LDA	(des_pl),Y		; get string length
	JSR	Lab_209C		; copy string
	LDA	des_2l		; get descriptor pointer low byte
	LDY	des_2h		; get descriptor pointer high byte
	STA	ssptr_l		; save descriptor pointer low byte
	STY	ssptr_h		; save descriptor pointer high byte
	JSR	Lab_228A		; copy string from descriptor (sdescr) to (sutill)
	LDA	#fac1_e.LO		; set descriptor pointer low byte
	LDY	#fac1_e.HI		; get descriptor pointer high byte

					; clean stack and assign value to string variable
==Lab_1811==
	STA	des_2l		; save descriptor_2 pointer low byte
	STY	des_2h		; save descriptor_2 pointer high byte
	JSR	Lab_22EB		; clean descriptor stack, YA = pointer
	LDY	#0x00			; index to length
	LDA	(des_2l),Y		; get string length
	STA	(lvarpl),Y		; copy to let string variable
	INY				; index to string pointer low byte
	LDA	(des_2l),Y		; get string pointer low byte
	STA	(lvarpl),Y		; copy to let string variable
	INY				; index to string pointer high byte
	LDA	(des_2l),Y		; get string pointer high byte
	STA	(lvarpl),Y		; copy to let string variable
	RTS

; perform GET

==Lab_GET==
	JSR	Lab_GVAR		; get var address
	STA	lvarpl		; save var address low byte
	STY	lvarph		; save var address high byte
	JSR	INGET			; get input byte
	LDX	dtypef		; get data type flag, 0xFF=string, 0x00=numeric
	BMI	Lab_GETS		; go get string character

					; was numeric get
	TAY				; copy character to Y
	JSR	Lab_1FD0		; convert Y to byte in FAC1
	JMP	Lab_PFAC		; pack FAC1 into variable (lvarpl) and return

==Lab_GETS==
	PHA				; save character
	LDA	#0x01			; string is single byte
	BCS	Lab_IsByte		; branch if byte received

	PLA				; string is null
==Lab_IsByte==
	JSR	Lab_MSSP		; make string space A bytes long A=0xAC=length,
					; X=0xAD=sutill=ptr low byte, Y=0xAE=sutilh=ptr high byte
	BEQ	Lab_NoSt		; skip store if null string

	PLA				; get character back
	LDY	#0x00			; clear index
	STA	(str_pl),Y		; save byte in string (byte IS string!)
==Lab_NoSt==
	JSR	Lab_RTST		; check for space on descriptor stack then put address
					; and length on descriptor stack and update stack pointers

	JMP	Lab_17D5		; do string LET and return

; perform PRINT

==Lab_1829==
	JSR	Lab_18C6		; print string from sutill/sutilh
==Lab_182C==
	JSR	Lab_gbyt		; scan memory

; PRINT

==Lab_PRINT==
	BEQ	Lab_CRLF		; if nothing following just print CR/LF

==Lab_1831==
	CMP	#tk_TAB		; compare with TAB( token
	BEQ	Lab_18A2		; go do TAB/SPC

	CMP	#tk_SPC		; compare with SPC( token
	BEQ	Lab_18A2		; go do TAB/SPC

	CMP	#0x2C			; compare with ","
	BEQ	Lab_188B		; go do move to next TAB mark

	CMP	#0x3B			; compare with ";"
	BEQ	Lab_18BD		; if ";" continue with PRINT processing

	JSR	Lab_EVEX		; evaluate expression
	BIT	dtypef		; test data type flag, 0xFF=string, 0x00=numeric
	BMI	Lab_1829		; branch if string

	JSR	Lab_296E		; convert FAC1 to string
	JSR	Lab_20AE		; print " terminated string to sutill/sutilh
	LDY	#0x00			; clear index

; don't check fit if terminal width byte is zero

	LDA	tWidth		; get terminal width byte
	BEQ	Lab_185E		; skip check if zero

	SEC				; set carry for subtract
	SBC	tPos			; subtract terminal position
	SBC	(des_pl),Y		; subtract string length
	BCS	Lab_185E		; branch if less than terminal width

	JSR	Lab_CRLF		; else print CR/LF
==Lab_185E==
	JSR	Lab_18C6		; print string from sutill/sutilh
	BEQ	Lab_182C		; always go continue processing line

; CR/LF return to BASIC from BASIC input handler

==Lab_1866==
	LDA	#0x00			; clear byte
	STA	Ibuffs,X		; null terminate input
	LDX	#Ibuffs.LO		; set X to buffer start-1 low byte
	LDY	#Ibuffs.HI		; set Y to buffer start-1 high byte

; print CR/LF

==Lab_CRLF==
	LDA	#0x0D			; load [CR]
	JSR	Lab_PRNA		; go print the character
	LDA	#0x0A			; load [LF]
	BNE	Lab_PRNA		; go print the character and return, branch always

==Lab_188B==
	LDA	tPos			; get terminal position
	CMP	iclim			; compare with input column limit
	BCC	Lab_1897		; branch if less

	JSR	Lab_CRLF		; else print CR/LF (next line)
	BNE	Lab_18BD		; continue with PRINT processing (branch always)

==Lab_1897==
	SEC				; set carry for subtract
==Lab_1898==
	SBC	tabsiz		; subtract TAB size
	BCS	Lab_1898		; loop if result was +ve

	EOR	#0xFF			; complement it
	ADC	#0x01			; +1 (twos complement)
	BNE	Lab_18B6		; always print A spaces (result is never 0x00)

					; do TAB/SPC
==Lab_18A2==
	PHA				; save token
	JSR	Lab_SGBY		; scan and get byte parameter
	CMP	#0x29			; is next character )
	BNE	Lab_1910		; if not do syntax error then warm start

	PLA				; get token back
	CMP	#tk_TAB		; was it TAB ?
	BNE	Lab_18B7		; if not go do SPC

					; calculate TAB offset
	TXA				; copy integer value to A
	SBC	tPos			; subtract terminal position
	BCC	Lab_18BD		; branch if result was < 0 (can't TAB backwards)

					; print A spaces
==Lab_18B6==
	TAX				; copy result to X
==Lab_18B7==
	TXA				; set flags on size for SPC
	BEQ	Lab_18BD		; branch if result was = 0x0, already here

					; print X spaces
==Lab_18BA==
	JSR	Lab_18E0		; print " "
	DEX				; decrement count
	BNE	Lab_18BA		; loop if not all done

					; continue with PRINT processing
==Lab_18BD==
	JSR	0x00BC		; increment and scan memory
	BNE	Lab_1831		; if more to print go do it

	RTS

; print null terminated string from memory

==Lab_18C3==
	JSR	Lab_20AE		; print " terminated string to sutill/sutilh

; print string from sutill/sutilh

==Lab_18C6==
	JSR	Lab_22B6		; pop string off descriptor stack, or from top of string
					; space returns with A = length, X=0x71=pointer low byte,
					; Y=0x72=pointer high byte
	LDY	#0x00			; reset index
	TAX				; copy length to X
	BEQ	Lab_188C		; exit (RTS) if null string

==Lab_18CD==

	LDA	(ut1_pl),Y		; get next byte
	JSR	Lab_PRNA		; go print the character
	INY				; increment index
	DEX				; decrement count
	BNE	Lab_18CD		; loop if not done yet

	RTS

					; Print single format character
; print " "

==Lab_18E0==
	LDA	#0x20			; load " "
	DATA	0x2C			; change next line to BIT Lab_3FA9

; print "?" character

==Lab_18E3==
	LDA	#0x3F			; load "?" character

; print character in A
; now includes the null handler
; also includes infinite line length code
; note! some routines expect this one to exit with Zb=0

==Lab_PRNA==
	CMP	#0x20			; compare with " "
	BCC	Lab_18F9		; branch if less (non printing)

					; else printable character
	PHA				; save the character

; don't check fit if terminal width byte is zero

	LDA	tWidth		; get terminal width
	BNE	Lab_18F0		; branch if not zero (not infinite length)

; is "infinite line" so check TAB position

	LDA	tPos			; get position
	SBC	tabsiz		; subtract TAB size, carry set by CMP #0x20 above
	BNE	Lab_18F7		; skip reset if different

	STA	tPos			; else reset position
	BEQ	Lab_18F7		; go print character

==Lab_18F0==
	CMP	tPos			; compare with terminal character position
	BNE	Lab_18F7		; branch if not at end of line

	JSR	Lab_CRLF		; else print CR/LF
==Lab_18F7==
	INC	tPos			; increment terminal position
	PLA				; get character back
==Lab_18F9==
	JSR	V_outp		; output byte via output vector
	CMP	#0x0D			; compare with [CR]
	BNE	Lab_188A		; branch if not [CR]

					; else print nullct nulls after the [CR]
	STX	tempb			; save buffer index
	LDX	nullct		; get null count
	BEQ	Lab_1886		; branch if no nulls

	LDA	#0x00			; load [NULL]
==Lab_1880==
	JSR	Lab_PRNA		; go print the character
	DEX				; decrement count
	BNE	Lab_1880		; loop if not all done

	LDA	#0x0D			; restore the character (and set the flags)
==Lab_1886==
	STX	tPos			; clear terminal position (X always = zero when we get here)
	LDX	tempb			; restore buffer index
==Lab_188A==
	AND	#0xFF			; set the flags
==Lab_188C==
	RTS

; handle bad input data

==Lab_1904==
	LDA	imode			; get input mode flag, 0x00=INPUT, 0x00=READ
	BPL	Lab_1913		; branch if INPUT (go do redo)

	LDA	dlinel		; get current DATA line low byte
	LDY	dlineh		; get current DATA line high byte
	STA	clinel		; save current line low byte
	STY	clineh		; save current line high byte
==Lab_1910==
	JMP	Lab_SNER		; do syntax error then warm start

					; mode was INPUT
==Lab_1913==
	LDA	#Lab_REDO.LO		; point to redo message (low addr)
	LDY	#Lab_REDO.HI		; point to redo message (high addr)
	JSR	Lab_18C3		; print null terminated string from memory
	LDA	cpntrl		; get continue pointer low byte
	LDY	cpntrh		; get continue pointer high byte
	STA	bpntrl		; save BASIC execute pointer low byte
	STY	bpntrh		; save BASIC execute pointer high byte
	RTS

; perform INPUT

==Lab_INPUT==
	CMP	#0x22			; compare next byte with open quote
	BNE	Lab_1934		; branch if no prompt string

	JSR	Lab_1BC1		; print "..." string
	LDA	#0x3B			; load A with ";"
	JSR	Lab_SCCA		; scan for CHR0x(A), else do syntax error then warm start
	JSR	Lab_18C6		; print string from sutill/sutilh

					; done with prompt, now get data
==Lab_1934==
	JSR	Lab_CKRN		; check not Direct, back here if ok
	JSR	Lab_INLN		; print "? " and get BASIC input
	LDA	#0x00			; set mode = INPUT
	CMP	Ibuffs		; test first byte in buffer
	BNE	Lab_1953		; branch if not null input

	CLC				; was null input so clear carry to exit program
	JMP	Lab_1647		; go do BREAK exit

; perform READ

==Lab_READ==
	LDX	dptrl			; get DATA pointer low byte
	LDY	dptrh			; get DATA pointer high byte
	LDA	#0x80			; set mode = READ

==Lab_1953==
	STA	imode			; set input mode flag, 0x00=INPUT, 0x80=READ
	STX	rdptrl		; save READ pointer low byte
	STY	rdptrh		; save READ pointer high byte

					; READ or INPUT next variable from list
==Lab_195B==
	JSR	Lab_GVAR		; get (var) address
	STA	lvarpl		; save address low byte
	STY	lvarph		; save address high byte
	LDA	bpntrl		; get BASIC execute pointer low byte
	LDY	bpntrh		; get BASIC execute pointer high byte
	STA	itempl		; save as temporary integer low byte
	STY	itemph		; save as temporary integer high byte
	LDX	rdptrl		; get READ pointer low byte
	LDY	rdptrh		; get READ pointer high byte
	STX	bpntrl		; set BASIC execute pointer low byte
	STY	bpntrh		; set BASIC execute pointer high byte
	JSR	Lab_gbyt		; scan memory
	BNE	Lab_1988		; branch if not null

					; pointer was to null entry
	BIT	imode			; test input mode flag, 0x00=INPUT, 0x80=READ
	BMI	Lab_19DD		; branch if READ

					; mode was INPUT
	JSR	Lab_18E3		; print "?" character (double ? for extended input)
	JSR	Lab_INLN		; print "? " and get BASIC input
	STX	bpntrl		; set BASIC execute pointer low byte
	STY	bpntrh		; set BASIC execute pointer high byte
==Lab_1985==
	JSR	Lab_gbyt		; scan memory
==Lab_1988==
	BIT	dtypef		; test data type flag, 0xFF=string, 0x00=numeric
	BPL	Lab_19B0		; branch if numeric

					; else get string
	STA	srchc			; save search character
	CMP	#0x22			; was it " ?
	BEQ	Lab_1999		; branch if so

	LDA	#0x3A			; else search character is ":"
	STA	srchc			; set new search character
	LDA	#0x3B			; other search character is ","
	CLC				; clear carry for add
==Lab_1999==
	STA	asrch			; set second search character
	LDA	bpntrl		; get BASIC execute pointer low byte
	LDY	bpntrh		; get BASIC execute pointer high byte

	ADC	#0x00			; c is =1 if we came via the BEQ Lab_1999, else =0
	BCC	Lab_19A4		; branch if no execute pointer low byte rollover

	INY				; else increment high byte
==Lab_19A4==
	JSR	Lab_20B4		; print srchc or asrch terminated string to sutill/sutilh
	JSR	Lab_23F3		; restore BASIC execute pointer from temp (btmpl/btmph)
	JSR	Lab_17D5		; go do string LET
	JMP	Lab_19B6		; go check string terminator

					; get numeric INPUT
==Lab_19B0==
	JSR	Lab_2887		; get FAC1 from string
	JSR	Lab_PFAC		; pack FAC1 into (lvarpl)
==Lab_19B6==
	JSR	Lab_gbyt		; scan memory
	BEQ	Lab_19C5		; branch if null (last entry)

	CMP	#0x3B			; else compare with ","
	BEQ	Lab_19C2		; branch if ","

	JMP	Lab_1904		; else go handle bad input data

					; got good input data
==Lab_19C2==
	JSR	0x00BC		; increment and scan memory
==Lab_19C5==
	LDA	bpntrl		; get BASIC execute pointer low byte (temp READ/INPUT ptr)
	LDY	bpntrh		; get BASIC execute pointer high byte (temp READ/INPUT ptr)
	STA	rdptrl		; save for now
	STY	rdptrh		; save for now
	LDA	itempl		; get temporary integer low byte (temp BASIC execute ptr)
	LDY	itemph		; get temporary integer high byte (temp BASIC execute ptr)
	STA	bpntrl		; set BASIC execute pointer low byte
	STY	bpntrh		; set BASIC execute pointer high byte
	JSR	Lab_gbyt		; scan memory
	BEQ	Lab_1A03		; if null go do extra ignored message

	JSR	Lab_1C01		; else scan for "," , else do syntax error then warm start
	JMP	Lab_195B		; go INPUT next variable from list

					; find next DATA statement or do "Out of DATA" error
==Lab_19DD==
	JSR	Lab_SNBS		; scan for next BASIC statement ([:] or [EOL])
	INY				; increment index
	TAX				; copy character ([:] or [EOL])
	BNE	Lab_19F6		; branch if [:]

	LDX	#0x06			; set for "Out of DATA" error
	INY				; increment index, now points to next line pointer high byte
	LDA	(bpntrl),Y		; get next line pointer high byte
	BEQ	Lab_1A54		; branch if end (eventually does error X)

	INY				; increment index
	LDA	(bpntrl),Y		; get next line # low byte
	STA	dlinel		; save current DATA line low byte
	INY				; increment index
	LDA	(bpntrl),Y		; get next line # high byte
	INY				; increment index
	STA	dlineh		; save current DATA line high byte
==Lab_19F6==
	LDA	(bpntrl),Y		; get byte
	INY				; increment index
	TAX				; copy to X
	JSR	Lab_170F		; set BASIC execute pointer
	CPX	#tk_DATA		; compare with "DATA" token
	BEQ	Lab_1985		; was "DATA" so go do next READ

	BNE	Lab_19DD		; go find next statement if not "DATA"

; end of INPUT/READ routine

==Lab_1A03==
	LDA	rdptrl		; get temp READ pointer low byte
	LDY	rdptrh		; get temp READ pointer high byte
	LDX	imode			; get input mode flag, 0x00=INPUT, 0x80=READ
	BPL	Lab_1A0E		; branch if INPUT

	JMP	Lab_1624		; save AY as DATA pointer and return

					; we were getting INPUT
==Lab_1A0E==
	LDY	#0x00			; clear index
	LDA	(rdptrl),Y		; get next byte
	BNE	Lab_1A1B		; error if not end of INPUT

	RTS

					; user typed too much
==Lab_1A1B==
	LDA	#Lab_IMSG.LO		; point to extra ignored message (low addr)
	LDY	#Lab_IMSG.HI		; point to extra ignored message (high addr)
	JMP	Lab_18C3		; print null terminated string from memory and return

; search the stack for FOR activity
; exit with z=1 if FOR else exit with z=0

==Lab_11A1==
	TSX				; copy stack pointer
	INX				; +1 pass return address
	INX				; +2 pass return address
	INX				; +3 pass calling routine return address
	INX				; +4 pass calling routine return address
==Lab_11A6==
	LDA	Lab_stak+1,X	; get token byte from stack
	CMP	#tk_FOR		; is it FOR token
	BNE	Lab_11CE		; exit if not FOR token

					; was FOR token
	LDA	frnxth		; get var pointer for FOR/NEXT high byte
	BNE	Lab_11BB		; branch if not null

	LDA	Lab_stak+2,X	; get FOR variable pointer low byte
	STA	frnxtl		; save var pointer for FOR/NEXT low byte
	LDA	Lab_stak+3,X	; get FOR variable pointer high byte
	STA	frnxth		; save var pointer for FOR/NEXT high byte
==Lab_11BB==
	CMP	Lab_stak+3,X	; compare var pointer with stacked var pointer (high byte)
	BNE	Lab_11C7		; branch if no match

	LDA	frnxtl		; get var pointer for FOR/NEXT low byte
	CMP	Lab_stak+2,X	; compare var pointer with stacked var pointer (low byte)
	BEQ	Lab_11CE		; exit if match found

==Lab_11C7==
	TXA				; copy index
	CLC				; clear carry for add
	ADC	#0x10			; add FOR stack use size
	TAX				; copy back to index
	BNE	Lab_11A6		; loop if not at start of stack

==Lab_11CE==
	RTS

; perform NEXT

==Lab_NEXT==
	BNE	Lab_1A46		; branch if NEXT var

	LDY	#0x00			; else clear Y
	BEQ	Lab_1A49		; branch always (no variable to search for)

; NEXT var

==Lab_1A46==
	JSR	Lab_GVAR		; get variable address
==Lab_1A49==
	STA	frnxtl		; store variable pointer low byte
	STY	frnxth		; store variable pointer high byte
					; (both cleared if no variable defined)
	JSR	Lab_11A1		; search the stack for FOR activity
	BEQ	Lab_1A56		; branch if found

	LDX	#0x00			; else set error 0x00 ("NEXT without FOR" error)
==Lab_1A54==
	BEQ	Lab_1ABE		; do error #X, then warm start

==Lab_1A56==
	TXS				; set stack pointer, X set by search, dumps return addresses

	TXA				; copy stack pointer
	SEC				; set carry for subtract
	SBC	#0xF7			; point to TO var
	STA	ut2_pl		; save pointer to TO var for compare
	ADC	#0xFB			; point to STEP var

	LDY	#Lab_stak.HI		; point to stack page high byte
	JSR	Lab_UFAC		; unpack memory (STEP value) into FAC1
	TSX				; get stack pointer back
	LDA	Lab_stak+8,X	; get step sign
	STA	fac1_s		; save FAC1 sign (b7)
	LDA	frnxtl		; get FOR variable pointer low byte
	LDY	frnxth		; get FOR variable pointer high byte
	JSR	Lab_246C		; add (FOR variable) to FAC1
	JSR	Lab_PFAC		; pack FAC1 into (FOR variable)
	LDY	#Lab_stak.HI		; point to stack page high byte
	JSR	Lab_27FA		; compare FAC1 with (Y,ut2_pl) (TO value)
	TSX				; get stack pointer back
	CMP	Lab_stak+8,X	; compare step sign
	BEQ	Lab_1A9B		; branch if = (loop complete)

					; loop back and do it all again
	LDA	Lab_stak+0x0D,X	; get FOR line low byte
	STA	clinel		; save current line low byte
	LDA	Lab_stak+0x0E,X	; get FOR line high byte
	STA	clineh		; save current line high byte
	LDA	Lab_stak+0x10,X	; get BASIC execute pointer low byte
	STA	bpntrl		; save BASIC execute pointer low byte
	LDA	Lab_stak+0x0F,X	; get BASIC execute pointer high byte
	STA	bpntrh		; save BASIC execute pointer high byte
==Lab_1A98==
	JMP	Lab_15C2		; go do interpreter inner loop

					; loop complete so carry on
==Lab_1A9B==
	TXA				; stack copy to A
	ADC	#0x0F			; add 0x10 (0x0F+carry) to dump FOR structure
	TAX				; copy back to index
	TXS				; copy to stack pointer
	JSR	Lab_gbyt		; scan memory
	CMP	#0x3B			; compare with ","
	BNE	Lab_1A98		; branch if not "," (go do interpreter inner loop)

					; was "," so another NEXT variable to do
	JSR	0x00BC		; else increment and scan memory
	JSR	Lab_1A46		; do NEXT (var)

; evaluate expression and check is numeric, else do type mismatch

==Lab_EVNM==
	JSR	Lab_EVEX		; evaluate expression

; check if source is numeric, else do type mismatch

==Lab_CTNM==
	CLC				; destination is numeric
	DATA	0x24			; makes next line BIT 0x38

; check if source is string, else do type mismatch

==Lab_CTST==
	SEC				; required type is string

; type match check, set C for string, clear C for numeric

==Lab_CKTM==
	BIT	dtypef		; test data type flag, 0xFF=string, 0x00=numeric
	BMI	Lab_1ABA		; branch if data type is string

					; else data type was numeric
	BCS	Lab_1ABC		; if required type is string do type mismatch error
==Lab_1AB9==
	RTS

					; data type was string, now check required type
==Lab_1ABA==
	BCS	Lab_1AB9		; exit if required type is string

					; else do type mismatch error
==Lab_1ABC==
	LDX	#0x18			; error code 0x18 ("Type mismatch" error)
==Lab_1ABE==
	JMP	Lab_XERR		; do error #X, then warm start

; evaluate expression

==Lab_EVEX==
	LDX	bpntrl		; get BASIC execute pointer low byte
	BNE	Lab_1AC7		; skip next if not zero

	DEC	bpntrh		; else decrement BASIC execute pointer high byte
==Lab_1AC7==
	DEC	bpntrl		; decrement BASIC execute pointer low byte

==Lab_EVEZ==
	LDA	#0x00			; set null precedence (flag done)
==Lab_1ACC==
	PHA				; push precedence byte
	LDA	#0x02			; 2 bytes
	JSR	Lab_1212		; check room on stack for A bytes
	JSR	Lab_GVAL		; get value from line
	LDA	#0x00			; clear A
	STA	comp_f		; clear compare function flag
==Lab_1ADB==
	JSR	Lab_gbyt		; scan memory
==Lab_1ADE==
	SEC				; set carry for subtract
	SBC	#tk_GT		; subtract token for > (lowest comparison function)
	BCC	Lab_1AFA		; branch if < tk_GT

	CMP	#0x03			; compare with ">" to "<" tokens
	BCS	Lab_1AFA		; branch if >= tk_SGN (highest evaluation function +1)

					; was token for > = or < (A = 0, 1 or 2)
	CMP	#0x01			; compare with token for =
	ROL				; *2, b0 = carry (=1 if token was = or <)
					; (A = 0, 3 or 5)
	EOR	#0x01			; toggle b0
					; (A = 1, 2 or 4. 1 if >, 2 if =, 4 if <)
	EOR	comp_f		; EOR with compare function flag bits
	CMP	comp_f		; compare with compare function flag
	BCC	Lab_1B53		; if <(comp_f) do syntax error then warm start
					; was more than one <, = or >)

	STA	comp_f		; save new compare function flag
	JSR	0x00BC		; increment and scan memory
	JMP	Lab_1ADE		; go do next character

					; token is < ">" or > "<" tokens
==Lab_1AFA==
	LDX	comp_f		; get compare function flag
	BNE	Lab_1B2A		; branch if compare function

	BCS	Lab_1B78		; go do functions

					; else was <  tk_GT so is operator or lower
	ADC	#tk_GT-tk_PLUS	; add # of operators (+, -, *, /, ^, AND, OR or EOR)
	BCC	Lab_1B78		; branch if < + operator

					; carry was set so token was +, -, *, /, ^, AND, OR or EOR
	BNE	Lab_1B0B		; branch if not + token

	BIT	dtypef		; test data type flag, 0xFF=string, 0x00=numeric
	BPL	Lab_1B0B		; branch if not string

					; will only be 0x00 if type is string and token was +
	JMP	Lab_224D		; add strings, string 1 is in descriptor des_pl, string 2
					; is in line, and return

==Lab_1B0B==
	STA	ut1_pl		; save it
	ASL A				; *2
	ADC	ut1_pl		; *3
	TAY				; copy to index
==Lab_1B13==
	PLA				; pull previous precedence
	CMP	Lab_OPPT,Y		; compare with precedence byte
	BCS	Lab_1B7D		; branch if A >=

	JSR	Lab_CTNM		; check if source is numeric, else do type mismatch
==Lab_1B1C==
	PHA				; save precedence
==Lab_1B1D==
	JSR	Lab_1B43		; get vector, execute function then continue evaluation
	PLA				; restore precedence
	LDY	prstk			; get precedence stacked flag
	BPL	Lab_1B3C		; branch if stacked values

	TAX				; copy precedence (set flags)
	BEQ	Lab_1B9D		; exit if done

	BNE	Lab_1B86		; else pop FAC2 and return, branch always

==Lab_1B2A==
	ROL	dtypef		; shift data type flag into Cb
	TXA				; copy compare function flag
	STA	dtypef		; clear data type flag, X is 0xxx xxxx
	ROL				; shift data type into compare function byte b0
	LDX	bpntrl		; get BASIC execute pointer low byte
	BNE	Lab_1B34		; branch if no underflow

	DEC	bpntrh		; else decrement BASIC execute pointer high byte
==Lab_1B34==
	DEC	bpntrl		; decrement BASIC execute pointer low byte
tk_LT_PLUS	= tk_LT-tk_PLUS
	LDY	#tk_LT_PLUS*3	; set offset to last operator entry
	STA	comp_f		; save new compare function flag
	BNE	Lab_1B13		; branch always

==Lab_1B3C==
	CMP	Lab_OPPT,Y		;.compare with stacked function precedence
	BCS	Lab_1B86		; branch if A >=, pop FAC2 and return

	BCC	Lab_1B1C		; branch always

;.get vector, execute function then continue evaluation

==Lab_1B43==
	LDA	Lab_OPPT+2,Y	; get function vector high byte
	PHA				; onto stack
	LDA	Lab_OPPT+1,Y	; get function vector low byte
	PHA				; onto stack
					; now push sign, round FAC1 and put on stack
	JSR	Lab_1B5B		; function will return here, then the next RTS will call
					; the function
	LDA	comp_f		; get compare function flag
	PHA				; push compare evaluation byte
	LDA	Lab_OPPT,Y		; get precedence byte
	JMP	Lab_1ACC		; continue evaluating expression

==Lab_1B53==
	JMP	Lab_SNER		; do syntax error then warm start

; push sign, round FAC1 and put on stack

==Lab_1B5B==
	PLA				; get return addr low byte
	STA	ut1_pl		; save it
	INC	ut1_pl		; increment it (was ret-1 pushed? yes!)
					; note! no check is made on the high byte! if the calling
					; routine assembles to a page edge then this all goes
					; horribly wrong !!!
	PLA				; get return addr high byte
	STA	ut1_ph		; save it
	LDA	fac1_s		; get FAC1 sign (b7)
	PHA				; push sign

; round FAC1 and put on stack

==Lab_1B66==
	JSR	Lab_27BA		; round FAC1
	LDA	fac1_3		; get FAC1 mantissa3
	PHA				; push on stack
	LDA	fac1_2		; get FAC1 mantissa2
	PHA				; push on stack
	LDA	fac1_1		; get FAC1 mantissa1
	PHA				; push on stack
	LDA	fac1_e		; get FAC1 exponent
	PHA				; push on stack
	JMP	(ut1_pl)		; return, sort of

; do functions

==Lab_1B78==
	LDY	#0xFF			; flag function
	PLA				; pull precedence byte
==Lab_1B7B==
	BEQ	Lab_1B9D		; exit if done

==Lab_1B7D==
	CMP	#0x64			; compare previous precedence with 0x64
	BEQ	Lab_1B84		; branch if was 0x64 (< function)

	JSR	Lab_CTNM		; check if source is numeric, else do type mismatch
==Lab_1B84==
	STY	prstk			; save precedence stacked flag

					; pop FAC2 and return
==Lab_1B86==
	PLA				; pop byte
	LSR				; shift out comparison evaluation lowest bit
	STA	cflag			; save comparison evaluation flag
	PLA				; pop exponent
	STA	fac2_e		; save FAC2 exponent
	PLA				; pop mantissa1
	STA	fac2_1		; save FAC2 mantissa1
	PLA				; pop mantissa2
	STA	fac2_2		; save FAC2 mantissa2
	PLA				; pop mantissa3
	STA	fac2_3		; save FAC2 mantissa3
	PLA				; pop sign
	STA	fac2_s		; save FAC2 sign (b7)
	EOR	fac1_s		; EOR FAC1 sign (b7)
	STA	fac_sc		; save sign compare (FAC1 EOR FAC2)
==Lab_1B9D==
	LDA	fac1_e		; get FAC1 exponent
	RTS

; print "..." string to string util area

==Lab_1BC1==
	LDA	bpntrl		; get BASIC execute pointer low byte
	LDY	bpntrh		; get BASIC execute pointer high byte
	ADC	#0x00			; add carry to low byte
	BCC	Lab_1BCA		; branch if no overflow

	INY				; increment high byte
==Lab_1BCA==
	JSR	Lab_20AE		; print " terminated string to sutill/sutilh
	JMP	Lab_23F3		; restore BASIC execute pointer from temp and return

; get value from line

==Lab_GVAL==
	JSR	0x00BC		; increment and scan memory
	BCS	Lab_1BAC		; branch if not numeric character

					; else numeric string found (e.g. 123)
==Lab_1BA9==
	JMP	Lab_2887		; get FAC1 from string and return

; get value from line .. continued

					; wasn't a number so ..
==Lab_1BAC==
	TAX				; set the flags
	BMI	Lab_1BD0		; if -ve go test token values

					; else it is either a string, number, variable or (expr.LO>)
	CMP	#0x24			; compare with "0x"
	BEQ	Lab_1BA9		; branch if "0x", hex number

	CMP	#0x25			; else compare with "%"
	BEQ	Lab_1BA9		; branch if "%", binary number

	CMP	#0x2E			; compare with "."
	BEQ	Lab_1BA9		; if so get FAC1 from string and return (e.g. was .123)

					; it wasn't any sort of number so ..
	CMP	#0x22			; compare with "
	BEQ	Lab_1BC1		; branch if open quote

					; wasn't any sort of number so ..

; evaluate expression within parentheses

	CMP	#0x28			; compare with "("
	BNE	Lab_1C18		; if not "(" get (var), return value in FAC1 and 0x flag

==Lab_1BF7==
	JSR	Lab_EVEZ		; evaluate expression, no decrement

; all the 'scan for' routines return the character after the sought character

; scan for ")" , else do syntax error then warm start

==Lab_1BFB==
	LDA	#0x29			; load A with ")"

; scan for CHR0x(A) , else do syntax error then warm start

==Lab_SCCA==
	LDY	#0x00			; clear index
	CMP	(bpntrl),Y		; check next byte is = A
	BNE	Lab_SNER		; if not do syntax error then warm start

	JMP	lab_igby		; increment and scan memory then return

; scan for "(" , else do syntax error then warm start

==Lab_1BFE==
	LDA	#0x28			; load A with "("
	BNE	Lab_SCCA		; scan for CHR0x(A), else do syntax error then warm start
					; (branch always)

; scan for "," , else do syntax error then warm start

==Lab_1C01==
	LDA	#0x2C			; load A with ","
	BNE	Lab_SCCA		; scan for CHR0x(A), else do syntax error then warm start
					; (branch always)

; syntax error then warm start

==Lab_SNER==
	LDX	#0x02			; error code 0x02 ("Syntax" error)
	JMP	Lab_XERR		; do error #X, then warm start

; get value from line .. continued
; do tokens

==Lab_1BD0==
	CMP	#tk_MINUS		; compare with token for -
	BEQ	Lab_1C11		; branch if - token (do set-up for functions)

					; wasn't -n so ..
	CMP	#tk_PLUS		; compare with token for +
	BEQ	Lab_GVAL		; branch if + token (+n = n so ignore leading +)

	CMP	#tk_NOT		; compare with token for NOT
	BNE	Lab_1BE7		; branch if not token for NOT

					; was NOT token
tk_EQUAL_PLUS	= tk_EQUAL-tk_PLUS
	LDY	#tk_EQUAL_PLUS*3	; offset to NOT function
	BNE	Lab_1C13		; do set-up for function then execute (branch always)

; do = compare

==Lab_EQUAL==
	JSR	Lab_EVIR		; evaluate integer expression (no sign check)
	LDA	fac1_3		; get FAC1 mantissa3
	EOR	#0xFF			; invert it
	TAY				; copy it
	LDA	fac1_2		; get FAC1 mantissa2
	EOR	#0xFF			; invert it
	JMP	Lab_AYFC		; save and convert integer AY to FAC1 and return

; get value from line .. continued

					; wasn't +, -, or NOT so ..
==Lab_1BE7==
	CMP	#tk_FN		; compare with token for FN
	BNE	Lab_1BEE		; branch if not token for FN

	JMP	Lab_201E		; go evaluate FNx

; get value from line .. continued

					; wasn't +, -, NOT or FN so ..
==Lab_1BEE==
	SBC	#tk_SGN		; subtract with token for SGN
	BCS	Lab_1C27		; if a function token go do it

	JMP	Lab_SNER		; else do syntax error

; set-up for functions

==Lab_1C11==
tk_GT_PLUS	= tk_GT-tk_PLUS
	LDY	#tk_GT_PLUS*3	; set offset from base to > operator
==Lab_1C13==
	PLA				; dump return address low byte
	PLA				; dump return address high byte
	JMP	Lab_1B1D		; execute function then continue evaluation

; variable name set-up
; get (var), return value in FAC_1 and 0x flag

==Lab_1C18==
	JSR	Lab_GVAR		; get (var) address
	STA	fac1_2		; save address low byte in FAC1 mantissa2
	STY	fac1_3		; save address high byte in FAC1 mantissa3
	LDX	dtypef		; get data type flag, 0xFF=string, 0x00=numeric
	BMI	Lab_1C25		; if string then return (does RTS)

==Lab_1C24==
	JMP	Lab_UFAC		; unpack memory (AY) into FAC1

==Lab_1C25==
	RTS

; get value from line .. continued
; only functions left so ..

; set up function references

; new for V2.0+ this replaces a lot of IF .. THEN .. ELSEIF .. THEN .. that was needed
; to process function calls. now the function vector is computed and pushed on the stack
; and the preprocess offset is read. if the preprocess offset is non zero then the vector
; is calculated and the routine called, if not this routine just does RTS. whichever
; happens the RTS at the end of this routine, or the end of the preprocess routine, calls
; the function code

; this also removes some less than elegant code that was used to bypass type checking
; for functions that returned strings

==Lab_1C27==
	ASL A				; *2 (2 bytes per function address)
	TAY				; copy to index

	LDA	Lab_FTBM,Y		; get function jump vector high byte
	PHA				; push functions jump vector high byte
	LDA	Lab_FTBL,Y		; get function jump vector low byte
	PHA				; push functions jump vector low byte

	LDA	Lab_FTPM,Y		; get function pre process vector high byte
	BEQ	Lab_1C56		; skip pre process if null vector

	PHA				; push functions pre process vector high byte
	LDA	Lab_FTPL,Y		; get function pre process vector low byte
	PHA				; push functions pre process vector low byte

==Lab_1C56==
	RTS				; do function, or pre process, call

; process string expression in parenthesis

==Lab_PPFS==
	JSR	Lab_1BF7		; process expression in parenthesis
	JMP	Lab_CTST		; check if source is string then do function,
					; else do type mismatch

; process numeric expression in parenthesis

==Lab_PPFN==
	JSR	Lab_1BF7		; process expression in parenthesis
	JMP	Lab_CTNM		; check if source is numeric then do function,
					; else do type mismatch

; set numeric data type and increment BASIC execute pointer

==Lab_PPBI==
	LSR	dtypef		; clear data type flag, 0xFF=string, 0x00=numeric
	JMP	lab_igby		; increment and scan memory then do function

; process string for LEFT0x, RIGHT0x or MID0x

==Lab_LRMS==
	JSR	Lab_EVEZ		; evaluate (should be string) expression
	JSR	Lab_1C01		; scan for ",", else do syntax error then warm start
	JSR	Lab_CTST		; check if source is string, else do type mismatch

	PLA				; get function jump vector low byte
	TAX				; save functions jump vector low byte
	PLA				; get function jump vector high byte
	TAY				; save functions jump vector high byte
	LDA	des_ph		; get descriptor pointer high byte
	PHA				; push string pointer high byte
	LDA	des_pl		; get descriptor pointer low byte
	PHA				; push string pointer low byte
	TYA				; get function jump vector high byte back
	PHA				; save functions jump vector high byte
	TXA				; get function jump vector low byte back
	PHA				; save functions jump vector low byte
	JSR	Lab_GTBY		; get byte parameter
	TXA				; copy byte parameter to A
	RTS				; go do function

; process numeric expression(s) for BIN0x or HEX0x

==Lab_BHSS==
	JSR	Lab_EVEZ		; process expression
	JSR	Lab_CTNM		; check if source is numeric, else do type mismatch
	LDA	fac1_e		; get FAC1 exponent
	CMP	#0x98			; compare with exponent = 2^24
	BCS	Lab_BHER		; branch if n>=2^24 (is too big)

	JSR	Lab_2831		; convert FAC1 floating-to-fixed
	LDX	#0x02			; 3 bytes to do
==Lab_CFAC==
	LDA	fac1_1,X		; get byte from FAC1
	STA	nums_1,X		; save byte to temp
	DEX				; decrement index
	BPL	Lab_CFAC		; copy FAC1 mantissa to temp

	JSR	Lab_gbyt		; get next BASIC byte
	LDX	#0x00			; set default to no leading "0"s
	CMP	#0x28			; compare with close bracket
	BEQ	Lab_1C54		; if ")" go do rest of function

	JSR	Lab_SCGB		; scan for "," and get byte
	JSR	Lab_gbyt		; get last byte back
	CMP	#0x28			; is next character )
	BNE	Lab_BHER		; if not ")" go do error

==Lab_1C54==
	RTS				; else do function

==Lab_BHER==
	JMP	Lab_FCER		; do function call error then warm start

; perform EOR

; added operator format is the same as AND or OR, precedence is the same as OR

; this bit worked first time but it took a while to sort out the operator table
; pointers and offsets afterwards!

==Lab_EOR==
	JSR	GetFirst		; get first integer expression (no sign check)
	EOR	xoaw_l		; EOR with expression 1 low byte
	TAY				; save in Y
	LDA	fac1_2		; get FAC1 mantissa2
	EOR	xoaw_h		; EOR with expression 1 high byte
	JMP	Lab_AYFC		; save and convert integer AY to FAC1 and return

; perform OR

==Lab_OR==
	JSR	GetFirst		; get first integer expression (no sign check)
	ORA	xoaw_l		; OR with expression 1 low byte
	TAY				; save in Y
	LDA	fac1_2		; get FAC1 mantissa2
	ORA	xoaw_h		; OR with expression 1 high byte
	JMP	Lab_AYFC		; save and convert integer AY to FAC1 and return

; perform AND

==Lab_AND==
	JSR	GetFirst		; get first integer expression (no sign check)
	AND	xoaw_l		; AND with expression 1 low byte
	TAY				; save in Y
	LDA	fac1_2		; get FAC1 mantissa2
	AND	xoaw_h		; AND with expression 1 high byte
	JMP	Lab_AYFC		; save and convert integer AY to FAC1 and return

; get first value for OR, AND or EOR

==GetFirst==
	JSR	Lab_EVIR		; evaluate integer expression (no sign check)
	LDA	fac1_2		; get FAC1 mantissa2
	STA	xoaw_h		; save it
	LDA	fac1_3		; get FAC1 mantissa3
	STA	xoaw_l		; save it
	JSR	Lab_279B		; copy FAC2 to FAC1 (get 2nd value in expression)
	JSR	Lab_EVIR		; evaluate integer expression (no sign check)
	LDA	fac1_3		; get FAC1 mantissa3
==Lab_1C95==
	RTS

; perform comparisons

; do < compare

==Lab_LTHAN==
	JSR	Lab_CKTM		; type match check, set C for string
	BCS	Lab_1CAE		; branch if string

					; do numeric < compare
	LDA	fac2_s		; get FAC2 sign (b7)
	ORA	#0x7F			; set all non sign bits
	AND	fac2_1		; and FAC2 mantissa1 (AND in sign bit)
	STA	fac2_1		; save FAC2 mantissa1
	LDA	#fac2_e.LO		; set pointer low byte to FAC2
	LDY	#fac2_e.HI		; set pointer high byte to FAC2
	JSR	Lab_27F8		; compare FAC1 with FAC2 (AY)
	TAX				; copy result
	JMP	Lab_1CE1		; go evaluate result

					; do string < compare
==Lab_1CAE==
	LSR	dtypef		; clear data type flag, 0xFF=string, 0x00=numeric
	DEC	comp_f		; clear < bit in compare function flag
	JSR	Lab_22B6		; pop string off descriptor stack, or from top of string
					; space returns with A = length, X=pointer low byte,
					; Y=pointer high byte
	STA	str_ln		; save length
	STX	str_pl		; save string pointer low byte
	STY	str_ph		; save string pointer high byte
	LDA	fac2_2		; get descriptor pointer low byte
	LDY	fac2_3		; get descriptor pointer high byte
	JSR	Lab_22BA		; pop (YA) descriptor off stack or from top of string space
					; returns with A = length, X=pointer low byte,
					; Y=pointer high byte
	STX	fac2_2		; save string pointer low byte
	STY	fac2_3		; save string pointer high byte
	TAX				; copy length
	SEC				; set carry for subtract
	SBC	str_ln		; subtract string 1 length
	BEQ	Lab_1CD6		; branch if str 1 length = string 2 length

	LDA	#0x01			; set str 1 length > string 2 length
	BCC	Lab_1CD6		; branch if so

	LDX	str_ln		; get string 1 length
	LDA	#0xFF			; set str 1 length < string 2 length
==Lab_1CD6==
	STA	fac1_s		; save length compare
	LDY	#0xFF			; set index
	INX				; adjust for loop
==Lab_1CDB==
	INY				; increment index
	DEX				; decrement count
	BNE	Lab_1CE6		; branch if still bytes to do

	LDX	fac1_s		; get length compare back
==Lab_1CE1==
	BMI	Lab_1CF2		; branch if str 1 < str 2

	CLC				; flag str 1 <= str 2
	BCC	Lab_1CF2		; go evaluate result

==Lab_1CE6==
	LDA	(fac2_2),Y		; get string 2 byte
	CMP	(fac1_1),Y		; compare with string 1 byte
	BEQ	Lab_1CDB		; loop if bytes =

	LDX	#0xFF			; set str 1 < string 2
	BCS	Lab_1CF2		; branch if so

	LDX	#0x01			;  set str 1 > string 2
==Lab_1CF2==
	INX				; x = 0, 1 or 2
	TXA				; copy to A
	ROL				; *2 (1, 2 or 4)
	AND	cflag			; AND with comparison evaluation flag
	BEQ	Lab_1CFB		; branch if 0 (compare is false)

	LDA	#0xFF			; else set result true
==Lab_1CFB==
	JMP	Lab_27DB		; save A as integer byte and return

==Lab_1CFE==
	JSR	Lab_1C01		; scan for ",", else do syntax error then warm start

; perform DIM

==Lab_DIM==
	TAX				; copy "DIM" flag to X
	JSR	Lab_1D10		; search for variable
	JSR	Lab_gbyt		; scan memory
	BNE	Lab_1CFE		; scan for "," and loop if not null

	RTS

; perform << (left shift)

==Lab_LSHIFT==
	JSR	GetPair		; get integer expression and byte (no sign check)
	LDA	fac1_2		; get expression high byte
	LDX	tempb			; get shift count
	BEQ	NoShift		; branch if zero

	CPX	#0x10			; compare bit count with 16d
	BCS	TooBig		; branch if >=

==Ls_loop==
	ASL	fac1_3		; shift low byte
	ROL				; shift high byte
	DEX				; decrement bit count
	BNE	Ls_loop		; loop if shift not complete

	LDY	fac1_3		; get expression low byte
	JMP	Lab_AYFC		; save and convert integer AY to FAC1 and return

; perform >> (right shift)

==Lab_RSHIFT==
	JSR	GetPair		; get integer expression and byte (no sign check)
	LDA	fac1_2		; get expression high byte
	LDX	tempb			; get shift count
	BEQ	NoShift		; branch if zero

	CPX	#0x10			; compare bit count with 16d
	BCS	TooBig		; branch if >=

==Rs_loop==
	LSR				; shift high byte
	ROR	fac1_3		; shift low byte
	DEX				; decrement bit count
	BNE	Rs_loop		; loop if shift not complete

==NoShift==
	LDY	fac1_3		; get expression low byte
	JMP	Lab_AYFC		; save and convert integer AY to FAC1 and return

==TooBig==
	LDA	#0x00			; clear high byte
	TAY				; copy to low byte
	JMP	Lab_AYFC		; save and convert integer AY to FAC1 and return

==GetPair==
	JSR	Lab_EVBY		; evaluate byte expression, result in X
	STX	tempb			; save it
	JSR	Lab_279B		; copy FAC2 to FAC1 (get 2nd value in expression)
	JMP	Lab_EVIR		; evaluate integer expression (no sign check)

; search for variable

; return pointer to variable in cvaral/cvarah

==Lab_GVAR==
	LDX	#0x00			; set DIM flag = 0x00
	JSR	Lab_gbyt		; scan memory (1st character)
==Lab_1D10==
	STX	defdim		; save DIM flag
==Lab_1D12==
	STA	varnm1		; save 1st character
	AND	#0x7F			; clear FN flag bit
	JSR	Lab_CASC		; check byte, return C=0 if<"A" or >"Z"
	BCS	Lab_1D1F		; branch if ok

	JMP	Lab_SNER		; else syntax error then warm start

					; was variable name so ..
==Lab_1D1F==
	LDX	#0x00			; clear 2nd character temp
	STX	dtypef		; clear data type flag, 0xFF=string, 0x00=numeric
	JSR	0x00BC		; increment and scan memory (2nd character)
	BCC	Lab_1D2D		; branch if character = "0"-"9" (ok)

					; 2nd character wasn't "0" to "9" so ..
	JSR	Lab_CASC		; check byte, return C=0 if<"A" or >"Z"
	BCC	Lab_1D38		; branch if <"A" or >"Z" (go check if string)

==Lab_1D2D==
	TAX				; copy 2nd character

					; ignore further (valid) characters in the variable name
==Lab_1D2E==
	JSR	0x00BC		; increment and scan memory (3rd character)
	BCC	Lab_1D2E		; loop if character = "0"-"9" (ignore)

	JSR	Lab_CASC		; check byte, return C=0 if<"A" or >"Z"
	BCS	Lab_1D2E		; loop if character = "A"-"Z" (ignore)

					; check if string variable
==Lab_1D38==
	CMP	#24			; compare with "0x"
	BNE	Lab_1D47		; branch if not string

; to introduce a new variable type (% suffix for integers say) then this branch
; will need to go to that check and then that branch, if it fails, go to Lab_1D47

					; type is string
	LDA	#0xFF			; set data type = string
	STA	dtypef		; set data type flag, 0xFF=string, 0x00=numeric
	TXA				; get 2nd character back
	ORA	#0x80			; set top bit (indicate string var)
	TAX				; copy back to 2nd character temp
	JSR	0x00BC		; increment and scan memory

; after we have determined the variable type we need to come back here to determine
; if it's an array of type. this would plug in a%(b[,c[,d]])) integer arrays nicely


Lab_1D47				; gets here with character after var name in A
	STX	varnm2		; save 2nd character
	ORA	sufnxf		; or with subscript/FNX flag (or FN name)
	CMP	#0x28			; compare with "("
	BNE	Lab_1D53		; branch if not "("

	JMP	Lab_1E17		; go find, or make, array

; either find or create var
; var name (1st two characters only!) is in varnm1,varnm2

					; variable name wasn't var(... so look for plain var
==Lab_1D53==
	LDA	#0x00			; clear A
	STA	sufnxf		; clear subscript/FNX flag
	LDA	svarl			; get start of vars low byte
	LDX	svarh			; get start of vars high byte
	LDY	#0x00			; clear index
==Lab_1D5D==
	STX	vrschh		; save search address high byte
==Lab_1D5F==
	STA	vrschl		; save search address low byte
	CPX	sarryh		; compare high address with var space end
	BNE	Lab_1D69		; skip next compare if <>

					; high addresses were = so compare low addresses
	CMP	sarryl		; compare low address with var space end
	BEQ	Lab_1D8B		; if not found go make new var

==Lab_1D69==
	LDA	varnm1		; get 1st character of var to find
	CMP	(vrschl),Y		; compare with variable name 1st character
	BNE	Lab_1D77		; branch if no match

					; 1st characters match so compare 2nd characters
	LDA	varnm2		; get 2nd character of var to find
	INY				; index to point to variable name 2nd character
	CMP	(vrschl),Y		; compare with variable name 2nd character
	BEQ	Lab_1DD7		; branch if match (found var)

	DEY				; else decrement index (now = 0x00)
==Lab_1D77==
	CLC				; clear carry for add
	LDA	vrschl		; get search address low byte
	ADC	#0x06			; +6 (offset to next var name)
	BCC	Lab_1D5F		; loop if no overflow to high byte

	INX				; else increment high byte
	BNE	Lab_1D5D		; loop always (RAM doesn't extend to 0xFFFF !)

; check byte, return C=0 if<"A" or >"Z" or "a" to "z"

==Lab_CASC==
	CMP	#97			; compare with "a"
	BCS	Lab_1D83		; go check <"z"+1

; check byte, return C=0 if<"A" or >"Z"

==Lab_1D82==
	CMP	#65			; compare with "A"
	BCC	Lab_1D8A		; exit if less

					; carry is set
	SBC	#0x5B			; subtract "Z"+1
	SEC				; set carry
	SBC	#0xA5			; subtract 0xA5 (restore byte)
					; carry clear if byte0x5A.HI
==Lab_1D8A==
	RTS

==Lab_1D83==
	SBC	#0x7B			; subtract "z"+1
	SEC				; set carry
	SBC	#0x85			; subtract 0x85 (restore byte)
					; carry clear if byte0x7A.HI
	RTS

					; reached end of variable mem without match
					; .. so create new variable
==Lab_1D8B==
	PLA				; pop return address low byte
	PHA				; push return address low byte
Lab_1C18p2	= Lab_1C18+2
	CMP	#Lab_1C18p2.LO	; compare with expected calling routine return low byte
	BNE	Lab_1D98		; if not get (var) go create new var

; This will only drop through if the call was from Lab_1C18 and is only called
; from there if it is searching for a variable from the RHS of a LET a=b statement
; it prevents the creation of variables not assigned a value.

; value returned by this is either numeric zero (exponent byte is 0x00) or null string
; (descriptor length byte is 0x00). in fact a pointer to any 0x00 byte would have done.

; doing this saves 6 bytes of variable memory and 168 machine cycles of time

; this is where you would put the undefined variable error call e.g.

;					; variable doesn't exist so flag error
;	LDX	#0x24			; error code 0x24 ("undefined variable" error)
;	JMP	Lab_XERR		; do error #X then warm start

; the above code has been tested and works a treat! (it replaces the three code lines
; below)

					; else return dummy null value
	LDA	#Lab_1D96.LO		; low byte point to 0x00,0x00
					; (uses part of misc constants table)
	LDY	#Lab_1D96.HI		; high byte point to 0x00,0x00
	RTS

					; create new numeric variable
==Lab_1D98==
	LDA	sarryl		; get var mem end low byte
	LDY	sarryh		; get var mem end high byte
	STA	ostrtl		; save old block start low byte
	STY	ostrth		; save old block start high byte
	LDA	earryl		; get array mem end low byte
	LDY	earryh		; get array mem end high byte
	STA	obendl		; save old block end low byte
	STY	obendh		; save old block end high byte
	CLC				; clear carry for add
	ADC	#0x06			; +6 (space for one var)
	BCC	Lab_1DAE		; branch if no overflow to high byte

	INY				; else increment high byte
==Lab_1DAE==
	STA	nbendl		; set new block end low byte
	STY	nbendh		; set new block end high byte
	JSR	Lab_11CF		; open up space in memory
	LDA	nbendl		; get new start low byte
	LDY	nbendh		; get new start high byte (-0x100)
	INY				; correct high byte
	STA	sarryl		; save new var mem end low byte
	STY	sarryh		; save new var mem end high byte
	LDY	#0x00			; clear index
	LDA	varnm1		; get var name 1st character
	STA	(vrschl),Y		; save var name 1st character
	INY				; increment index
	LDA	varnm2		; get var name 2nd character
	STA	(vrschl),Y		; save var name 2nd character
	LDA	#0x00			; clear A
	INY				; increment index
	STA	(vrschl),Y		; initialise var byte
	INY				; increment index
	STA	(vrschl),Y		; initialise var byte
	INY				; increment index
	STA	(vrschl),Y		; initialise var byte
	INY				; increment index
	STA	(vrschl),Y		; initialise var byte

					; found a match for var ((vrschl) = ptr)
==Lab_1DD7==
	LDA	vrschl		; get var address low byte
	CLC				; clear carry for add
	ADC	#0x02			; +2 (offset past var name bytes)
	LDY	vrschh		; get var address high byte
	BCC	Lab_1DE1		; branch if no overflow from add

	INY				; else increment high byte
==Lab_1DE1==
	STA	cvaral		; save current var address low byte
	STY	cvarah		; save current var address high byte
	RTS

; set-up array pointer (adatal/h) to first element in array
; set adatal,adatah to astrtl,astrth+2*dimcnt+#0x05

==Lab_1DE6==
	LDA	dimcnt		; get # of dimensions (1, 2 or 3)
	ASL A				; *2 (also clears the carry !)
	ADC	#0x05			; +5 (result is 7, 9 or 11 here)
	ADC	astrtl		; add array start pointer low byte
	LDY	astrth		; get array pointer high byte
	BCC	Lab_1DF2		; branch if no overflow

	INY				; else increment high byte
==Lab_1DF2==
	STA	adatal		; save array data pointer low byte
	STY	adatah		; save array data pointer high byte
	RTS

; evaluate integer expression

==Lab_EVIN==
	JSR	0x00BC		; increment and scan memory
	JSR	Lab_EVNM		; evaluate expression and check is numeric,
					; else do type mismatch

; evaluate integer expression (no check)

==Lab_EVPI==
	LDA	fac1_s		; get FAC1 sign (b7)
	BMI	Lab_1E12		; do function call error if -ve

; evaluate integer expression (no sign check)

==Lab_EVIR==
	LDA	fac1_e		; get FAC1 exponent
	CMP	#0x90			; compare with exponent = 2^16 (n2.HI^15)
	BCC	Lab_1E14		; branch if n2.LO^16 (is ok)

	LDA	#Lab_1DF7.LO		; set pointer low byte to -32768
	LDY	#Lab_1DF7.HI		; set pointer high byte to -32768
	JSR	Lab_27F8		; compare FAC1 with (AY)
==Lab_1E12==
	BNE	Lab_FCER		; if <> do function call error then warm start

==Lab_1E14==
	JMP	Lab_2831		; convert FAC1 floating-to-fixed and return

; find or make array

==Lab_1E17==
	LDA	defdim		; get DIM flag
	PHA				; push it
	LDA	dtypef		; get data type flag, 0xFF=string, 0x00=numeric
	PHA				; push it
	LDY	#0x00			; clear dimensions count

; now get the array dimension(s) and stack it (them) before the data type and DIM flag

==Lab_1E1F==
	TYA				; copy dimensions count
	PHA				; save it
	LDA	varnm2		; get array name 2nd byte
	PHA				; save it
	LDA	varnm1		; get array name 1st byte
	PHA				; save it
	JSR	Lab_EVIN		; evaluate integer expression
	PLA				; pull array name 1st byte
	STA	varnm1		; restore array name 1st byte
	PLA				; pull array name 2nd byte
	STA	varnm2		; restore array name 2nd byte
	PLA				; pull dimensions count
	TAY				; restore it
	TSX				; copy stack pointer
	LDA	Lab_stak+2,X	; get DIM flag
	PHA				; push it
	LDA	Lab_stak+1,X	; get data type flag
	PHA				; push it
	LDA	fac1_2		; get this dimension size high byte
	STA	Lab_stak+2,X	; stack before flag bytes
	LDA	fac1_3		; get this dimension size low byte
	STA	Lab_stak+1,X	; stack before flag bytes
	INY				; increment dimensions count
	JSR	Lab_gbyt		; scan memory
	CMP	#0x3B			; compare with ","
	BEQ	Lab_1E1F		; if found go do next dimension

	STY	dimcnt		; store dimensions count
	JSR	Lab_1BFB		; scan for ")" , else do syntax error then warm start
	PLA				; pull data type flag
	STA	dtypef		; restore data type flag, 0xFF=string, 0x00=numeric
	PLA				; pull DIM flag
	STA	defdim		; restore DIM flag
	LDX	sarryl		; get array mem start low byte
	LDA	sarryh		; get array mem start high byte

; now check to see if we are at the end of array memory (we would be if there were
; no arrays).

==Lab_1E5C==
	STX	astrtl		; save as array start pointer low byte
	STA	astrth		; save as array start pointer high byte
	CMP	earryh		; compare with array mem end high byte
	BNE	Lab_1E68		; branch if not reached array mem end

	CPX	earryl		; else compare with array mem end low byte
	BEQ	Lab_1EA1		; go build array if not found

					; search for array
==Lab_1E68==
	LDY	#0x00			; clear index
	LDA	(astrtl),Y		; get array name first byte
	INY				; increment index to second name byte
	CMP	varnm1		; compare with this array name first byte
	BNE	Lab_1E77		; branch if no match

	LDA	varnm2		; else get this array name second byte
	CMP	(astrtl),Y		; compare with array name second byte
	BEQ	Lab_1E8D		; array found so branch

					; no match
==Lab_1E77==
	INY				; increment index
	LDA	(astrtl),Y		; get array size low byte
	CLC				; clear carry for add
	ADC	astrtl		; add array start pointer low byte
	TAX				; copy low byte to X
	INY				; increment index
	LDA	(astrtl),Y		; get array size high byte
	ADC	astrth		; add array mem pointer high byte
	BCC	Lab_1E5C		; if no overflow go check next array

; do array bounds error

==Lab_1E85==
	LDX	#0x10			; error code 0x10 ("Array bounds" error)
	DATA	0x2C			; makes next bit BIT Lab_08A2

; do function call error

==Lab_FCER==
	LDX	#0x08			; error code 0x08 ("Function call" error)
==Lab_1E8A==
	JMP	Lab_XERR		; do error #X, then warm start

					; found array, are we trying to dimension it?
==Lab_1E8D==
	LDX	#0x12			; set error 0x12 ("Double dimension" error)
	LDA	defdim		; get DIM flag
	BNE	Lab_1E8A		; if we are trying to dimension it do error #X, then warm
					; start

; found the array and we're not dimensioning it so we must find an element in it

	JSR	Lab_1DE6		; set-up array pointer (adatal/h) to first element in array
					; (astrtl,astrth points to start of array)
	LDA	dimcnt		; get dimensions count
	LDY	#0x04			; set index to array's # of dimensions
	CMP	(astrtl),Y		; compare with no of dimensions
	BNE	Lab_1E85		; if wrong do array bounds error, could do "Wrong
					; dimensions" error here .. if we want a different
					; error message

	JMP	Lab_1F28		; found array so go get element
					; (could jump to Lab_1F28 as all Lab_1F24 does is take
					; dimcnt and save it at (astrtl),Y which is already the
					; same or we would have taken the BNE)

					; array not found, so build it
==Lab_1EA1==
	JSR	Lab_1DE6		; set-up array pointer (adatal/h) to first element in array
					; (astrtl,astrth points to start of array)
	JSR	Lab_121F		; check available memory, "Out of memory" error if no room
					; addr to check is in AY (low/high)
	LDY	#0x00			; clear Y (don't need to clear A)
	STY	aspth			; clear array data size high byte
	LDA	varnm1		; get variable name 1st byte
	STA	(astrtl),Y		; save array name 1st byte
	INY				; increment index
	LDA	varnm2		; get variable name 2nd byte
	STA	(astrtl),Y		; save array name 2nd byte
	LDA	dimcnt		; get dimensions count
	LDY	#0x04			; index to dimension count
	STY	asptl			; set array data size low byte (four bytes per element)
	STA	(astrtl),Y		; set array's dimensions count

					; now calculate the size of the data space for the array
	CLC				; clear carry for add (clear on subsequent loops)
==Lab_1EC0==
	LDX	#0x0B			; set default dimension value low byte
	LDA	#0x00			; set default dimension value high byte
	BIT	defdim		; test default DIM flag
	BVC	Lab_1ED0		; branch if b6 of defdim is clear

	PLA				; else pull dimension value low byte
	ADC	#0x01			; +1 (allow for zeroeth element)
	TAX				; copy low byte to X
	PLA				; pull dimension value high byte
	ADC	#0x00			; add carry from low byte

==Lab_1ED0==
	INY				; index to dimension value high byte
	STA	(astrtl),Y		; save dimension value high byte
	INY				; index to dimension value high byte
	TXA				; get dimension value low byte
	STA	(astrtl),Y		; save dimension value low byte
	JSR	Lab_1F7C		; does XY = (astrtl),Y * (asptl)
	STX	asptl			; save array data size low byte
	STA	aspth			; save array data size high byte
	LDY	ut1_pl		; restore index (saved by subroutine)
	DEC	dimcnt		; decrement dimensions count
	BNE	Lab_1EC0		; loop while not = 0

	ADC	adatah		; add size high byte to first element high byte
					; (carry is always clear here)
	BCS	Lab_1F45		; if overflow go do "Out of memory" error

	STA	adatah		; save end of array high byte
	TAY				; copy end high byte to Y
	TXA				; get array size low byte
	ADC	adatal		; add array start low byte
	BCC	Lab_1EF3		; branch if no carry

	INY				; else increment end of array high byte
	BEQ	Lab_1F45		; if overflow go do "Out of memory" error

					; set-up mostly complete, now zero the array
==Lab_1EF3==
	JSR	Lab_121F		; check available memory, "Out of memory" error if no room
					; addr to check is in AY (low/high)
	STA	earryl		; save array mem end low byte
	STY	earryh		; save array mem end high byte
	LDA	#0x00			; clear byte for array clear
	INC	aspth			; increment array size high byte (now block count)
	LDY	asptl			; get array size low byte (now index to block)
	BEQ	Lab_1F07		; branch if low byte = 0x00

==Lab_1F02==
	DEY				; decrement index (do 0 to n-1)
	STA	(adatal),Y		; zero byte
	BNE	Lab_1F02		; loop until this block done

==Lab_1F07==
	DEC	adatah		; decrement array pointer high byte
	DEC	aspth			; decrement block count high byte
	BNE	Lab_1F02		; loop until all blocks done

	INC	adatah		; correct for last loop
	SEC				; set carry for subtract
	LDY	#0x02			; index to array size low byte
	LDA	earryl		; get array mem end low byte
	SBC	astrtl		; subtract array start low byte
	STA	(astrtl),Y		; save array size low byte
	INY				; index to array size high byte
	LDA	earryh		; get array mem end high byte
	SBC	astrth		; subtract array start high byte
	STA	(astrtl),Y		; save array size high byte
	LDA	defdim		; get default DIM flag
	BNE	Lab_1F7B		; exit (RET) if this was a DIM command

					; else, find element
	INY				; index to # of dimensions

==Lab_1F24==
	LDA	(astrtl),Y		; get array's dimension count
	STA	dimcnt		; save it

; we have found, or built, the array. now we need to find the element

==Lab_1F28==
	LDA	#0x00			; clear byte
	STA	asptl			; clear array data pointer low byte
==Lab_1F2C==
	STA	aspth			; save array data pointer high byte
	INY				; increment index (point to array bound high byte)
	PLA				; pull array index low byte
	TAX				; copy to X
	STA	fac1_2		; save index low byte to FAC1 mantissa2
	PLA				; pull array index high byte
	STA	fac1_3		; save index high byte to FAC1 mantissa3
	CMP	(astrtl),Y		; compare with array bound high byte
	BCC	Lab_1F48		; branch if within bounds

	BNE	Lab_1F42		; if outside bounds do array bounds error

					; else high byte was = so test low bytes
	INY				; index to array bound low byte
	TXA				; get array index low byte
	CMP	(astrtl),Y		; compare with array bound low byte
	BCC	Lab_1F49		; branch if within bounds

==Lab_1F42==
	JMP	Lab_1E85		; else do array bounds error

==Lab_1F45==
	JMP	Lab_OMER		; do "Out of memory" error then warm start

==Lab_1F48==
	INY				; index to array bound low byte
==Lab_1F49==
	LDA	aspth			; get array data pointer high byte
	ORA	asptl			; OR with array data pointer low byte
	BEQ	Lab_1F5A		; branch if array data pointer = null (skip multiply)

	JSR	Lab_1F7C		; does XY = (astrtl),Y * (asptl)
	TXA				; get result low byte
	ADC	fac1_2		; add index low byte from FAC1 mantissa2
	TAX				; save result low byte
	TYA				; get result high byte
	LDY	ut1_pl		; restore index
==Lab_1F5A==
	ADC	fac1_3		; add index high byte from FAC1 mantissa3
	STX	asptl			; save array data pointer low byte
	DEC	dimcnt		; decrement dimensions count
	BNE	Lab_1F2C		; loop if dimensions still to do

	ASL	asptl			; array data pointer low byte * 2
	ROL				; array data pointer high byte * 2
	ASL	asptl			; array data pointer low byte * 4
	ROL				; array data pointer high byte * 4
	TAY				; copy high byte
	LDA	asptl			; get low byte
	ADC	adatal		; add array data start pointer low byte
	STA	cvaral		; save as current var address low byte
	TYA				; get high byte back
	ADC	adatah		; add array data start pointer high byte
	STA	cvarah		; save as current var address high byte
	TAY				; copy high byte to Y
	LDA	cvaral		; get current var address low byte
==Lab_1F7B==
	RTS

; does XY = (astrtl),Y * (asptl)

==Lab_1F7C==
	STY	ut1_pl		; save index
	LDA	(astrtl),Y		; get dimension size low byte
	STA	dims_l		; save dimension size low byte
	DEY				; decrement index
	LDA	(astrtl),Y		; get dimension size high byte
	STA	dims_h		; save dimension size high byte

	LDA	#0x10			; count = 0x10 (16 bit multiply)
	STA	numbit		; save bit count
	LDX	#0x00			; clear result low byte
	LDY	#0x00			; clear result high byte
==Lab_1F8F==
	TXA				; get result low byte
	ASL A				; *2
	TAX				; save result low byte
	TYA				; get result high byte
	ROL				; *2
	TAY				; save result high byte
	BCS	Lab_1F45		; if overflow go do "Out of memory" error

	ASL	asptl			; shift multiplier low byte
	ROL	aspth			; shift multiplier high byte
	BCC	Lab_1FA8		; skip add if no carry

	CLC				; else clear carry for add
	TXA				; get result low byte
	ADC	dims_l		; add dimension size low byte
	TAX				; save result low byte
	TYA				; get result high byte
	ADC	dims_h		; add dimension size high byte
	TAY				; save result high byte
	BCS	Lab_1F45		; if overflow go do "Out of memory" error

==Lab_1FA8==
	DEC	numbit		; decrement bit count
	BNE	Lab_1F8F		; loop until all done

	RTS

; perform FRE()

==Lab_FRE==
	LDA	dtypef		; get data type flag, 0xFF=string, 0x00=numeric
	BPL	Lab_1FB4		; branch if numeric

	JSR	Lab_22B6		; pop string off descriptor stack, or from top of string
					; space returns with A = length, X=0x71=pointer low byte,
					; Y=0x72=pointer high byte

					; FRE(n) was numeric so do this
==Lab_1FB4==
	JSR	Lab_GARB		; go do garbage collection
	SEC				; set carry for subtract
	LDA	sstorl		; get bottom of string space low byte
	SBC	earryl		; subtract array mem end low byte
	TAY				; copy result to Y
	LDA	sstorh		; get bottom of string space high byte
	SBC	earryh		; subtract array mem end high byte

; save and convert integer AY to FAC1

==Lab_AYFC==
	LSR	dtypef		; clear data type flag, 0xFF=string, 0x00=numeric
	STA	fac1_1		; save FAC1 mantissa1
	STY	fac1_2		; save FAC1 mantissa2
	LDX	#0x90			; set exponent=2^16 (integer)
	JMP	Lab_27E3		; set exp=X, clear fac1_3, normalise and return

; perform POS()

==Lab_POS==
	LDY	tPos			; get terminal position

; convert Y to byte in FAC1

==Lab_1FD0==
	LDA	#0x00			; clear high byte
	BEQ	Lab_AYFC		; always save and convert integer AY to FAC1 and return

; check not Direct (used by DEF and INPUT)

==Lab_CKRN==
	LDX	clineh		; get current line high byte
	INX				; increment it
	BNE	Lab_1F7B		; return if can continue not direct mode

					; else do illegal direct error
==Lab_1FD9==
	LDX	#0x16			; error code 0x16 ("Illegal direct" error)
==Lab_1FDB==
	JMP	Lab_XERR		; go do error #X, then warm start

; perform DEF

==Lab_DEF==
	JSR	Lab_200B		; check FNx syntax
	STA	func_l		; save function pointer low byte
	STY	func_h		; save function pointer high byte
	JSR	Lab_CKRN		; check not Direct (back here if ok)
	JSR	Lab_1BFE		; scan for "(" , else do syntax error then warm start
	LDA	#0x80			; set flag for FNx
	STA	sufnxf		; save subscript/FNx flag
	JSR	Lab_GVAR		; get (var) address
	JSR	Lab_CTNM		; check if source is numeric, else do type mismatch
	JSR	Lab_1BFB		; scan for ")" , else do syntax error then warm start
	LDA	#tk_EQUAL		; get = token
	JSR	Lab_SCCA		; scan for CHR0x(A), else do syntax error then warm start
	LDA	cvarah		; get current var address high byte
	PHA				; push it
	LDA	cvaral		; get current var address low byte
	PHA				; push it
	LDA	bpntrh		; get BASIC execute pointer high byte
	PHA				; push it
	LDA	bpntrl		; get BASIC execute pointer low byte
	PHA				; push it
	JSR	Lab_DATA		; go perform DATA
	JMP	Lab_207A		; put execute pointer and variable pointer into function
					; and return

; check FNx syntax

==Lab_200B==
	LDA	#tk_FN		; get FN" token
	JSR	Lab_SCCA		; scan for CHR0x(A) , else do syntax error then warm start
					; return character after A
	ORA	#0x80			; set FN flag bit
	STA	sufnxf		; save FN flag so array variable test fails
	JSR	Lab_1D12		; search for FN variable
	JMP	Lab_CTNM		; check if source is numeric and return, else do type
					; mismatch

					; Evaluate FNx
==Lab_201E==
	JSR	Lab_200B		; check FNx syntax
	PHA				; push function pointer low byte
	TYA				; copy function pointer high byte
	PHA				; push function pointer high byte
	JSR	Lab_1BFE		; scan for "(", else do syntax error then warm start
	JSR	Lab_EVEX		; evaluate expression
	JSR	Lab_1BFB		; scan for ")", else do syntax error then warm start
	JSR	Lab_CTNM		; check if source is numeric, else do type mismatch
	PLA				; pop function pointer high byte
	STA	func_h		; restore it
	PLA				; pop function pointer low byte
	STA	func_l		; restore it
	LDX	#0x20			; error code 0x20 ("Undefined function" error)
	LDY	#0x03			; index to variable pointer high byte
	LDA	(func_l),Y		; get variable pointer high byte
	BEQ	Lab_1FDB		; if zero go do undefined function error

	STA	cvarah		; save variable address high byte
	DEY				; index to variable address low byte
	LDA	(func_l),Y		; get variable address low byte
	STA	cvaral		; save variable address low byte
	TAX				; copy address low byte

					; now stack the function variable value before use
	INY				; index to mantissa_3
==Lab_2043==
	LDA	(cvaral),Y		; get byte from variable
	PHA				; stack it
	DEY				; decrement index
	BPL	Lab_2043		; loop until variable stacked

	LDY	cvarah		; get variable address high byte
	JSR	Lab_2778		; pack FAC1 (function expression value) into (XY)
					; (function variable), return Y=0, always
	LDA	bpntrh		; get BASIC execute pointer high byte
	PHA				; push it
	LDA	bpntrl		; get BASIC execute pointer low byte
	PHA				; push it
	LDA	(func_l),Y		; get function execute pointer low byte
	STA	bpntrl		; save as BASIC execute pointer low byte
	INY				; index to high byte
	LDA	(func_l),Y		; get function execute pointer high byte
	STA	bpntrh		; save as BASIC execute pointer high byte
	LDA	cvarah		; get variable address high byte
	PHA				; push it
	LDA	cvaral		; get variable address low byte
	PHA				; push it
	JSR	Lab_EVNM		; evaluate expression and check is numeric,
					; else do type mismatch
	PLA				; pull variable address low byte
	STA	func_l		; save variable address low byte
	PLA				; pull variable address high byte
	STA	func_h		; save variable address high byte
	JSR	Lab_gbyt		; scan memory
	BEQ	Lab_2074		; branch if null (should be [EOL] marker)

	JMP	Lab_SNER		; else syntax error then warm start

; restore bpntrl,bpntrh and function variable from stack

==Lab_2074==
	PLA				; pull BASIC execute pointer low byte
	STA	bpntrl		; restore BASIC execute pointer low byte
	PLA				; pull BASIC execute pointer high byte
	STA	bpntrh		; restore BASIC execute pointer high byte

; put execute pointer and variable pointer into function

==Lab_207A==
	LDY	#0x00			; clear index
	PLA				; pull BASIC execute pointer low byte
	STA	(func_l),Y		; save to function
	INY				; increment index
	PLA				; pull BASIC execute pointer high byte
	STA	(func_l),Y		; save to function
	INY				; increment index
	PLA				; pull current var address low byte
	STA	(func_l),Y		; save to function
	INY				; increment index
	PLA				; pull current var address high byte
	STA	(func_l),Y		; save to function
	RTS

; perform STR0x()

==Lab_STRS==
	JSR	Lab_CTNM		; check if source is numeric, else do type mismatch
	JSR	Lab_296E		; convert FAC1 to string
	LDA	#decssp1.LO		; set result string low pointer
	LDY	#decssp1.HI		; set result string high pointer
	BEQ	Lab_20AE		; print null terminated string to sutill/sutilh

; Do string vector
; copy des_pl/h to des_2l/h and make string space A bytes long

==Lab_209C==
	LDX	des_pl		; get descriptor pointer low byte
	LDY	des_ph		; get descriptor pointer high byte
	STX	des_2l		; save descriptor pointer low byte
	STY	des_2h		; save descriptor pointer high byte

; make string space A bytes long
; A=length, X=sutill=ptr low byte, Y=sutilh=ptr high byte

==Lab_MSSP==
	JSR	Lab_2115		; make space in string memory for string A long
					; return X=sutill=ptr low byte, Y=sutilh=ptr high byte
	STX	str_pl		; save string pointer low byte
	STY	str_ph		; save string pointer high byte
	STA	str_ln		; save length
	RTS

; Scan, set up string
; print " terminated string to sutill/sutilh

==Lab_20AE==
	LDX	#0x22			; set terminator to "
	STX	srchc			; set search character (terminator 1)
	STX	asrch			; set terminator 2

; print [srchc] or [asrch] terminated string to sutill/sutilh
; source is AY

==Lab_20B4==
	STA	ssptr_l		; store string start low byte
	STY	ssptr_h		; store string start high byte
	STA	str_pl		; save string pointer low byte
	STY	str_ph		; save string pointer high byte
	LDY	#0xFF			; set length to -1
==Lab_20BE==
	INY				; increment length
	LDA	(ssptr_l),Y		; get byte from string
	BEQ	Lab_20CF		; exit loop if null byte [EOS]

	CMP	srchc			; compare with search character (terminator 1)
	BEQ	Lab_20CB		; branch if terminator

	CMP	asrch			; compare with terminator 2
	BNE	Lab_20BE		; loop if not terminator 2

==Lab_20CB==
	CMP	#0x22			; compare with "
	BEQ	Lab_20D0		; branch if " (carry set if = !)

==Lab_20CF==
	CLC				; clear carry for add (only if [EOL] terminated string)
==Lab_20D0==
	STY	str_ln		; save length in FAC1 exponent
	TYA				; copy length to A
	ADC	ssptr_l		; add string start low byte
	STA	sendl			; save string end low byte
	LDX	ssptr_h		; get string start high byte
	BCC	Lab_20DC		; branch if no low byte overflow

	INX				; else increment high byte
==Lab_20DC==
	STX	sendh			; save string end high byte
	LDA	ssptr_h		; get string start high byte
	CMP	#Ram_base.HI		; compare with start of program memory
	BCS	Lab_RTST		; branch if not in utility area

					; string in utility area, move to string memory
	TYA				; copy length to A
	JSR	Lab_209C		; copy des_pl/h to des_2l/h and make string space A bytes
					; long
	LDX	ssptr_l		; get string start low byte
	LDY	ssptr_h		; get string start high byte
	JSR	Lab_2298		; store string A bytes long from XY to (sutill)

; check for space on descriptor stack then ..
; put string address and length on descriptor stack and update stack pointers

==Lab_RTST==
	LDX	next_s		; get string stack pointer
	CPX	#des_sk+0x09		; compare with max+1
	BNE	Lab_20F8		; branch if space on string stack

					; else do string too complex error
	LDX	#0x1C			; error code 0x1C ("String too complex" error)
==Lab_20F5==
	JMP	Lab_XERR		; do error #X, then warm start

; put string address and length on descriptor stack and update stack pointers

==Lab_20F8==
	LDA	str_ln		; get string length
	STA	plus_0,X		; put on string stack
	LDA	str_pl		; get string pointer low byte
	STA	plus_1,X		; put on string stack
	LDA	str_ph		; get string pointer high byte
	STA	plus_2,X		; put on string stack
	LDY	#0x00			; clear Y
	STX	des_pl		; save string descriptor pointer low byte
	STY	des_ph		; save string descriptor pointer high byte (always 0x00)
	DEY				; Y = 0xFF
	STY	dtypef		; save data type flag, 0xFF=string
	STX	last_sl		; save old stack pointer (current top item)
	INX				; update stack pointer
	INX				; update stack pointer
	INX				; update stack pointer
	STX	next_s		; save new top item value
	RTS

; Build descriptor
; make space in string memory for string A long
; return X=sutill=ptr low byte, Y=sutill=ptr high byte

==Lab_2115==
	LSR	gclctd		; clear garbage collected flag (b7)

					; make space for string A long
==Lab_2117==
	PHA				; save string length
	EOR	#0xFF			; complement it
	SEC				; set carry for subtract (twos comp add)
	ADC	sstorl		; add bottom of string space low byte (subtract length)
	LDY	sstorh		; get bottom of string space high byte
	BCS	Lab_2122		; skip decrement if no underflow

	DEY				; decrement bottom of string space high byte
==Lab_2122==
	CPY	earryh		; compare with array mem end high byte
	BCC	Lab_2137		; do out of memory error if less

	BNE	Lab_212C		; if not = skip next test

	CMP	earryl		; compare with array mem end low byte
	BCC	Lab_2137		; do out of memory error if less

==Lab_212C==
	STA	sstorl		; save bottom of string space low byte
	STY	sstorh		; save bottom of string space high byte
	STA	sutill		; save string utility ptr low byte
	STY	sutilh		; save string utility ptr high byte
	TAX				; copy low byte to X
	PLA				; get string length back
	RTS

==Lab_2137==
	LDX	#0x0C			; error code 0x0C ("Out of memory" error)
	LDA	gclctd		; get garbage collected flag
	BMI	Lab_20F5		; if set then do error code X

	JSR	Lab_GARB		; else go do garbage collection
	LDA	#0x80			; flag for garbage collected
	STA	gclctd		; set garbage collected flag
	PLA				; pull length
	BNE	Lab_2117		; go try again (loop always, length should never be = 0x00)

; garbage collection routine

==Lab_GARB==
	LDX	ememl			; get end of mem low byte
	LDA	ememh			; get end of mem high byte

; re-run routine from last ending

==Lab_214B==
	STX	sstorl		; set string storage low byte
	STA	sstorh		; set string storage high byte
	LDY	#0x00			; clear index
	STY	garb_h		; clear working pointer high byte (flag no strings to move)
	LDA	earryl		; get array mem end low byte
	LDX	earryh		; get array mem end high byte
	STA	histrl		; save as highest string low byte
	STX	histrh		; save as highest string high byte
	LDA	#des_sk		; set descriptor stack pointer
	STA	ut1_pl		; save descriptor stack pointer low byte
	STY	ut1_ph		; save descriptor stack pointer high byte (0x00)
==Lab_2161==
	CMP	next_s		; compare with descriptor stack pointer
	BEQ	Lab_216A		; branch if =

	JSR	Lab_21D7		; go garbage collect descriptor stack
	BEQ	Lab_2161		; loop always

					; done stacked strings, now do string vars
==Lab_216A==
	ASL	g_step		; set step size = 0x06
	LDA	svarl			; get start of vars low byte
	LDX	svarh			; get start of vars high byte
	STA	ut1_pl		; save as pointer low byte
	STX	ut1_ph		; save as pointer high byte
==Lab_2176==
	CPX	sarryh		; compare start of arrays high byte
	BNE	Lab_217E		; branch if no high byte match

	CMP	sarryl		; else compare start of arrays low byte
	BEQ	Lab_2183		; branch if = var mem end

==Lab_217E==
	JSR	Lab_21D1		; go garbage collect strings
	BEQ	Lab_2176		; loop always

					; done string vars, now do string arrays
==Lab_2183==
	STA	nbendl		; save start of arrays low byte as working pointer
	STX	nbendh		; save start of arrays high byte as working pointer
	LDA	#0x04			; set step size
	STA	g_step		; save step size
==Lab_218B==
	LDA	nbendl		; get pointer low byte
	LDX	nbendh		; get pointer high byte
==Lab_218F==
	CPX	earryh		; compare with array mem end high byte
	BNE	Lab_219A		; branch if not at end

	CMP	earryl		; else compare with array mem end low byte
	BEQ	Lab_2216		; tidy up and exit if at end

==Lab_219A==
	STA	ut1_pl		; save pointer low byte
	STX	ut1_ph		; save pointer high byte
	LDY	#0x02			; set index
	LDA	(ut1_pl),Y		; get array size low byte
	ADC	nbendl		; add start of this array low byte
	STA	nbendl		; save start of next array low byte
	INY				; increment index
	LDA	(ut1_pl),Y		; get array size high byte
	ADC	nbendh		; add start of this array high byte
	STA	nbendh		; save start of next array high byte
	LDY	#0x01			; set index
	LDA	(ut1_pl),Y		; get name second byte
	BPL	Lab_218B		; skip if not string array

; was string array so ..

	LDY	#0x04			; set index
	LDA	(ut1_pl),Y		; get # of dimensions
	ASL A				; *2
	ADC	#0x05			; +5 (array header size)
	JSR	Lab_2208		; go set up for first element
==Lab_21C4==
	CPX	nbendh		; compare with start of next array high byte
	BNE	Lab_21CC		; branch if <> (go do this array)

	CMP	nbendl		; else compare element pointer low byte with next array
					; low byte
	BEQ	Lab_218F		; if equal then go do next array

==Lab_21CC==
	JSR	Lab_21D7		; go defrag array strings
	BEQ	Lab_21C4		; go do next array string (loop always)

; defrag string variables
; enter with XA = variable pointer
; return with XA = next variable pointer

==Lab_21D1==
	INY				; increment index (Y was 0x00)
	LDA	(ut1_pl),Y		; get var name byte 2
	BPL	Lab_2206		; if not string, step pointer to next var and return

	INY				; else increment index
==Lab_21D7==
	LDA	(ut1_pl),Y		; get string length
	BEQ	Lab_2206		; if null, step pointer to next string and return

	INY				; else increment index
	LDA	(ut1_pl),Y		; get string pointer low byte
	TAX				; copy to X
	INY				; increment index
	LDA	(ut1_pl),Y		; get string pointer high byte
	CMP	sstorh		; compare bottom of string space high byte
	BCC	Lab_21EC		; branch if less

	BNE	Lab_2206		; if greater, step pointer to next string and return

					; high bytes were = so compare low bytes
	CPX	sstorl		; compare bottom of string space low byte
	BCS	Lab_2206		; if >=, step pointer to next string and return

					; string pointer is < string storage pointer (pos in mem)
==Lab_21EC==
	CMP	histrh		; compare to highest string high byte
	BCC	Lab_2207		; if <, step pointer to next string and return

	BNE	Lab_21F6		; if > update pointers, step to next and return

					; high bytes were = so compare low bytes
	CPX	histrl		; compare to highest string low byte
	BCC	Lab_2207		; if <, step pointer to next string and return

					; string is in string memory space
==Lab_21F6==
	STX	histrl		; save as new highest string low byte
	STA	histrh		; save as new highest string high byte
	LDA	ut1_pl		; get start of vars(descriptors) low byte
	LDX	ut1_ph		; get start of vars(descriptors) high byte
	STA	garb_l		; save as working pointer low byte
	STX	garb_h		; save as working pointer high byte
	DEY				; decrement index DIFFERS
	DEY				; decrement index (should point to descriptor start)
	STY	g_indx		; save index pointer

					; step pointer to next string
==Lab_2206==
	CLC				; clear carry for add
==Lab_2207==
	LDA	g_step		; get step size
==Lab_2208==
	ADC	ut1_pl		; add pointer low byte
	STA	ut1_pl		; save pointer low byte
	BCC	Lab_2211		; branch if no overflow

	INC	ut1_ph		; else increment high byte
==Lab_2211==
	LDX	ut1_ph		; get pointer high byte
	LDY	#0x00			; clear Y
	RTS

; search complete, now either exit or set-up and move string

==Lab_2216==
	DEC	g_step		; decrement step size (now 0x03 for descriptor stack)
	LDX	garb_h		; get string to move high byte
	BEQ	Lab_2211		; exit if nothing to move

	LDY	g_indx		; get index byte back (points to descriptor)
	CLC				; clear carry for add
	LDA	(garb_l),Y		; get string length
	ADC	histrl		; add highest string low byte
	STA	obendl		; save old block end low pointer
	LDA	histrh		; get highest string high byte
	ADC	#0x00			; add any carry
	STA	obendh		; save old block end high byte
	LDA	sstorl		; get bottom of string space low byte
	LDX	sstorh		; get bottom of string space high byte
	STA	nbendl		; save new block end low byte
	STX	nbendh		; save new block end high byte
	JSR	Lab_11D6		; open up space in memory, don't set array end
	LDY	g_indx		; get index byte
	INY				; point to descriptor low byte
	LDA	nbendl		; get string pointer low byte
	STA	(garb_l),Y		; save new string pointer low byte
	TAX				; copy string pointer low byte
	INC	nbendh		; correct high byte (move sets high byte -1)
	LDA	nbendh		; get new string pointer high byte
	INY				; point to descriptor high byte
	STA	(garb_l),Y		; save new string pointer high byte
	JMP	Lab_214B		; re-run routine from last ending
					; (but don't collect this string)

; concatenate
; add strings, string 1 is in descriptor des_pl, string 2 is in line

==Lab_224D==
	LDA	des_ph		; get descriptor pointer high byte
	PHA				; put on stack
	LDA	des_pl		; get descriptor pointer low byte
	PHA				; put on stack
	JSR	Lab_GVAL		; get value from line
	JSR	Lab_CTST		; check if source is string, else do type mismatch
	PLA				; get descriptor pointer low byte back
	STA	ssptr_l		; set pointer low byte
	PLA				; get descriptor pointer high byte back
	STA	ssptr_h		; set pointer high byte
	LDY	#0x00			; clear index
	LDA	(ssptr_l),Y		; get length_1 from descriptor
	CLC				; clear carry for add
	ADC	(des_pl),Y		; add length_2
	BCC	Lab_226D		; branch if no overflow

	LDX	#0x1A			; else set error code 0x1A ("String too long" error)
	JMP	Lab_XERR		; do error #X, then warm start

==Lab_226D==
	JSR	Lab_209C		; copy des_pl/h to des_2l/h and make string space A bytes
					; long
	JSR	Lab_228A		; copy string from descriptor (sdescr) to (sutill)
	LDA	des_2l		; get descriptor pointer low byte
	LDY	des_2h		; get descriptor pointer high byte
	JSR	Lab_22BA		; pop (YA) descriptor off stack or from top of string space
					; returns with A = length, ut1_pl = pointer low byte,
					; ut1_ph = pointer high byte
	JSR	Lab_229C		; store string A bytes long from (ut1_pl) to (sutill)
	LDA	ssptr_l		;.set descriptor pointer low byte
	LDY	ssptr_h		;.set descriptor pointer high byte
	JSR	Lab_22BA		; pop (YA) descriptor off stack or from top of string space
					; returns with A = length, X=ut1_pl=pointer low byte,
					; Y=ut1_ph=pointer high byte
	JSR	Lab_RTST		; check for space on descriptor stack then put string
					; address and length on descriptor stack and update stack
					; pointers
	JMP	Lab_1ADB		;.continue evaluation

; copy string from descriptor (sdescr) to (sutill)

==Lab_228A==
	LDY	#0x00			; clear index
	LDA	(sdescr),Y		; get string length
	PHA				; save on stack
	INY				; increment index
	LDA	(sdescr),Y		; get source string pointer low byte
	TAX				; copy to X
	INY				; increment index
	LDA	(sdescr),Y		; get source string pointer high byte
	TAY				; copy to Y
	PLA				; get length back

; store string A bytes long from YX to (sutill)

==Lab_2298==
	STX	ut1_pl		; save source string pointer low byte
	STY	ut1_ph		; save source string pointer high byte

; store string A bytes long from (ut1_pl) to (sutill)

==Lab_229C==
	TAX				; copy length to index (don't count with Y)
	BEQ	Lab_22B2		; branch if = 0x0 (null string) no need to add zero length

	LDY	#0x00			; zero pointer (copy forward)
==Lab_22A0==
	LDA	(ut1_pl),Y		; get source byte
	STA	(sutill),Y		; save destination byte

	INY				; increment index
	DEX				; decrement counter
	BNE	Lab_22A0		; loop while <> 0

	TYA				; restore length from Y
==Lab_22A9==
	CLC				; clear carry for add
	ADC	sutill		; add string utility ptr low byte
	STA	sutill		; save string utility ptr low byte
	BCC	Lab_22B2		; branch if no carry

	INC	sutilh		; else increment string utility ptr high byte
==Lab_22B2==
	RTS

; evaluate string

==Lab_EVST==
	JSR	Lab_CTST		; check if source is string, else do type mismatch

; pop string off descriptor stack, or from top of string space
; returns with A = length, X=pointer low byte, Y=pointer high byte

==Lab_22B6==
	LDA	des_pl		; get descriptor pointer low byte
	LDY	des_ph		; get descriptor pointer high byte

; pop (YA) descriptor off stack or from top of string space
; returns with A = length, X=ut1_pl=pointer low byte, Y=ut1_ph=pointer high byte

==Lab_22BA==
	STA	ut1_pl		; save descriptor pointer low byte
	STY	ut1_ph		; save descriptor pointer high byte
	JSR	Lab_22EB		; clean descriptor stack, YA = pointer
	PHP				; save status flags
	LDY	#0x00			; clear index
	LDA	(ut1_pl),Y		; get length from string descriptor
	PHA				; put on stack
	INY				; increment index
	LDA	(ut1_pl),Y		; get string pointer low byte from descriptor
	TAX				; copy to X
	INY				; increment index
	LDA	(ut1_pl),Y		; get string pointer high byte from descriptor
	TAY				; copy to Y
	PLA				; get string length back
	PLP				; restore status
	BNE	Lab_22E6		; branch if pointer <> last_sl,last_sh

	CPY	sstorh		; compare bottom of string space high byte
	BNE	Lab_22E6		; branch if <>

	CPX	sstorl		; else compare bottom of string space low byte
	BNE	Lab_22E6		; branch if <>

	PHA				; save string length
	CLC				; clear carry for add
	ADC	sstorl		; add bottom of string space low byte
	STA	sstorl		; save bottom of string space low byte
	BCC	Lab_22E5		; skip increment if no overflow

	INC	sstorh		; increment bottom of string space high byte
==Lab_22E5==
	PLA				; restore string length
==Lab_22E6==
	STX	ut1_pl		; save string pointer low byte
	STY	ut1_ph		; save string pointer high byte
	RTS

; clean descriptor stack, YA = pointer
; checks if AY is on the descriptor stack, if so does a stack discard

==Lab_22EB==
	CPY	last_sh		; compare pointer high byte
	BNE	Lab_22FB		; exit if <>

	CMP	last_sl		; compare pointer low byte
	BNE	Lab_22FB		; exit if <>

	STA	next_s		; save descriptor stack pointer
	SBC	#0x03			; -3
	STA	last_sl		; save low byte -3
	LDY	#0x00			; clear high byte
==Lab_22FB==
	RTS

; perform CHR0x()

==Lab_CHRS==
	JSR	Lab_EVBY		; evaluate byte expression, result in X
	TXA				; copy to A
	PHA				; save character
	LDA	#0x01			; string is single byte
	JSR	Lab_MSSP		; make string space A bytes long A=0xAC=length,
					; X=0xAD=sutill=ptr low byte, Y=0xAE=sutilh=ptr high byte
	PLA				; get character back
	LDY	#0x00			; clear index
	STA	(str_pl),Y		; save byte in string (byte IS string!)
	JMP	Lab_RTST		; check for space on descriptor stack then put string
					; address and length on descriptor stack and update stack
					; pointers

; perform LEFT0x()

==Lab_LEFT==
	PHA				; push byte parameter
	JSR	Lab_236F		; pull string data and byte parameter from stack
					; return pointer in des_2l/h, byte in A (and X), Y=0
	CMP	(des_2l),Y		; compare byte parameter with string length
	TYA				; clear A
	BEQ	Lab_2316		; go do string copy (branch always)

; perform RIGHT0x()

==Lab_RIGHT==
	PHA				; push byte parameter
	JSR	Lab_236F		; pull string data and byte parameter from stack
					; return pointer in des_2l/h, byte in A (and X), Y=0
	CLC				; clear carry for add-1
	SBC	(des_2l),Y		; subtract string length
	EOR	#0xFF			; invert it (A=LEN(expression0x)-l)

==Lab_2316==
	BCC	Lab_231C		; branch if string length > byte parameter

	LDA	(des_2l),Y		; else make parameter = length
	TAX				; copy to byte parameter copy
	TYA				; clear string start offset
==Lab_231C==
	PHA				; save string start offset
==Lab_231D==
	TXA				; copy byte parameter (or string length if <)
==Lab_231E==
	PHA				; save string length
	JSR	Lab_MSSP		; make string space A bytes long A=0xAC=length,
					; X=0xAD=sutill=ptr low byte, Y=0xAE=sutilh=ptr high byte
	LDA	des_2l		; get descriptor pointer low byte
	LDY	des_2h		; get descriptor pointer high byte
	JSR	Lab_22BA		; pop (YA) descriptor off stack or from top of string space
					; returns with A = length, X=ut1_pl=pointer low byte,
					; Y=ut1_ph=pointer high byte
	PLA				; get string length back
	TAY				; copy length to Y
	PLA				; get string start offset back
	CLC				; clear carry for add
	ADC	ut1_pl		; add start offset to string start pointer low byte
	STA	ut1_pl		; save string start pointer low byte
	BCC	Lab_2335		; branch if no overflow

	INC	ut1_ph		; else increment string start pointer high byte
==Lab_2335==
	TYA				; copy length to A
	JSR	Lab_229C		; store string A bytes long from (ut1_pl) to (sutill)
	JMP	Lab_RTST		; check for space on descriptor stack then put string
					; address and length on descriptor stack and update stack
					; pointers

; perform MID0x()

==Lab_MIDS==
	PHA				; push byte parameter
	LDA	#0xFF			; set default length = 255
	STA	mids_l		; save default length
	JSR	Lab_gbyt		; scan memory
	CMP	#0x28			; compare with ")"
	BEQ	Lab_2358		; branch if = ")" (skip second byte get)

	JSR	Lab_1C01		; scan for "," , else do syntax error then warm start
	JSR	Lab_GTBY		; get byte parameter (use copy in mids_l)
==Lab_2358==
	JSR	Lab_236F		; pull string data and byte parameter from stack
					; return pointer in des_2l/h, byte in A (and X), Y=0
	DEX				; decrement start index
	TXA				; copy to A
	PHA				; save string start offset
	CLC				; clear carry for sub-1
	LDX	#0x00			; clear output string length
	SBC	(des_2l),Y		; subtract string length
	BCS	Lab_231D		; if startstring.HI length go do null string

	EOR	#0xFF			; complement -length
	CMP	mids_l		; compare byte parameter
	BCC	Lab_231E		; if lengthremaining.HI string go do RIGHT0x

	LDA	mids_l		; get length byte
	BCS	Lab_231E		; go do string copy (branch always)

; pull string data and byte parameter from stack
; return pointer in des_2l/h, byte in A (and X), Y=0

==Lab_236F==
	JSR	Lab_1BFB		; scan for ")" , else do syntax error then warm start
	PLA				; pull return address low byte (return address)
	STA	fnxjpl		; save functions jump vector low byte
	PLA				; pull return address high byte (return address)
	STA	fnxjph		; save functions jump vector high byte
	PLA				; pull byte parameter
	TAX				; copy byte parameter to X
	PLA				; pull string pointer low byte
	STA	des_2l		; save it
	PLA				; pull string pointer high byte
	STA	des_2h		; save it
	LDY	#0x00			; clear index
	TXA				; copy byte parameter
	BEQ	Lab_23A8		; if null do function call error then warm start

	INC	fnxjpl		; increment function jump vector low byte
					; (JSR pushes return addr-1. this is all very nice
					; but will go tits up if either call is on a page
					; boundary!)
	JMP	(fnxjpl)		; in effect, RTS

; perform LCASE0x()

==Lab_LCASE==
	JSR	Lab_EVST		; evaluate string
	STA	str_ln		; set string length
	TAY				; copy length to Y
	BEQ	NoString		; branch if null string

	JSR	Lab_MSSP		; make string space A bytes long A=length,
					; X=sutill=ptr low byte, Y=sutilh=ptr high byte
	STX	str_pl		; save string pointer low byte
	STY	str_ph		; save string pointer high byte
	TAY				; get string length back

==LC_loop==
	DEY				; decrement index
	LDA	(ut1_pl),Y		; get byte from string
	JSR	Lab_1D82		; is character "A" to "Z"
	BCC	NoUcase		; branch if not upper case alpha

	ORA	#0x20			; convert upper to lower case
==NoUcase==
	STA	(sutill),Y		; save byte back to string
	TYA				; test index
	BNE	LC_loop		; loop if not all done

	BEQ	NoString		; tidy up and exit, branch always

; perform UCASE0x()

==Lab_UCASE==
	JSR	Lab_EVST		; evaluate string
	STA	str_ln		; set string length
	TAY				; copy length to Y
	BEQ	NoString		; branch if null string

	JSR	Lab_MSSP		; make string space A bytes long A=length,
					; X=sutill=ptr low byte, Y=sutilh=ptr high byte
	STX	str_pl		; save string pointer low byte
	STY	str_ph		; save string pointer high byte
	TAY				; get string length back

==UC_loop==
	DEY				; decrement index
	LDA	(ut1_pl),Y		; get byte from string
	JSR	Lab_CASC		; is character "a" to "z" (or "A" to "Z")
	BCC	NoLcase		; branch if not alpha

	AND	#0xDF			; convert lower to upper case
==NoLcase==
	STA	(sutill),Y		; save byte back to string
	TYA				; test index
	BNE	UC_loop		; loop if not all done

==NoString==
	JMP	Lab_RTST		; check for space on descriptor stack then put string
					; address and length on descriptor stack and update stack
					; pointers

; perform SADD()

==Lab_SADD==
	JSR	0x00BC		; increment and scan memory
	JSR	Lab_GVAR		; get var address

	JSR	Lab_1BFB		; scan for ")", else do syntax error then warm start
	JSR	Lab_CTST		; check if source is string, else do type mismatch

	LDY	#0x02			; index to string pointer high byte
	LDA	(cvaral),Y		; get string pointer high byte
	TAX				; copy string pointer high byte to X
	DEY				; index to string pointer low byte
	LDA	(cvaral),Y		; get string pointer low byte
	TAY				; copy string pointer low byte to Y
	TXA				; copy string pointer high byte to A
	JMP	Lab_AYFC		; save and convert integer AY to FAC1 and return

; perform LEN()

==Lab_LENS==
	JSR	Lab_ESGL		; evaluate string, get length in A (and Y)
	JMP	Lab_1FD0		; convert Y to byte in FAC1 and return

; evaluate string, get length in Y

==Lab_ESGL==
	JSR	Lab_EVST		; evaluate string
	TAY				; copy length to Y
	RTS

; perform ASC()

==Lab_ASC==
	JSR	Lab_ESGL		; evaluate string, get length in A (and Y)
	BEQ	Lab_23A8		; if null do function call error then warm start

	LDY	#0x00			; set index to first character
	LDA	(ut1_pl),Y		; get byte
	TAY				; copy to Y
	JMP	Lab_1FD0		; convert Y to byte in FAC1 and return

; do function call error then warm start

==Lab_23A8==
	JMP	Lab_FCER		; do function call error then warm start

; scan and get byte parameter

==Lab_SGBY==
	JSR	0x00BC		; increment and scan memory

; get byte parameter

==Lab_GTBY==
	JSR	Lab_EVNM		; evaluate expression and check is numeric,
					; else do type mismatch

; evaluate byte expression, result in X

==Lab_EVBY==
	JSR	Lab_EVPI		; evaluate integer expression (no check)

	LDY	fac1_2		; get FAC1 mantissa2
	BNE	Lab_23A8		; if top byte <> 0 do function call error then warm start

	LDX	fac1_3		; get FAC1 mantissa3
	JMP	Lab_gbyt		; scan memory and return

; perform VAL()

==Lab_VAL==
	JSR	Lab_ESGL		; evaluate string, get length in A (and Y)
	BNE	Lab_23C5		; branch if not null string

					; string was null so set result = 0x00
	JMP	Lab_24F1		; clear FAC1 exponent and sign and return

==Lab_23C5==
	LDX	bpntrl		; get BASIC execute pointer low byte
	LDY	bpntrh		; get BASIC execute pointer high byte
	STX	btmpl			; save BASIC execute pointer low byte
	STY	btmph			; save BASIC execute pointer high byte
	LDX	ut1_pl		; get string pointer low byte
	STX	bpntrl		; save as BASIC execute pointer low byte
	CLC				; clear carry
	ADC	ut1_pl		; add string length
	STA	ut2_pl		; save string end low byte
	LDA	ut1_ph		; get string pointer high byte
	STA	bpntrh		; save as BASIC execute pointer high byte
	ADC	#0x00			; add carry to high byte
	STA	ut2_ph		; save string end high byte
	LDY	#0x00			; set index to 0x00
	LDA	(ut2_pl),Y		; get string end +1 byte
	PHA				; push it
	TYA				; clear A
	STA	(ut2_pl),Y		; terminate string with 0x00
	JSR	Lab_gbyt		; scan memory
	JSR	Lab_2887		; get FAC1 from string
	PLA				; restore string end +1 byte
	LDY	#0x00			; set index to zero
	STA	(ut2_pl),Y		; put string end byte back

; restore BASIC execute pointer from temp (btmpl/btmph)

==Lab_23F3==
	LDX	btmpl			; get BASIC execute pointer low byte back
	LDY	btmph			; get BASIC execute pointer high byte back
	STX	bpntrl		; save BASIC execute pointer low byte
	STY	bpntrh		; save BASIC execute pointer high byte
	RTS

; get two parameters for POKE or WAIT

==Lab_GADB==
	JSR	Lab_EVNM		; evaluate expression and check is numeric,
					; else do type mismatch
	JSR	Lab_F2FX		; save integer part of FAC1 in temporary integer

; scan for "," and get byte, else do Syntax error then warm start

==Lab_SCGB==
	JSR	Lab_1C01		; scan for "," , else do syntax error then warm start
	LDA	itemph		; save temporary integer high byte
	PHA				; on stack
	LDA	itempl		; save temporary integer low byte
	PHA				; on stack
	JSR	Lab_GTBY		; get byte parameter
	PLA				; pull low byte
	STA	itempl		; restore temporary integer low byte
	PLA				; pull high byte
	STA	itemph		; restore temporary integer high byte
	RTS

; convert float to fixed routine. accepts any value that fits in 24 bits, +ve or
; -ve and converts it into a right truncated integer in itempl and itemph

; save unsigned 16 bit integer part of FAC1 in temporary integer

==Lab_F2FX==
	LDA	fac1_e		; get FAC1 exponent
	CMP	#0x98			; compare with exponent = 2^24
	BCS	Lab_23A8		; if >= do function call error then warm start

==Lab_F2FU==
	JSR	Lab_2831		; convert FAC1 floating-to-fixed
	LDA	fac1_2		; get FAC1 mantissa2
	LDY	fac1_3		; get FAC1 mantissa3
	STY	itempl		; save temporary integer low byte
	STA	itemph		; save temporary integer high byte
	RTS

; perform PEEK()

==Lab_PEEK==
	JSR	Lab_F2FX		; save integer part of FAC1 in temporary integer
	LDX	#0x00			; clear index
	LDA	(itempl,X)		; get byte via temporary integer (addr)
	TAY				; copy byte to Y
	JMP	Lab_1FD0		; convert Y to byte in FAC1 and return

; perform POKE

==Lab_POKE==
	JSR	Lab_GADB		; get two parameters for POKE or WAIT
	TXA				; copy byte argument to A
	LDX	#0x00			; clear index
	STA	(itempl,X)		; save byte via temporary integer (addr)
	RTS

; perform DEEK()

==Lab_DEEK==
	JSR	Lab_F2FX		; save integer part of FAC1 in temporary integer
	LDX	#0x00			; clear index
	LDA	(itempl,X)		; PEEK low byte
	TAY				; copy to Y
	INC	itempl		; increment pointer low byte
	BNE	Deekh			; skip high increment if no rollover

	INC	itemph		; increment pointer high byte
==Deekh==
	LDA	(itempl,X)		; PEEK high byte
	JMP	Lab_AYFC		; save and convert integer AY to FAC1 and return

; perform DOKE

==Lab_DOKE==
	JSR	Lab_EVNM		; evaluate expression and check is numeric,
					; else do type mismatch
	JSR	Lab_F2FX		; convert floating-to-fixed

	STY	frnxtl		; save pointer low byte (float to fixed returns word in AY)
	STA	frnxth		; save pointer high byte

	JSR	Lab_1C01		; scan for "," , else do syntax error then warm start
	JSR	Lab_EVNM		; evaluate expression and check is numeric,
					; else do type mismatch
	JSR	Lab_F2FX		; convert floating-to-fixed

	TYA				; copy value low byte (float to fixed returns word in AY)
	LDX	#0x00			; clear index
	STA	(frnxtl,X)		; POKE low byte
	INC	frnxtl		; increment pointer low byte
	BNE	Dokeh			; skip high increment if no rollover

	INC	frnxth		; increment pointer high byte
==Dokeh==
	LDA	itemph		; get value high byte
	STA	(frnxtl,X)		; POKE high byte
	JMP	Lab_gbyt		; scan memory and return

; perform SWAP

==Lab_SWAP==
	JSR	Lab_GVAR		; get var1 address
	STA	lvarpl		; save var1 address low byte
	STY	lvarph		; save var1 address high byte
	LDA	dtypef		; get data type flag, 0xFF=string, 0x00=numeric
	PHA				; save data type flag

	JSR	Lab_1C01		; scan for "," , else do syntax error then warm start
	JSR	Lab_GVAR		; get var2 address (pointer in cvaral/h)
	PLA				; pull var1 data type flag
	EOR	dtypef		; compare with var2 data type
	BPL	SwapErr		; exit if not both the same type

	LDY	#0x03			; four bytes to swap (either value or descriptor+1)
==SwapLp==
	LDA	(lvarpl),Y		; get byte from var1
	TAX				; save var1 byte
	LDA	(cvaral),Y		; get byte from var2
	STA	(lvarpl),Y		; save byte to var1
	TXA				; restore var1 byte
	STA	(cvaral),Y		; save byte to var2
	DEY				; decrement index
	BPL	SwapLp		; loop until done

	RTS

==SwapErr==
	JMP	Lab_1ABC		; do "Type mismatch" error then warm start

; perform CALL

==Lab_CALL==
	JSR	Lab_EVNM		; evaluate expression and check is numeric,
					; else do type mismatch
	JSR	Lab_F2FX		; convert floating-to-fixed
	LDA	#CallExit.HI		; set return address high byte
	PHA				; put on stack
	LDA	#CallExit.LO-1	; set return address low byte
	PHA				; put on stack
	JMP	(itempl)		; do indirect jump to user routine

; if the called routine exits correctly then it will return to here. this will then get
; the next byte for the interpreter and return

==CallExit==
	JMP	Lab_gbyt		; scan memory and return

; perform WAIT

==Lab_WAIT==
	JSR	Lab_GADB		; get two parameters for POKE or WAIT
	STX	frnxtl		; save byte
	LDX	#0x00			; clear mask
	JSR	Lab_gbyt		; scan memory
	BEQ	Lab_2441		; skip if no third argument

	JSR	Lab_SCGB		; scan for "," and get byte, else SN error then warm start
==Lab_2441==
	STX	frnxth		; save EOR argument
==Lab_2445==
	LDA	(itempl),Y		; get byte via temporary integer (addr)
	EOR	frnxth		; EOR with second argument (mask)
	AND	frnxtl		; AND with first argument (byte)
	BEQ	Lab_2445		; loop if result is zero

==Lab_244D==
	RTS

; perform subtraction, FAC1 from (AY)

==Lab_2455==
	JSR	Lab_264D		; unpack memory (AY) into FAC2

; perform subtraction, FAC1 from FAC2

==Lab_SUBTRACT==
	LDA	fac1_s		; get FAC1 sign (b7)
	EOR	#0xFF			; complement it
	STA	fac1_s		; save FAC1 sign (b7)
	EOR	fac2_s		; EOR with FAC2 sign (b7)
	STA	fac_sc		; save sign compare (FAC1 EOR FAC2)
	LDA	fac1_e		; get FAC1 exponent
	JMP	Lab_ADD		; go add FAC2 to FAC1

; perform addition

==Lab_2467==
	JSR	Lab_257B		; shift FACX A times right (8.HI shifts)
	BCC	Lab_24A8		;.go subtract mantissas

; add 0.5 to FAC1

==Lab_244E==
	LDA	#Lab_2A96.LO		; set 0.5 pointer low byte
	LDY	#Lab_2A96.HI		; set 0.5 pointer high byte

; add (AY) to FAC1

==Lab_246C==
	JSR	Lab_264D		; unpack memory (AY) into FAC2

; add FAC2 to FAC1

==Lab_ADD==
	BNE	Lab_2474		; branch if FAC1 was not zero

; copy FAC2 to FAC1

==Lab_279B==
	LDA	fac2_s		; get FAC2 sign (b7)

; save FAC1 sign and copy ABS(FAC2) to FAC1

==Lab_279D==
	STA	fac1_s		; save FAC1 sign (b7)
	LDX	#0x04			; 4 bytes to copy
==Lab_27A1==
	LDA	fac1_o,X		; get byte from FAC2,X
	STA	fac1_e-1,X		; save byte at FAC1,X
	DEX				; decrement count
	BNE	Lab_27A1		; loop if not all done

	STX	fac1_r		; clear FAC1 rounding byte
	RTS

					; FAC1 is non zero
==Lab_2474==
	LDX	fac1_r		; get FAC1 rounding byte
	STX	fac2_r		; save as FAC2 rounding byte
	LDX	#fac2_e		; set index to FAC2 exponent addr
	LDA	fac2_e		; get FAC2 exponent
==Lab_247C==
	TAY				; copy exponent
	BEQ	Lab_244D		; exit if zero

	SEC				; set carry for subtract
	SBC	fac1_e		; subtract FAC1 exponent
	BEQ	Lab_24A8		; branch if = (go add mantissa)

	BCC	Lab_2498		; branch if <

					; FAC2FAC1.HI
	STY	fac1_e		; save FAC1 exponent
	LDY	fac2_s		; get FAC2 sign (b7)
	STY	fac1_s		; save FAC1 sign (b7)
	EOR	#0xFF			; complement A
	ADC	#0x00			; +1 (twos complement, carry is set)
	LDY	#0x00			; clear Y
	STY	fac2_r		; clear FAC2 rounding byte
	LDX	#fac1_e		; set index to FAC1 exponent addr
	BNE	Lab_249C		; branch always

==Lab_2498==
	LDY	#0x00			; clear Y
	STY	fac1_r		; clear FAC1 rounding byte
==Lab_249C==
	CMP	#0xF9			; compare exponent diff with 0xF9
	BMI	Lab_2467		; branch if range 0x79-0xF8

	TAY				; copy exponent difference to Y
	LDA	fac1_r		; get FAC1 rounding byte
	LSR	plus_1,X		; shift FAC? mantissa1
	JSR	Lab_2592		; shift FACX Y times right

					; exponents are equal now do mantissa subtract
==Lab_24A8==
	BIT	fac_sc		; test sign compare (FAC1 EOR FAC2)
	BPL	Lab_24F8		; if = add FAC2 mantissa to FAC1 mantissa and return

	LDY	#fac1_e		; set index to FAC1 exponent addr
	CPX	#fac2_e		; compare X to FAC2 exponent addr
	BEQ	Lab_24B4		; branch if =

	LDY	#fac2_e		; else set index to FAC2 exponent addr

					; subtract smaller from bigger (take sign of bigger)
==Lab_24B4==
	SEC				; set carry for subtract
	EOR	#0xFF			; ones complement A
	ADC	fac2_r		; add FAC2 rounding byte
	STA	fac1_r		; save FAC1 rounding byte
	LDA	plus_3,Y		; get FACY mantissa3
	SBC	plus_3,X		; subtract FACX mantissa3
	STA	fac1_3		; save FAC1 mantissa3
	LDA	plus_2,Y		; get FACY mantissa2
	SBC	plus_2,X		; subtract FACX mantissa2
	STA	fac1_2		; save FAC1 mantissa2
	LDA	plus_1,Y		; get FACY mantissa1
	SBC	plus_1,X		; subtract FACX mantissa1
	STA	fac1_1		; save FAC1 mantissa1

; do ABS and normalise FAC1

==Lab_24D0==
	BCS	Lab_24D5		; branch if number is +ve

	JSR	Lab_2537		; negate FAC1

; normalise FAC1

==Lab_24D5==
	LDY	#0x00			; clear Y
	TYA				; clear A
	CLC				; clear carry for add
==Lab_24D9==
	LDX	fac1_1		; get FAC1 mantissa1
	BNE	Lab_251B		; if not zero normalise FAC1

	LDX	fac1_2		; get FAC1 mantissa2
	STX	fac1_1		; save FAC1 mantissa1
	LDX	fac1_3		; get FAC1 mantissa3
	STX	fac1_2		; save FAC1 mantissa2
	LDX	fac1_r		; get FAC1 rounding byte
	STX	fac1_3		; save FAC1 mantissa3
	STY	fac1_r		; clear FAC1 rounding byte
	ADC	#0x08			; add x to exponent offset
	CMP	#0x18			; compare with 0x18 (max offset, all bits would be =0)
	BNE	Lab_24D9		; loop if not max

; clear FAC1 exponent and sign

==Lab_24F1==
	LDA	#0x00			; clear A
==Lab_24F3==
	STA	fac1_e		; set FAC1 exponent

; save FAC1 sign

==Lab_24F5==
	STA	fac1_s		; save FAC1 sign (b7)
	RTS

; add FAC2 mantissa to FAC1 mantissa

==Lab_24F8==
	ADC	fac2_r		; add FAC2 rounding byte
	STA	fac1_r		; save FAC1 rounding byte
	LDA	fac1_3		; get FAC1 mantissa3
	ADC	fac2_3		; add FAC2 mantissa3
	STA	fac1_3		; save FAC1 mantissa3
	LDA	fac1_2		; get FAC1 mantissa2
	ADC	fac2_2		; add FAC2 mantissa2
	STA	fac1_2		; save FAC1 mantissa2
	LDA	fac1_1		; get FAC1 mantissa1
	ADC	fac2_1		; add FAC2 mantissa1
	STA	fac1_1		; save FAC1 mantissa1
	BCS	Lab_252A		; if carry then normalise FAC1 for C=1

	RTS				; else just exit

==Lab_2511==
	ADC	#0x01			; add 1 to exponent offset
	ASL	fac1_r		; shift FAC1 rounding byte
	ROL	fac1_3		; shift FAC1 mantissa3
	ROL	fac1_2		; shift FAC1 mantissa2
	ROL	fac1_1		; shift FAC1 mantissa1

; normalise FAC1

==Lab_251B==
	BPL	Lab_2511		; loop if not normalised

	SEC				; set carry for subtract
	SBC	fac1_e		; subtract FAC1 exponent
	BCS	Lab_24F1		; branch if underflow (set result = 0x0)

	EOR	#0xFF			; complement exponent
	ADC	#0x01			; +1 (twos complement)
	STA	fac1_e		; save FAC1 exponent

; test and normalise FAC1 for C=0/1

==Lab_2528==
	BCC	Lab_2536		; exit if no overflow

; normalise FAC1 for C=1

==Lab_252A==
	INC	fac1_e		; increment FAC1 exponent
	BEQ	Lab_2564		; if zero do overflow error and warm start

	ROR	fac1_1		; shift FAC1 mantissa1
	ROR	fac1_2		; shift FAC1 mantissa2
	ROR	fac1_3		; shift FAC1 mantissa3
	ROR	fac1_r		; shift FAC1 rounding byte
==Lab_2536==
	RTS

; negate FAC1

==Lab_2537==
	LDA	fac1_s		; get FAC1 sign (b7)
	EOR	#0xFF			; complement it
	STA	fac1_s		; save FAC1 sign (b7)

; twos complement FAC1 mantissa

==Lab_253D==
	LDA	fac1_1		; get FAC1 mantissa1
	EOR	#0xFF			; complement it
	STA	fac1_1		; save FAC1 mantissa1
	LDA	fac1_2		; get FAC1 mantissa2
	EOR	#0xFF			; complement it
	STA	fac1_2		; save FAC1 mantissa2
	LDA	fac1_3		; get FAC1 mantissa3
	EOR	#0xFF			; complement it
	STA	fac1_3		; save FAC1 mantissa3
	LDA	fac1_r		; get FAC1 rounding byte
	EOR	#0xFF			; complement it
	STA	fac1_r		; save FAC1 rounding byte
	INC	fac1_r		; increment FAC1 rounding byte
	BNE	Lab_2563		; exit if no overflow

; increment FAC1 mantissa

==Lab_2559==
	INC	fac1_3		; increment FAC1 mantissa3
	BNE	Lab_2563		; finished if no rollover

	INC	fac1_2		; increment FAC1 mantissa2
	BNE	Lab_2563		; finished if no rollover

	INC	fac1_1		; increment FAC1 mantissa1
==Lab_2563==
	RTS

; do overflow error (overflow exit)

==Lab_2564==
	LDX	#0x0A			; error code 0x0A ("Overflow" error)
	JMP	Lab_XERR		; do error #X, then warm start

; shift FCAtemp << A+8 times

==Lab_2569==
	LDX	#fact_1-1		; set offset to FACtemp
==Lab_256B==
	LDY	plus_3,X		; get FACX mantissa3
	STY	fac1_r		; save as FAC1 rounding byte
	LDY	plus_2,X		; get FACX mantissa2
	STY	plus_3,X		; save FACX mantissa3
	LDY	plus_1,X		; get FACX mantissa1
	STY	plus_2,X		; save FACX mantissa2
	LDY	fac1_o		; get FAC1 overflow byte
	STY	plus_1,X		; save FACX mantissa1

; shift FACX -A times right (> 8 shifts)

==Lab_257B==
	ADC	#0x08			; add 8 to shift count
	BMI	Lab_256B		; go do 8 shift if still -ve

	BEQ	Lab_256B		; go do 8 shift if zero

	SBC	#0x08			; else subtract 8 again
	TAY				; save count to Y
	LDA	fac1_r		; get FAC1 rounding byte
	BCS	Lab_259A		;.

==Lab_2588==
	ASL	plus_1,X		; shift FACX mantissa1
	BCC	Lab_258E		; branch if +ve

	INC	plus_1,X		; this sets b7 eventually
==Lab_258E==
	ROR	plus_1,X		; shift FACX mantissa1 (correct for ASL)
	ROR	plus_1,X		; shift FACX mantissa1 (put carry in b7)

; shift FACX Y times right

==Lab_2592==
	ROR	plus_2,X		; shift FACX mantissa2
	ROR	plus_3,X		; shift FACX mantissa3
	ROR				; shift FACX rounding byte
	INY				; increment exponent diff
	BNE	Lab_2588		; branch if range adjust not complete

==Lab_259A==
	CLC				; just clear it
	RTS

; perform LOG()

==Lab_LOG==
	JSR	Lab_27CA		; test sign and zero
	BEQ	Lab_25C4		; if zero do function call error then warm start

	BPL	Lab_25C7		; skip error if +ve

==Lab_25C4==
	JMP	Lab_FCER		; do function call error then warm start (-ve)

==Lab_25C7==
	LDA	fac1_e		; get FAC1 exponent
	SBC	#0x7F			; normalise it
	PHA				; save it
	LDA	#0x80			; set exponent to zero
	STA	fac1_e		; save FAC1 exponent
	LDA	#Lab_25AD.LO		; set 1/root2 pointer low byte
	LDY	#Lab_25AD.HI		; set 1/root2 pointer high byte
	JSR	Lab_246C		; add (AY) to FAC1 (1/root2)
	LDA	#Lab_25B1.LO		; set root2 pointer low byte
	LDY	#Lab_25B1.HI		; set root2 pointer high byte
	JSR	Lab_26CA		; convert AY and do (AY)/FAC1 (root2/(x+(1/root2)))
	LDA	#Lab_259C.LO		; set 1 pointer low byte
	LDY	#Lab_259C.HI		; set 1 pointer high byte
	JSR	Lab_2455		; subtract (AY) from FAC1 ((root2/(x+(1/root2)))-1)
	LDA	#Lab_25A0.LO		; set pointer low byte to counter
	LDY	#Lab_25A0.HI		; set pointer high byte to counter
	JSR	Lab_2B6E		; ^2 then series evaluation
	LDA	#Lab_25B5.LO		; set -0.5 pointer low byte
	LDY	#Lab_25B5.HI		; set -0.5 pointer high byte
	JSR	Lab_246C		; add (AY) to FAC1
	PLA				; restore FAC1 exponent
	JSR	Lab_2912		; evaluate new ASCII digit
	LDA	#Lab_25B9.LO		; set LOG(2) pointer low byte
	LDY	#Lab_25B9.HI		; set LOG(2) pointer high byte

; do convert AY, FCA1*(AY)

==Lab_25FB==
	JSR	Lab_264D		; unpack memory (AY) into FAC2
==Lab_MULTIPLY==
	BEQ	Lab_264C		; exit if zero

	JSR	Lab_2673		; test and adjust accumulators
	LDA	#0x00			; clear A
	STA	fact_1		; clear temp mantissa1
	STA	fact_2		; clear temp mantissa2
	STA	fact_3		; clear temp mantissa3
	LDA	fac1_r		; get FAC1 rounding byte
	JSR	Lab_2622		; go do shift/add FAC2
	LDA	fac1_3		; get FAC1 mantissa3
	JSR	Lab_2622		; go do shift/add FAC2
	LDA	fac1_2		; get FAC1 mantissa2
	JSR	Lab_2622		; go do shift/add FAC2
	LDA	fac1_1		; get FAC1 mantissa1
	JSR	Lab_2627		; go do shift/add FAC2
	JMP	Lab_273C		; copy temp to FAC1, normalise and return

==Lab_2622==
	BNE	Lab_2627		; branch if byte <> zero

	JMP	Lab_2569		; shift FCAtemp << A+8 times

					; else do shift and add
==Lab_2627==
	LSR				; shift byte
	ORA	#0x80			; set top bit (mark for 8 times)
==Lab_262A==
	TAY				; copy result
	BCC	Lab_2640		; skip next if bit was zero

	CLC				; clear carry for add
	LDA	fact_3		; get temp mantissa3
	ADC	fac2_3		; add FAC2 mantissa3
	STA	fact_3		; save temp mantissa3
	LDA	fact_2		; get temp mantissa2
	ADC	fac2_2		; add FAC2 mantissa2
	STA	fact_2		; save temp mantissa2
	LDA	fact_1		; get temp mantissa1
	ADC	fac2_1		; add FAC2 mantissa1
	STA	fact_1		; save temp mantissa1
==Lab_2640==
	ROR	fact_1		; shift temp mantissa1
	ROR	fact_2		; shift temp mantissa2
	ROR	fact_3		; shift temp mantissa3
	ROR	fac1_r		; shift temp rounding byte
	TYA				; get byte back
	LSR				; shift byte
	BNE	Lab_262A		; loop if all bits not done

==Lab_264C==
	RTS

; unpack memory (AY) into FAC2

==Lab_264D==
	STA	ut1_pl		; save pointer low byte
	STY	ut1_ph		; save pointer high byte
	LDY	#0x03			; 4 bytes to get (0-3)
	LDA	(ut1_pl),Y		; get mantissa3
	STA	fac2_3		; save FAC2 mantissa3
	DEY				; decrement index
	LDA	(ut1_pl),Y		; get mantissa2
	STA	fac2_2		; save FAC2 mantissa2
	DEY				; decrement index
	LDA	(ut1_pl),Y		; get mantissa1+sign
	STA	fac2_s		; save FAC2 sign (b7)
	EOR	fac1_s		; EOR with FAC1 sign (b7)
	STA	fac_sc		; save sign compare (FAC1 EOR FAC2)
	LDA	fac2_s		; recover FAC2 sign (b7)
	ORA	#0x80			; set 1xxx xxx (set normal bit)
	STA	fac2_1		; save FAC2 mantissa1
	DEY				; decrement index
	LDA	(ut1_pl),Y		; get exponent byte
	STA	fac2_e		; save FAC2 exponent
	LDA	fac1_e		; get FAC1 exponent
	RTS

; test and adjust accumulators

==Lab_2673==
	LDA	fac2_e		; get FAC2 exponent
==Lab_2675==
	BEQ	Lab_2696		; branch if FAC2 = 0x00 (handle underflow)

	CLC				; clear carry for add
	ADC	fac1_e		; add FAC1 exponent
	BCC	Lab_2680		; branch if sum of exponents 0x0100.LO

	BMI	Lab_269B		; do overflow error

	CLC				; clear carry for the add
	DATA	0x2C			; makes next line BIT 0x1410
==Lab_2680==
	BPL	Lab_2696		; if +ve go handle underflow

	ADC	#0x80			; adjust exponent
	STA	fac1_e		; save FAC1 exponent
	BNE	Lab_268B		; branch if not zero

	JMP	Lab_24F5		; save FAC1 sign and return

==Lab_268B==
	LDA	fac_sc		; get sign compare (FAC1 EOR FAC2)
	STA	fac1_s		; save FAC1 sign (b7)
==Lab_268F==
	RTS

; handle overflow and underflow

==Lab_2690==
	LDA	fac1_s		; get FAC1 sign (b7)
	BPL	Lab_269B		; do overflow error

					; handle underflow
==Lab_2696==
	PLA				; pop return address low byte
	PLA				; pop return address high byte
	JMP	Lab_24F1		; clear FAC1 exponent and sign and return

; multiply by 10

==Lab_269E==
	JSR	Lab_27AB		; round and copy FAC1 to FAC2
	TAX				; copy exponent (set the flags)
	BEQ	Lab_268F		; exit if zero

	CLC				; clear carry for add
	ADC	#0x02			; add two to exponent (*4)
	BCS	Lab_269B		; do overflow error if > 0xFF

	LDX	#0x00			; clear byte
	STX	fac_sc		; clear sign compare (FAC1 EOR FAC2)
	JSR	Lab_247C		; add FAC2 to FAC1 (*5)
	INC	fac1_e		; increment FAC1 exponent (*10)
	BNE	Lab_268F		; if non zero just do RTS

==Lab_269B==
	JMP	Lab_2564		; do overflow error and warm start

; divide by 10

==Lab_26B9==
	JSR	Lab_27AB		; round and copy FAC1 to FAC2
	LDA	#Lab_26B5.LO		; set pointer to 10d low addr
	LDY	#Lab_26B5.HI		; set pointer to 10d high addr
	LDX	#0x00			; clear sign

; divide by (AY) (X=sign)

==Lab_26C2==
	STX	fac_sc		; save sign compare (FAC1 EOR FAC2)
	JSR	Lab_UFAC		; unpack memory (AY) into FAC1
	JMP	Lab_DIVIDE		; do FAC2/FAC1

					; Perform divide-by
; convert AY and do (AY)/FAC1

==Lab_26CA==
	JSR	Lab_264D		; unpack memory (AY) into FAC2

					; Perform divide-into
==Lab_DIVIDE==
	BEQ	Lab_2737		; if zero go do /0 error

	JSR	Lab_27BA		; round FAC1
	LDA	#0x00			; clear A
	SEC				; set carry for subtract
	SBC	fac1_e		; subtract FAC1 exponent (2s complement)
	STA	fac1_e		; save FAC1 exponent
	JSR	Lab_2673		; test and adjust accumulators
	INC	fac1_e		; increment FAC1 exponent
	BEQ	Lab_269B		; if zero do overflow error

	LDX	#0xFF			; set index for pre increment
	LDA	#0x01			; set bit to flag byte save
==Lab_26E4==
	LDY	fac2_1		; get FAC2 mantissa1
	CPY	fac1_1		; compare FAC1 mantissa1
	BNE	Lab_26F4		; branch if <>

	LDY	fac2_2		; get FAC2 mantissa2
	CPY	fac1_2		; compare FAC1 mantissa2
	BNE	Lab_26F4		; branch if <>

	LDY	fac2_3		; get FAC2 mantissa3
	CPY	fac1_3		; compare FAC1 mantissa3
==Lab_26F4==
	PHP				; save FAC2-FAC1 compare status
	ROL				; shift the result byte
	BCC	Lab_2702		; if no carry skip the byte save

	LDY	#0x01			; set bit to flag byte save
	INX				; else increment the index to FACt
	CPX	#0x02			; compare with the index to fact_3
	BMI	Lab_2701		; if not last byte just go save it

	BNE	Lab_272B		; if all done go save FAC1 rounding byte, normalise and
					; return

	LDY	#0x40			; set bit to flag byte save for the rounding byte
==Lab_2701==
	STA	fact_1,X		; write result byte to fact_1 + index
	TYA				; copy the next save byte flag
==Lab_2702==
	PLP				; restore FAC2-FAC1 compare status
	BCC	Lab_2704		; if FAC2 < FAC1 then skip the subtract

	TAY				; save FAC2-FAC1 compare status
	LDA	fac2_3		; get FAC2 mantissa3
	SBC	fac1_3		; subtract FAC1 mantissa3
	STA	fac2_3		; save FAC2 mantissa3
	LDA	fac2_2		; get FAC2 mantissa2
	SBC	fac1_2		; subtract FAC1 mantissa2
	STA	fac2_2		; save FAC2 mantissa2
	LDA	fac2_1		; get FAC2 mantissa1
	SBC	fac1_1		; subtract FAC1 mantissa1
	STA	fac2_1		; save FAC2 mantissa1
	TYA				; restore FAC2-FAC1 compare status

					; FAC2 = FAC2*2
==Lab_2704==
	ASL	fac2_3		; shift FAC2 mantissa3
	ROL	fac2_2		; shift FAC2 mantissa2
	ROL	fac2_1		; shift FAC2 mantissa1
	BCS	Lab_26F4		; loop with no compare

	BMI	Lab_26E4		; loop with compare

	BPL	Lab_26F4		; loop always with no compare

; do A<6.LO, save as FAC1 rounding byte, normalise and return

==Lab_272B==
	LSR				; shift b1 - b0 ..
	ROR				; ..
	ROR				; .. to b7 - b6
	STA	fac1_r		; save FAC1 rounding byte
	PLP				; dump FAC2-FAC1 compare status
	JMP	Lab_273C		; copy temp to FAC1, normalise and return

; do "Divide by zero" error

==Lab_2737==
	LDX	#0x14			; error code 0x14 ("Divide by zero" error)
	JMP	Lab_XERR		; do error #X, then warm start

; copy temp to FAC1 and normalise

==Lab_273C==
	LDA	fact_1		; get temp mantissa1
	STA	fac1_1		; save FAC1 mantissa1
	LDA	fact_2		; get temp mantissa2
	STA	fac1_2		; save FAC1 mantissa2
	LDA	fact_3		; get temp mantissa3
	STA	fac1_3		; save FAC1 mantissa3
	JMP	Lab_24D5		; normalise FAC1 and return

; unpack memory (AY) into FAC1

==Lab_UFAC==
	STA	ut1_pl		; save pointer low byte
	STY	ut1_ph		; save pointer high byte
	LDY	#0x03			; 4 bytes to do
	LDA	(ut1_pl),Y		; get last byte
	STA	fac1_3		; save FAC1 mantissa3
	DEY				; decrement index
	LDA	(ut1_pl),Y		; get last-1 byte
	STA	fac1_2		; save FAC1 mantissa2
	DEY				; decrement index
	LDA	(ut1_pl),Y		; get second byte
	STA	fac1_s		; save FAC1 sign (b7)
	ORA	#0x80			; set 1xxx xxxx (add normal bit)
	STA	fac1_1		; save FAC1 mantissa1
	DEY				; decrement index
	LDA	(ut1_pl),Y		; get first byte (exponent)
	STA	fac1_e		; save FAC1 exponent
	STY	fac1_r		; clear FAC1 rounding byte
	RTS

; pack FAC1 into adatal

==Lab_276E==
	LDX	#adatal.LO		; set pointer low byte
==Lab_2770==
	LDY	#adatal.HI		; set pointer high byte
	BEQ	Lab_2778		; pack FAC1 into (XY) and return

; pack FAC1 into (lvarpl)

==Lab_PFAC==
	LDX	lvarpl		; get destination pointer low byte
	LDY	lvarph		; get destination pointer high byte

; pack FAC1 into (XY)

==Lab_2778==
	JSR	Lab_27BA		; round FAC1
	STX	ut1_pl		; save pointer low byte
	STY	ut1_ph		; save pointer high byte
	LDY	#0x03			; set index
	LDA	fac1_3		; get FAC1 mantissa3
	STA	(ut1_pl),Y		; store in destination
	DEY				; decrement index
	LDA	fac1_2		; get FAC1 mantissa2
	STA	(ut1_pl),Y		; store in destination
	DEY				; decrement index
	LDA	fac1_s		; get FAC1 sign (b7)
	ORA	#0x7F			; set bits x111 1111
	AND	fac1_1		; AND in FAC1 mantissa1
	STA	(ut1_pl),Y		; store in destination
	DEY				; decrement index
	LDA	fac1_e		; get FAC1 exponent
	STA	(ut1_pl),Y		; store in destination
	STY	fac1_r		; clear FAC1 rounding byte
	RTS

; round and copy FAC1 to FAC2

==Lab_27AB==
	JSR	Lab_27BA		; round FAC1

; copy FAC1 to FAC2

==Lab_27AE==
	LDX	#0x05			; 5 bytes to copy
==Lab_27B0==
	LDA	fac1_e-1,X		; get byte from FAC1,X
	STA	fac1_o,X		; save byte at FAC2,X
	DEX				; decrement count
	BNE	Lab_27B0		; loop if not all done

	STX	fac1_r		; clear FAC1 rounding byte
==Lab_27B9==
	RTS

; round FAC1

==Lab_27BA==
	LDA	fac1_e		; get FAC1 exponent
	BEQ	Lab_27B9		; exit if zero

	ASL	fac1_r		; shift FAC1 rounding byte
	BCC	Lab_27B9		; exit if no overflow

; round FAC1 (no check)

==Lab_27C2==
	JSR	Lab_2559		; increment FAC1 mantissa
	BNE	Lab_27B9		; branch if no overflow

	JMP	Lab_252A		; normalise FAC1 for C=1 and return

; get FAC1 sign
; return A=FF,C=1/-ve A=01,C=0/+ve

==Lab_27CA==
	LDA	fac1_e		; get FAC1 exponent
	BEQ	Lab_27D7		; exit if zero (already correct SGN(0)=0)

; return A=FF,C=1/-ve A=01,C=0/+ve
; no = 0 check

==Lab_27CE==
	LDA	fac1_s		; else get FAC1 sign (b7)

; return A=FF,C=1/-ve A=01,C=0/+ve
; no = 0 check, sign in A

==Lab_27D0==
	ROL				; move sign bit to carry
	LDA	#0xFF			; set byte for -ve result
	BCS	Lab_27D7		; return if sign was set (-ve)

	LDA	#0x01			; else set byte for +ve result
==Lab_27D7==
	RTS

; perform SGN()

==Lab_SGN==
	JSR	Lab_27CA		; get FAC1 sign
					; return A=0xFF/-ve A=0x01/+ve
; save A as integer byte

==Lab_27DB==
	STA	fac1_1		; save FAC1 mantissa1
	LDA	#0x00			; clear A
	STA	fac1_2		; clear FAC1 mantissa2
	LDX	#0x88			; set exponent

; set exp=X, clearFAC1 mantissa3 and normalise

==Lab_27E3==
	LDA	fac1_1		; get FAC1 mantissa1
	EOR	#0xFF			; complement it
	ROL				; sign bit into carry

; set exp=X, clearFAC1 mantissa3 and normalise

==Lab_STFA==
	LDA	#0x00			; clear A
	STA	fac1_3		; clear FAC1 mantissa3
	STX	fac1_e		; set FAC1 exponent
	STA	fac1_r		; clear FAC1 rounding byte
	STA	fac1_s		; clear FAC1 sign (b7)
	JMP	Lab_24D0		; do ABS and normalise FAC1

; perform ABS()

==Lab_ABS==
	LSR	fac1_s		; clear FAC1 sign (put zero in b7)
	RTS

; compare FAC1 with (AY)
; returns A=0x00 if FAC1 = (AY)
; returns A=0x01 if FAC1 > (AY)
; returns A=0xFF if FAC1 < (AY)

==Lab_27F8==
	STA	ut2_pl		; save pointer low byte
==Lab_27FA==
	STY	ut2_ph		; save pointer high byte
	LDY	#0x00			; clear index
	LDA	(ut2_pl),Y		; get exponent
	INY				; increment index
	TAX				; copy (AY) exponent to X
	BEQ	Lab_27CA		; branch if (AY) exponent=0 and get FAC1 sign
					; A=FF,C=1/-ve A=01,C=0/+ve

	LDA	(ut2_pl),Y		; get (AY) mantissa1 (with sign)
	EOR	fac1_s		; EOR FAC1 sign (b7)
	BMI	Lab_27CE		; if signs <> do return A=FF,C=1/-ve
					; A=01,C=0/+ve and return

	CPX	fac1_e		; compare (AY) exponent with FAC1 exponent
	BNE	Lab_2828		; branch if different

	LDA	(ut2_pl),Y		; get (AY) mantissa1 (with sign)
	ORA	#0x80			; normalise top bit
	CMP	fac1_1		; compare with FAC1 mantissa1
	BNE	Lab_2828		; branch if different

	INY				; increment index
	LDA	(ut2_pl),Y		; get mantissa2
	CMP	fac1_2		; compare with FAC1 mantissa2
	BNE	Lab_2828		; branch if different

	INY				; increment index
	LDA	#0x7F			; set for 1/2 value rounding byte
	CMP	fac1_r		; compare with FAC1 rounding byte (set carry)
	LDA	(ut2_pl),Y		; get mantissa3
	SBC	fac1_3		; subtract FAC1 mantissa3
	BEQ	Lab_2850		; exit if mantissa3 equal

; gets here if number <> FAC1

==Lab_2828==
	LDA	fac1_s		; get FAC1 sign (b7)
	BCC	Lab_282E		; branch if FAC1 > (AY)

	EOR	#0xFF			; else toggle FAC1 sign
==Lab_282E==
	JMP	Lab_27D0		; return A=FF,C=1/-ve A=01,C=0/+ve

; convert FAC1 floating-to-fixed

==Lab_2831==
	LDA	fac1_e		; get FAC1 exponent
	BEQ	Lab_287F		; if zero go clear FAC1 and return

	SEC				; set carry for subtract
	SBC	#0x98			; subtract maximum integer range exponent
	BIT	fac1_s		; test FAC1 sign (b7)
	BPL	Lab_2845		; branch if FAC1 +ve

					; FAC1 was -ve
	TAX				; copy subtracted exponent
	LDA	#0xFF			; overflow for -ve number
	STA	fac1_o		; set FAC1 overflow byte
	JSR	Lab_253D		; twos complement FAC1 mantissa
	TXA				; restore subtracted exponent
==Lab_2845==
	LDX	#fac1_e		; set index to FAC1
	CMP	#0xF9			; compare exponent result
	BPL	Lab_2851		; if < 8 shifts shift FAC1 A times right and return

	JSR	Lab_257B		; shift FAC1 A times right (> 8 shifts)
	STY	fac1_o		; clear FAC1 overflow byte
==Lab_2850==
	RTS

; shift FAC1 A times right

==Lab_2851==
	TAY				; copy shift count
	LDA	fac1_s		; get FAC1 sign (b7)
	AND	#0x80			; mask sign bit only (x000 0000)
	LSR	fac1_1		; shift FAC1 mantissa1
	ORA	fac1_1		; OR sign in b7 FAC1 mantissa1
	STA	fac1_1		; save FAC1 mantissa1
	JSR	Lab_2592		; shift FAC1 Y times right
	STY	fac1_o		; clear FAC1 overflow byte
	RTS

; perform INT()

==Lab_INT==
	LDA	fac1_e		; get FAC1 exponent
	CMP	#0x98			; compare with max int
	BCS	Lab_2886		; exit if >= (already int, too big for fractional part!)

	JSR	Lab_2831		; convert FAC1 floating-to-fixed
	STY	fac1_r		; save FAC1 rounding byte
	LDA	fac1_s		; get FAC1 sign (b7)
	STY	fac1_s		; save FAC1 sign (b7)
	EOR	#0x80			; toggle FAC1 sign
	ROL				; shift into carry
	LDA	#0x98			; set new exponent
	STA	fac1_e		; save FAC1 exponent
	LDA	fac1_3		; get FAC1 mantissa3
	STA	temp3			; save for EXP() function
	JMP	Lab_24D0		; do ABS and normalise FAC1

; clear FAC1 and return

==Lab_287F==
	STA	fac1_1		; clear FAC1 mantissa1
	STA	fac1_2		; clear FAC1 mantissa2
	STA	fac1_3		; clear FAC1 mantissa3
	TAY				; clear Y
==Lab_2886==
	RTS

; get FAC1 from string
; this routine now handles hex and binary values from strings
; starting with "0x" and "%" respectively

==Lab_2887==
	LDY	#0x00			; clear Y
	STY	dtypef		; clear data type flag, 0xFF=string, 0x00=numeric
	LDX	#0x09			; set index
==Lab_288B==
	STY	numexp,X		; clear byte
	DEX				; decrement index
	BPL	Lab_288B		; loop until numexp to negnum (and FAC1) = 0x00

	BCC	Lab_28FE		; branch if 1st character numeric

; get FAC1 from string .. first character wasn't numeric

	CMP	#45			; else compare with "-"
	BNE	Lab_289A		; branch if not "-"

	STX	negnum		; set flag for -ve number (X = 0xFF)
	BEQ	Lab_289C		; branch always (go scan and check for hex/bin)

; get FAC1 from string .. first character wasn't numeric or -

==Lab_289A==
	CMP	#0x2B			; else compare with "+"
	BNE	Lab_289D		; branch if not "+" (go check for hex/bin)

; was "+" or "-" to start, so get next character

==Lab_289C==
	JSR	0x00BC		; increment and scan memory
	BCC	Lab_28FE		; branch if numeric character

; code here for hex and binary numbers

==Lab_289D==
	CMP	#24			; else compare with "0x"
	BNE	Lab_NHEX		; branch if not "0x"

	JMP	Lab_CHEX		; branch if "0x"

==Lab_NHEX==
	CMP	#0x25			; else compare with "%"
	BNE	Lab_28A3		; branch if not "%" (continue original code)

	JMP	Lab_CBIN		; branch if "%"

==Lab_289E==
	JSR	0x00BC		; increment and scan memory (ignore + or get next number)
==Lab_28A1==
	BCC	Lab_28FE		; branch if numeric character

; get FAC1 from string .. character wasn't numeric, -, +, hex or binary

==Lab_28A3==
	CMP	#0x2E			; else compare with "."
	BEQ	Lab_28D5		; branch if "."

; get FAC1 from string .. character wasn't numeric, -, + or .

	CMP	#69			; else compare with "E"
	BNE	Lab_28DB		; branch if not "E"

					; was "E" so evaluate exponential part
	JSR	0x00BC		; increment and scan memory
	BCC	Lab_28C7		; branch if numeric character

	CMP	#tk_MINUS		; else compare with token for -
	BEQ	Lab_28C2		; branch if token for -

	CMP	#45			; else compare with "-"
	BEQ	Lab_28C2		; branch if "-"

	CMP	#tk_PLUS		; else compare with token for +
	BEQ	Lab_28C4		; branch if token for +

	CMP	#0x2B			; else compare with "+"
	BEQ	Lab_28C4		; branch if "+"

	BNE	Lab_28C9		; branch always

==Lab_28C2==
	ROR	expneg		; set exponent -ve flag (C, which=1, into b7)
==Lab_28C4==
	JSR	0x00BC		; increment and scan memory
==Lab_28C7==
	BCC	Lab_2925		; branch if numeric character

==Lab_28C9==
	BIT	expneg		; test exponent -ve flag
	BPL	Lab_28DB		; if +ve go evaluate exponent

					; else do exponent = -exponent 
	LDA	#0x00			; clear result
	SEC				; set carry for subtract
	SBC	expcnt		; subtract exponent byte
	JMP	Lab_28DD		; go evaluate exponent

==Lab_28D5==
	ROR	numdpf		; set decimal point flag
	BIT	numdpf		; test decimal point flag
	BVC	Lab_289E		; branch if only one decimal point so far

					; evaluate exponent
==Lab_28DB==
	LDA	expcnt		; get exponent count byte
==Lab_28DD==
	SEC				; set carry for subtract
	SBC	numexp		; subtract numerator exponent
	STA	expcnt		; save exponent count byte
	BEQ	Lab_28F6		; branch if no adjustment

	BPL	Lab_28EF		; else if +ve go do FAC1*10^expcnt

					; else go do FAC1/10^(0-expcnt)
==Lab_28E6==
	JSR	Lab_26B9		; divide by 10
	INC	expcnt		; increment exponent count byte
	BNE	Lab_28E6		; loop until all done

	BEQ	Lab_28F6		; branch always

==Lab_28EF==
	JSR	Lab_269E		; multiply by 10
	DEC	expcnt		; decrement exponent count byte
	BNE	Lab_28EF		; loop until all done

==Lab_28F6==
	LDA	negnum		; get -ve flag
	BMI	Lab_28FB		; if -ve do - FAC1 and return

	RTS

; do - FAC1 and return

==Lab_28FB==
	JMP	Lab_GTHAN		; do - FAC1 and return

; do unsigned FAC1*10+number

==Lab_28FE==
	PHA				; save character
	BIT	numdpf		; test decimal point flag
	BPL	Lab_2905		; skip exponent increment if not set

	INC	numexp		; else increment number exponent
==Lab_2905==
	JSR	Lab_269E		; multiply FAC1 by 10
	PLA				; restore character
	AND	#0x0F			; convert to binary
	JSR	Lab_2912		; evaluate new ASCII digit
	JMP	Lab_289E		; go do next character

; evaluate new ASCII digit

==Lab_2912==
	PHA				; save digit
	JSR	Lab_27AB		; round and copy FAC1 to FAC2
	PLA				; restore digit
	JSR	Lab_27DB		; save A as integer byte
	LDA	fac2_s		; get FAC2 sign (b7)
	EOR	fac1_s		; toggle with FAC1 sign (b7)
	STA	fac_sc		; save sign compare (FAC1 EOR FAC2)
	LDX	fac1_e		; get FAC1 exponent
	JMP	Lab_ADD		; add FAC2 to FAC1 and return

; evaluate next character of exponential part of number

==Lab_2925==
	LDA	expcnt		; get exponent count byte
	CMP	#0x0A			; compare with 10 decimal
	BCC	Lab_2934		; branch if less

	LDA	#0x64			; make all -ve exponents = -100 decimal (causes underflow)
	BIT	expneg		; test exponent -ve flag
	BMI	Lab_2942		; branch if -ve

	JMP	Lab_2564		; else do overflow error

==Lab_2934==
	ASL A				; * 2
	ASL A				; * 4
	ADC	expcnt		; * 5
	ASL A				; * 10
	LDY	#0x00			; set index
	ADC	(bpntrl),Y		; add character (will be 0x30 too much!)
	SBC	#48-1		; convert character to binary
==Lab_2942==
	STA	expcnt		; save exponent count byte
	JMP	Lab_28C4		; go get next character

; print " in line [LINE #]"

==Lab_2953==
	LDA	#Lab_LMSG.LO		; point to " in line " message low byte
	LDY	#Lab_LMSG.HI		; point to " in line " message high byte
	JSR	Lab_18C3		; print null terminated string from memory

					; print Basic line #
	LDA	clineh		; get current line high byte
	LDX	clinel		; get current line low byte

; print XA as unsigned integer

==Lab_295E==
	STA	fac1_1		; save low byte as FAC1 mantissa1
	STX	fac1_2		; save high byte as FAC1 mantissa2
	LDX	#0x90			; set exponent to 16d bits
	SEC				; set integer is +ve flag
	JSR	Lab_STFA		; set exp=X, clearFAC1 mantissa3 and normalise
	LDY	#0x00			; clear index
	TYA				; clear A
	JSR	Lab_297B		; convert FAC1 to string, skip sign character save
	JMP	Lab_18C3		; print null terminated string from memory and return

; convert FAC1 to ASCII string result in (AY)
; not any more, moved scratchpad to page 0

==Lab_296E==
	LDY	#0x01			; set index = 1
	LDA	#0x20			; character = " " (assume +ve)
	BIT	fac1_s		; test FAC1 sign (b7)
	BPL	Lab_2978		; branch if +ve

	LDA	#0x2D			; else character = "-"
==Lab_2978==
	STA	decss,Y		; save leading character (" " or "-")
==Lab_297B==
	STA	fac1_s		; clear FAC1 sign (b7)
	STY	sendl			; save index
	INY				; increment index
	LDX	fac1_e		; get FAC1 exponent
	BNE	Lab_2989		; branch if FAC10.LO.HI

					; exponent was 0x00 so FAC1 is 0
	LDA	#48			; set character = "0"
	JMP	Lab_2A89		; save last character, [EOT] and exit

					; FAC1 is some non zero value
==Lab_2989==
	LDA	#0x00			; clear (number exponent count)
	CPX	#0x81			; compare FAC1 exponent with 0x81 (1.HI.00000)

	BCS	Lab_299A		; branch if FAC1=1.HI

					; FAC11.LO
	LDA	#Lab_294F.LO		; set pointer low byte to 1,000,000
	LDY	#Lab_294F.HI		; set pointer high byte to 1,000,000
	JSR	Lab_25FB		; do convert AY, FCA1*(AY)
	LDA	#0xFA			; set number exponent count (-6)
==Lab_299A==
	STA	numexp		; save number exponent count
==Lab_299C==
	LDA	#Lab_294B.LO		; set pointer low byte to 999999.4375 (max before sci note)
	LDY	#Lab_294B.HI		; set pointer high byte to 999999.4375
	JSR	Lab_27F8		; compare FAC1 with (AY)
	BEQ	Lab_29C3		; exit if FAC1 = (AY)

	BPL	Lab_29B9		; go do /10 if FAC1 > (AY)

					; FAC1 < (AY)
==Lab_29A7==
	LDA	#Lab_2947.LO		; set pointer low byte to 99999.9375
	LDY	#Lab_2947.HI		; set pointer high byte to 99999.9375
	JSR	Lab_27F8		; compare FAC1 with (AY)
	BEQ	Lab_29B2		; branch if FAC1 = (AY) (allow decimal places)

	BPL	Lab_29C0		; branch if FAC1 > (AY) (no decimal places)

					; FAC1 <= (AY)
==Lab_29B2==
	JSR	Lab_269E		; multiply by 10
	DEC	numexp		; decrement number exponent count
	BNE	Lab_29A7		; go test again (branch always)

==Lab_29B9==
	JSR	Lab_26B9		; divide by 10
	INC	numexp		; increment number exponent count
	BNE	Lab_299C		; go test again (branch always)

; now we have just the digits to do

==Lab_29C0==
	JSR	Lab_244E		; add 0.5 to FAC1 (round FAC1)
==Lab_29C3==
	JSR	Lab_2831		; convert FAC1 floating-to-fixed
	LDX	#0x01			; set default digits before dp = 1
	LDA	numexp		; get number exponent count
	CLC				; clear carry for add
	ADC	#0x07			; up to 6 digits before point
	BMI	Lab_29D8		; if -ve then 1 digit before dp

	CMP	#0x08			; A>=8 if n>=1E6
	BCS	Lab_29D9		; branch if >= 0x08

					; carry is clear
	ADC	#0xFF			; take 1 from digit count
	TAX				; copy to A
	LDA	#0x02			;.set exponent adjust
==Lab_29D8==
	SEC				; set carry for subtract
==Lab_29D9==
	SBC	#0x02			; -2
	STA	expcnt		;.save exponent adjust
	STX	numexp		; save digits before dp count
	TXA				; copy to A
	BEQ	Lab_29E4		; branch if no digits before dp

	BPL	Lab_29F7		; branch if digits before dp

==Lab_29E4==
	LDY	sendl			; get output string index
	LDA	#0x2E			; character "."
	INY				; increment index
	STA	decss,Y		; save to output string
	TXA				;.
	BEQ	Lab_29F5		;.

	LDA	#48			; character "0"
	INY				; increment index
	STA	decss,Y		; save to output string
==Lab_29F5==
	STY	sendl			; save output string index
==Lab_29F7==
	LDY	#0x00			; clear index (point to 100,000)
	LDX	#0x80			; 
==Lab_29FB==
	LDA	fac1_3		; get FAC1 mantissa3
	CLC				; clear carry for add
	ADC	Lab_2A9C,Y		; add -ve LSB
	STA	fac1_3		; save FAC1 mantissa3
	LDA	fac1_2		; get FAC1 mantissa2
	ADC	Lab_2A9B,Y		; add -ve NMSB
	STA	fac1_2		; save FAC1 mantissa2
	LDA	fac1_1		; get FAC1 mantissa1
	ADC	Lab_2A9A,Y		; add -ve MSB
	STA	fac1_1		; save FAC1 mantissa1
	INX				; 
	BCS	Lab_2A18		; 

	BPL	Lab_29FB		; not -ve so try again

	BMI	Lab_2A1A		; 

==Lab_2A18==
	BMI	Lab_29FB		; 

==Lab_2A1A==
	TXA				; 
	BCC	Lab_2A21		; 

	EOR	#0xFF			; 
	ADC	#0x0A			; 
==Lab_2A21==
	ADC	#48-1		; add "0"-1 to result
	INY				; increment index ..
	INY				; .. to next less ..
	INY				; .. power of ten
	STY	cvaral		; save as current var address low byte
	LDY	sendl			; get output string index
	INY				; increment output string index
	TAX				; copy character to X
	AND	#0x7F			; mask out top bit
	STA	decss,Y		; save to output string
	DEC	numexp		; decrement # of characters before the dp
	BNE	Lab_2A3B		; branch if still characters to do

					; else output the point
	LDA	#0x2E			; character "."
	INY				; increment output string index
	STA	decss,Y		; save to output string
==Lab_2A3B==
	STY	sendl			; save output string index
	LDY	cvaral		; get current var address low byte
	TXA				; get character back
	EOR	#0xFF			; 
	AND	#0x80			; 
	TAX				; 
	CPY	#0x12			; compare index with max
	BNE	Lab_29FB		; loop if not max

					; now remove trailing zeroes
	LDY	sendl			; get output string index
==Lab_2A4B==
	LDA	decss,Y		; get character from output string
	DEY				; decrement output string index
	CMP	#48			; compare with "0"
	BEQ	Lab_2A4B		; loop until non "0" character found

	CMP	#0x2E			; compare with "."
	BEQ	Lab_2A58		; branch if was dp

					; restore last character
	INY				; increment output string index
==Lab_2A58==
	LDA	#0x2B			; character "+"
	LDX	expcnt		; get exponent count
	BEQ	Lab_2A8C		; if zero go set null terminator and exit

					; exponent isn't zero so write exponent
	BPL	Lab_2A68		; branch if exponent count +ve

	LDA	#0x00			; clear A
	SEC				; set carry for subtract
	SBC	expcnt		; subtract exponent count adjust (convert -ve to +ve)
	TAX				; copy exponent count to X
	LDA	#45			; character "-"
==Lab_2A68==
	STA	decss+2,Y		; save to output string
	LDA	#0x45			; character "E"
	STA	decss+1,Y		; save exponent sign to output string
	TXA				; get exponent count back
	LDX	#48-1		; one less than "0" character
	SEC				; set carry for subtract
==Lab_2A74==
	INX				; increment 10's character
	SBC	#0x0A			;.subtract 10 from exponent count
	BCS	Lab_2A74		; loop while still >= 0

	ADC	#58			; add character ":" (0x30+0x0A, result is 10 less that value)
	STA	decss+4,Y		; save to output string
	TXA				; copy 10's character
	STA	decss+3,Y		; save to output string
	LDA	#0x00			; set null terminator
	STA	decss+5,Y		; save to output string
	BEQ	Lab_2A91		; go set string pointer (AY) and exit (branch always)

					; save last character, [EOT] and exit
==Lab_2A89==
	STA	decss,Y		; save last character to output string

					; set null terminator and exit
==Lab_2A8C==
	LDA	#0x00			; set null terminator
	STA	decss+1,Y		; save after last character

					; set string pointer (AY) and exit
==Lab_2A91==
	LDA	#decssp1.LO		; set result string low pointer
	LDY	#decssp1.HI		; set result string high pointer
	RTS

; perform power function

==Lab_POWER==
	BEQ	Lab_EXP		; go do  EXP()

	LDA	fac2_e		; get FAC2 exponent
	BNE	Lab_2ABF		; branch if FAC20.LO.HI

	JMP	Lab_24F3		; clear FAC1 exponent and sign and return

==Lab_2ABF==
	LDX	#func_l.LO		; set destination pointer low byte
	LDY	#func_l.HI		; set destination pointer high byte
	JSR	Lab_2778		; pack FAC1 into (XY)
	LDA	fac2_s		; get FAC2 sign (b7)
	BPL	Lab_2AD9		; branch if FAC20.HI

					; else FAC2 is -ve and can only be raised to an
					; integer power which gives an x +j0 result
	JSR	Lab_INT		; perform INT
	LDA	#func_l.LO		; set source pointer low byte
	LDY	#func_l.HI		; set source pointer high byte
	JSR	Lab_27F8		; compare FAC1 with (AY)
	BNE	Lab_2AD9		; branch if FAC1 <> (AY) to allow Function Call error
					; this will leave FAC1 -ve and cause a Function Call
					; error when LOG() is called

	TYA				; clear sign b7
	LDY	temp3			; save mantissa 3 from INT() function as sign in Y
					; for possible later negation, b0
==Lab_2AD9==
	JSR	Lab_279D		; save FAC1 sign and copy ABS(FAC2) to FAC1
	TYA				; copy sign back ..
	PHA				; .. and save it
	JSR	Lab_LOG		; do LOG(n)
	LDA	#garb_l.LO		; set pointer low byte
	LDY	#garb_l.HI		; set pointer high byte
	JSR	Lab_25FB		; do convert AY, FCA1*(AY) (square the value)
	JSR	Lab_EXP		; go do EXP(n)
	PLA				; pull sign from stack
	LSR				; b0 is to be tested, shift to Cb
	BCC	Lab_2AF9		; if no bit then exit

					; Perform negation
; do - FAC1

==Lab_GTHAN==
	LDA	fac1_e		; get FAC1 exponent
	BEQ	Lab_2AF9		; exit if fac1_e = 0x00

	LDA	fac1_s		; get FAC1 sign (b7)
	EOR	#0xFF			; complement it
	STA	fac1_s		; save FAC1 sign (b7)
==Lab_2AF9==
	RTS

; perform EXP()	(x^e)

==Lab_EXP==
	LDA	#Lab_2AFA.LO		; set 1.443 pointer low byte
	LDY	#Lab_2AFA.HI		; set 1.443 pointer high byte
	JSR	Lab_25FB		; do convert AY, FCA1*(AY)
	LDA	fac1_r		; get FAC1 rounding byte
	ADC	#0x50			; +0x50/0x100
	BCC	Lab_2B2B		; skip rounding if no carry

	JSR	Lab_27C2		; round FAC1 (no check)
==Lab_2B2B==
	STA	fac2_r		; save FAC2 rounding byte
	JSR	Lab_27AE		; copy FAC1 to FAC2
	LDA	fac1_e		; get FAC1 exponent
	CMP	#0x88			; compare with EXP limit (256d)
	BCC	Lab_2B39		; branch if less

==Lab_2B36==
	JSR	Lab_2690		; handle overflow and underflow
==Lab_2B39==
	JSR	Lab_INT		; perform INT
	LDA	temp3			; get mantissa 3 from INT() function
	CLC				; clear carry for add
	ADC	#0x81			; normalise +1
	BEQ	Lab_2B36		; if 0x00 go handle overflow

	SEC				; set carry for subtract
	SBC	#0x01			; now correct for exponent
	PHA				; save FAC2 exponent

					; swap FAC1 and FAC2
	LDX	#0x04			; 4 bytes to do
==Lab_2B49==
	LDA	fac2_e,X		; get FAC2,X
	LDY	fac1_e,X		; get FAC1,X
	STA	fac1_e,X		; save FAC1,X
	STY	fac2_e,X		; save FAC2,X
	DEX				; decrement count/index
	BPL	Lab_2B49		; loop if not all done

	LDA	fac2_r		; get FAC2 rounding byte
	STA	fac1_r		; save as FAC1 rounding byte
	JSR	Lab_SUBTRACT	; perform subtraction, FAC2 from FAC1
	JSR	Lab_GTHAN		; do - FAC1
	LDA	#Lab_2AFE.LO		; set counter pointer low byte
	LDY	#Lab_2AFE.HI		; set counter pointer high byte
	JSR	Lab_2B84		; go do series evaluation
	LDA	#0x00			; clear A
	STA	fac_sc		; clear sign compare (FAC1 EOR FAC2)
	PLA				;.get saved FAC2 exponent
	JMP	Lab_2675		; test and adjust accumulators and return

; ^2 then series evaluation

==Lab_2B6E==
	STA	cptrl			; save count pointer low byte
	STY	cptrh			; save count pointer high byte
	JSR	Lab_276E		; pack FAC1 into adatal
	LDA	#adatal.LO		; set pointer low byte (Y already 0x00)
	JSR	Lab_25FB		; do convert AY, FCA1*(AY)
	JSR	Lab_2B88		; go do series evaluation
	LDA	#adatal.LO		; pointer to original # low byte
	LDY	#adatal.HI		; pointer to original # high byte
	JMP	Lab_25FB		; do convert AY, FCA1*(AY) and return

; series evaluation

==Lab_2B84==
	STA	cptrl			; save count pointer low byte
	STY	cptrh			; save count pointer high byte
==Lab_2B88==
	LDX	#numexp.LO		; set pointer low byte
	JSR	Lab_2770		; set pointer high byte and pack FAC1 into numexp
	LDA	(cptrl),Y		; get constants count
	STA	numcon		; save constants count
	LDY	cptrl			; get count pointer low byte
	INY				; increment it (now constants pointer)
	TYA				; copy it
	BNE	Lab_2B97		; skip next if no overflow

	INC	cptrh			; else increment high byte
==Lab_2B97==
	STA	cptrl			; save low byte
	LDY	cptrh			; get high byte
==Lab_2B9B==
	JSR	Lab_25FB		; do convert AY, FCA1*(AY)
	LDA	cptrl			; get constants pointer low byte
	LDY	cptrh			; get constants pointer high byte
	CLC				; clear carry for add
	ADC	#0x04			; +4 to  low pointer (4 bytes per constant)
	BCC	Lab_2BA8		; skip next if no overflow

	INY				; increment high byte
==Lab_2BA8==
	STA	cptrl			; save pointer low byte
	STY	cptrh			; save pointer high byte
	JSR	Lab_246C		; add (AY) to FAC1
	LDA	#numexp.LO		; set pointer low byte to partial @ numexp
	LDY	#numexp.HI		; set pointer high byte to partial @ numexp
	DEC	numcon		; decrement constants count
	BNE	Lab_2B9B		; loop until all done

	RTS

; RND(n), 32 bit Galoise version. make n=0 for 19th next number in sequence or n0.LO.HI
; to get 19th next number in sequence after seed n. This version of the PRNG uses
; the Galois method and a sample of 65536 bytes produced gives the following values.

; Entropy = 7.997442 bits per byte
; Optimum compression would reduce these 65536 bytes by 0 percent

; Chi square distribution for 65536 samples is 232.01, and
; randomly would exceed this value 75.00 percent of the time

; Arithmetic mean value of data bytes is 127.6724, 127.5 would be random
; Monte Carlo value for Pi is 3.122871269, error 0.60 percent
; Serial correlation coefficient is -0.000370, totally uncorrelated would be 0.0

==Lab_RND==
	LDA	fac1_e		; get FAC1 exponent
	BEQ	NextPRN		; do next random # if zero

					; else get seed into random number store
	LDX	#rbyte4		; set PRNG pointer low byte
	LDY	#0x00			; set PRNG pointer high byte
	JSR	Lab_2778		; pack FAC1 into (XY)
==NextPRN==
	LDX	#0xAF			; set EOR byte
	LDY	#0x13			; do this nineteen times
==LoopPRN==
	ASL	rbyte1		; shift PRNG most significant byte
	ROL	rbyte2		; shift PRNG middle byte
	ROL	rbyte3		; shift PRNG least significant byte
	ROL	rbyte4		; shift PRNG extra byte
	BCC	Ninc1			; branch if bit 32 clear

	TXA				; set EOR byte
	EOR	rbyte1		; EOR PRNG extra byte
	STA	rbyte1		; save new PRNG extra byte
==Ninc1==
	DEY				; decrement loop count
	BNE	LoopPRN		; loop if not all done

	LDX	#0x02			; three bytes to copy
==CopyPRNG==
	LDA	rbyte1,X		; get PRNG byte
	STA	fac1_1,X		; save FAC1 byte
	DEX
	BPL	CopyPRNG		; loop if not complete

	LDA	#0x80			; set the exponent
	STA	fac1_e		; save FAC1 exponent

	ASL A				; clear A
	STA	fac1_s		; save FAC1 sign

	JMP	Lab_24D5		; normalise FAC1 and return

; perform COS()

==Lab_COS==
	LDA	#Lab_2C78.LO		; set (pi/2) pointer low byte
	LDY	#Lab_2C78.HI		; set (pi/2) pointer high byte
	JSR	Lab_246C		; add (AY) to FAC1

; perform SIN()

==Lab_SIN==
	JSR	Lab_27AB		; round and copy FAC1 to FAC2
	LDA	#Lab_2C7C.LO		; set (2*pi) pointer low byte
	LDY	#Lab_2C7C.HI		; set (2*pi) pointer high byte
	LDX	fac2_s		; get FAC2 sign (b7)
	JSR	Lab_26C2		; divide by (AY) (X=sign)
	JSR	Lab_27AB		; round and copy FAC1 to FAC2
	JSR	Lab_INT		; perform INT
	LDA	#0x00			; clear byte
	STA	fac_sc		; clear sign compare (FAC1 EOR FAC2)
	JSR	Lab_SUBTRACT	; perform subtraction, FAC2 from FAC1
	LDA	#Lab_2C80.LO		; set 0.25 pointer low byte
	LDY	#Lab_2C80.HI		; set 0.25 pointer high byte
	JSR	Lab_2455		; perform subtraction, (AY) from FAC1
	LDA	fac1_s		; get FAC1 sign (b7)
	PHA				; save FAC1 sign
	BPL	Lab_2C35		; branch if +ve

					; FAC1 sign was -ve
	JSR	Lab_244E		; add 0.5 to FAC1
	LDA	fac1_s		; get FAC1 sign (b7)
	BMI	Lab_2C38		; branch if -ve

	LDA	cflag			; get comparison evaluation flag
	EOR	#0xFF			; toggle flag
	STA	cflag			; save comparison evaluation flag
==Lab_2C35==
	JSR	Lab_GTHAN		; do - FAC1
==Lab_2C38==
	LDA	#Lab_2C80.LO		; set 0.25 pointer low byte
	LDY	#Lab_2C80.HI		; set 0.25 pointer high byte
	JSR	Lab_246C		; add (AY) to FAC1
	PLA				; restore FAC1 sign
	BPL	Lab_2C45		; branch if was +ve

					; else correct FAC1
	JSR	Lab_GTHAN		; do - FAC1
==Lab_2C45==
	LDA	#Lab_2C84.LO		; set pointer low byte to counter
	LDY	#Lab_2C84.HI		; set pointer high byte to counter
	JMP	Lab_2B6E		; ^2 then series evaluation and return

; perform TAN()

==Lab_TAN==
	JSR	Lab_276E		; pack FAC1 into adatal
	LDA	#0x00			; clear byte
	STA	cflag			; clear comparison evaluation flag
	JSR	Lab_SIN		; go do SIN(n)
	LDX	#func_l.LO		; set sin(n) pointer low byte
	LDY	#func_l.HI		; set sin(n) pointer high byte
	JSR	Lab_2778		; pack FAC1 into (XY)
	LDA	#adatal.LO		; set n pointer low addr
	LDY	#adatal.HI		; set n pointer high addr
	JSR	Lab_UFAC		; unpack memory (AY) into FAC1
	LDA	#0x00			; clear byte
	STA	fac1_s		; clear FAC1 sign (b7)
	LDA	cflag			; get comparison evaluation flag
	JSR	Lab_2C74		; save flag and go do series evaluation

	LDA	#func_l.LO		; set sin(n) pointer low byte
	LDY	#func_l.HI		; set sin(n) pointer high byte
	JMP	Lab_26CA		; convert AY and do (AY)/FAC1

==Lab_2C74==
	PHA				; save comparison evaluation flag
	JMP	Lab_2C35		; go do series evaluation

; perform USR()

==Lab_USR==
	JSR	usrjmp		; call user code
	JMP	Lab_1BFB		; scan for ")", else do syntax error then warm start

; perform ATN()

==Lab_ATN==
	LDA	fac1_s		; get FAC1 sign (b7)
	PHA				; save sign
	BPL	Lab_2CA1		; branch if +ve

	JSR	Lab_GTHAN		; else do - FAC1
==Lab_2CA1==
	LDA	fac1_e		; get FAC1 exponent
	PHA				; push exponent
	CMP	#0x81			; compare with 1
	BCC	Lab_2CAF		; branch if FAC11.LO

	LDA	#Lab_259C.LO		; set 1 pointer low byte
	LDY	#Lab_259C.HI		; set 1 pointer high byte
	JSR	Lab_26CA		; convert AY and do (AY)/FAC1
==Lab_2CAF==
	LDA	#Lab_2CC9.LO		; set pointer low byte to counter
	LDY	#Lab_2CC9.HI		; set pointer high byte to counter
	JSR	Lab_2B6E		; ^2 then series evaluation
	PLA				; restore old FAC1 exponent
	CMP	#0x81			; compare with 1
	BCC	Lab_2CC2		; branch if FAC11.LO

	LDA	#Lab_2C78.LO		; set (pi/2) pointer low byte
	LDY	#Lab_2C78.HI		; set (pi/2) pointer high byte
	JSR	Lab_2455		; perform subtraction, (AY) from FAC1
==Lab_2CC2==
	PLA				; restore FAC1 sign
	BPL	Lab_2D04		; exit if was +ve

	JMP	Lab_GTHAN		; else do - FAC1 and return

; perform BITSET

==Lab_BITSET==
	JSR	Lab_GADB		; get two parameters for POKE or WAIT
	CPX	#0x08			; only 0 to 7 are allowed
	BCS	FCError		; branch if > 7

	LDA	#0x00			; clear A
	SEC				; set the carry
==S_Bits==
	ROL				; shift bit
	DEX				; decrement bit number
	BPL	S_Bits		; loop if still +ve

	INX				; make X = 0x00
	ORA	(itempl,X)		; or with byte via temporary integer (addr)
	STA	(itempl,X)		; save byte via temporary integer (addr)
==Lab_2D04==
	RTS

; perform BITCLR

==Lab_BITCLR==
	JSR	Lab_GADB		; get two parameters for POKE or WAIT
	CPX	#0x08			; only 0 to 7 are allowed
	BCS	FCError		; branch if > 7

	LDA	#0xFF			; set A
==S_Bitc==
	ROL				; shift bit
	DEX				; decrement bit number
	BPL	S_Bitc		; loop if still +ve

	INX				; make X = 0x00
	AND	(itempl,X)		; and with byte via temporary integer (addr)
	STA	(itempl,X)		; save byte via temporary integer (addr)
	RTS

==FCError==
	JMP	Lab_FCER		; do function call error then warm start

; perform BITTST()

==Lab_BTST==
	JSR	0x00BC		; increment BASIC pointer
	JSR	Lab_GADB		; get two parameters for POKE or WAIT
	CPX	#0x08			; only 0 to 7 are allowed
	BCS	FCError		; branch if > 7

	JSR	Lab_gbyt		; get next BASIC byte
	CMP	#0x28			; is next character ")"
	BEQ	TST_OK		; if ")" go do rest of function

	JMP	Lab_SNER		; do syntax error then warm start

==TST_OK==
	JSR	0x00BC		; update BASIC execute pointer (to character past ")")
	LDA	#0x00			; clear A
	SEC				; set the carry
==T_Bits==
	ROL				; shift bit
	DEX				; decrement bit number
	BPL	T_Bits		; loop if still +ve

	INX				; make X = 0x00
	AND	(itempl,X)		; AND with byte via temporary integer (addr)
	BEQ	Lab_NOTT		; branch if zero (already correct)

	LDA	#0xFF			; set for -1 result
==Lab_NOTT==
	JMP	Lab_27DB		; go do SGN tail

; perform BIN0x()

==Lab_BINS==
	CPX	#0x19			; max + 1
	BCS	BinFErr		; exit if too big ( > or = )

	STX	tempb			; save # of characters (0x00 = leading zero remove)
	LDA	#0x18			; need A byte long space
	JSR	Lab_MSSP		; make string space A bytes long
	LDY	#0x17			; set index
	LDX	#0x18			; character count
==NextB1==
	LSR	nums_1		; shift highest byte
	ROR	nums_2		; shift middle byte
	ROR	nums_3		; shift lowest byte bit 0 to carry
	TXA				; load with "0"/2
	ROL				; shift in carry
	STA	(str_pl),Y		; save to temp string + index
	DEY				; decrement index
	BPL	NextB1		; loop if not done

	LDA	tempb			; get # of characters
	BEQ	EndBHS		; branch if truncate

	TAX				; copy length to X
	SEC				; set carry for add !
	EOR	#0xFF			; 1's complement
	ADC	#0x18			; add 24d
	BEQ	GoPr2			; if zero print whole string

	BNE	GoPr1			; else go make output string
	
; this is the exit code and is also used by HEX0x()
; truncate string to remove leading "0"s

==EndBHS==
	TAY				; clear index (A=0, X=length here)
==NextB2==
	LDA	(str_pl),Y		; get character from string
	CMP	#48			; compare with "0"
	BNE	GoPr			; if not "0" then go print string from here

	DEX				; decrement character count
	BEQ	GoPr3			; if zero then end of string so go print it

	INY				; else increment index
	BPL	NextB2		; loop always

; make fixed length output string - ignore overflows!

==GoPr3==
	INX				; need at least 1 character
==GoPr==
	TYA				; copy result
==GoPr1==
	CLC				; clear carry for add
	ADC	str_pl		; add low address
	STA	str_pl		; save low address
	LDA	#0x00			; do high byte
	ADC	str_ph		; add high address
	STA	str_ph		; save high address
==GoPr2==
	STX	str_ln		; X holds string length
	JSR	0x00BC		; update BASIC execute pointer (to character past ")")
	JMP	Lab_RTST		; check for space on descriptor stack then put address
					; and length on descriptor stack and update stack pointers

==BinFErr==
	JMP	Lab_FCER		; do function call error then warm start

; perform HEX0x()

==Lab_HEXS==
	CPX	#0x07			; max + 1
	BCS	BinFErr		; exit if too big ( > or = )

	STX	tempb			; save # of characters

	LDA	#0x06			; need 6 bytes for string
	JSR	Lab_MSSP		; make string space A bytes long
	LDY	#0x05			; set string index

	SED				; need decimal mode for nibble convert
	LDA	nums_3		; get lowest byte
	JSR	Lab_A2HX		; convert A to ASCII hex byte and output
	LDA	nums_2		; get middle byte
	JSR	Lab_A2HX		; convert A to ASCII hex byte and output
	LDA	nums_1		; get highest byte
	JSR	Lab_A2HX		; convert A to ASCII hex byte and output
	CLD				; back to binary

	LDX	#0x06			; character count
	LDA	tempb			; get # of characters
	BEQ	EndBHS		; branch if truncate

	TAX				; copy length to X
	SEC				; set carry for add !
	EOR	#0xFF			; 1's complement
	ADC	#0x06			; add 6d
	BEQ	GoPr2			; if zero print whole string

	BNE	GoPr1			; else go make output string (branch always)

; convert A to ASCII hex byte and output .. note set decimal mode before calling

==Lab_A2HX==
	TAX				; save byte
	AND	#0x0F			; mask off top bits
	JSR	Lab_AL2X		; convert low nibble to ASCII and output
	TXA				; get byte back
	LSR				; /2	shift high nibble to low nibble
	LSR				; /4
	LSR				; /8
	LSR				; /16
==Lab_AL2X==
	CMP	#0x0A			; set carry for +1 if 9.HI
	ADC	#48			; add ASCII "0"
	STA	(str_pl),Y		; save to temp string
	DEY				; decrement counter
	RTS

==Lab_NLTO==
	STA	fac1_e		; save FAC1 exponent
	LDA	#0x00			; clear sign compare
==Lab_MLTE==
	STA	fac_sc		; save sign compare (FAC1 EOR FAC2)
	TXA				; restore character
	JSR	Lab_2912		; evaluate new ASCII digit

; gets here if the first character was "0x" for hex
; get hex number

==Lab_CHEX==
	JSR	0x00BC		; increment and scan memory
	BCC	Lab_ISHN		; branch if numeric character

	ORA	#0x20			; case convert, allow "A" to "F" and "a" to "f"
	SBC	#97			; subtract "a" (carry set here)
	CMP	#0x06			; compare normalised with 0x06 (max+1)
	BCS	Lab_EXCH		; exit if >"f" or <"0"

	ADC	#0x0A			; convert to nibble
==Lab_ISHN==
	AND	#0x0F			; convert to binary
	TAX				; save nibble
	LDA	fac1_e		; get FAC1 exponent
	BEQ	Lab_MLTE		; skip multiply if zero

	ADC	#0x04			; add four to exponent (*16 - carry clear here)
	BCC	Lab_NLTO		; if no overflow do evaluate digit

==Lab_MLTO==
	JMP	Lab_2564		; do overflow error and warm start

==Lab_NXCH==
	TAX				; save bit
	LDA	fac1_e		; get FAC1 exponent
	BEQ	Lab_MLBT		; skip multiply if zero

	INC	fac1_e		; increment FAC1 exponent (*2)
	BEQ	Lab_MLTO		; do overflow error if = 0x00

	LDA	#0x00			; clear sign compare
==Lab_MLBT==
	STA	fac_sc		; save sign compare (FAC1 EOR FAC2)
	TXA				; restore bit
	JSR	Lab_2912		; evaluate new ASCII digit

; gets here if the first character was  "%" for binary
; get binary number

==Lab_CBIN==
	JSR	0x00BC		; increment and scan memory
	EOR	#48			; convert "0" to 0 etc.
	CMP	#0x02			; compare with max+1
	BCC	Lab_NXCH		; branch exit if < 2

==Lab_EXCH==
	JMP	Lab_28F6		; evaluate -ve flag and return

; ctrl-c check routine. includes limited "life" byte save for INGET routine
; now also the code that checks to see if an interrupt has occurred

==CTRLC==
	LDA	Ccflag		; get [CTRL-C] check flag
	BNE	Lab_FBA2		; exit if inhibited

	JSR	V_inpt		; scan input device
	BCC	Lab_FBA0		; exit if buffer empty

	STA	Ccbyte		; save received byte
	LDX	#0x20			; "life" timer for bytes
	STX	Ccnull		; set countdown
	JMP	Lab_1636		; return to BASIC

==Lab_FBA0==
	LDX	Ccnull		; get countdown byte
	BEQ	Lab_FBA2		; exit if finished

	DEC	Ccnull		; else decrement countdown
==Lab_FBA2==
	LDX	#nmibase		; set pointer to NMI values
	JSR	Lab_CKIN		; go check interrupt
	LDX	#irqbase		; set pointer to IRQ values
	JSR	Lab_CKIN		; go check interrupt
==Lab_CRTS==
	RTS

; check whichever interrupt is indexed by X

==Lab_CKIN==
	LDA	plus_0,X		; get interrupt flag byte
	BPL	Lab_CRTS		; branch if interrupt not enabled

; we disable the interrupt here and make two new commands RETIRQ and RETNMI to
; automatically enable the interrupt when we exit

	ASL A				; move happened bit to setup bit
	AND	#0x40			; mask happened bits
	BEQ	Lab_CRTS		; if no interrupt then exit

	STA	plus_0,X		; save interrupt flag byte

	TXA				; copy index ..
	TAY				; .. to Y

	PLA				; dump return address low byte, call from CTRL-C
	PLA				; dump return address high byte

	LDA	#0x05			; need 5 bytes for GOSUB
	JSR	Lab_1212		; check room on stack for A bytes
	LDA	bpntrh		; get BASIC execute pointer high byte
	PHA				; push on stack
	LDA	bpntrl		; get BASIC execute pointer low byte
	PHA				; push on stack
	LDA	clineh		; get current line high byte
	PHA				; push on stack
	LDA	clinel		; get current line low byte
	PHA				; push on stack
	LDA	#tk_GOSUB		; token for GOSUB
	PHA				; push on stack

	LDA	plus_1,Y		; get interrupt code pointer low byte
	STA	bpntrl		; save as BASIC execute pointer low byte
	LDA	plus_2,Y		; get interrupt code pointer high byte
	STA	bpntrh		; save as BASIC execute pointer high byte

	JMP	Lab_15C2		; go do interpreter inner loop
					; can't RTS, we used the stack! the RTS from the ctrl-c
					; check will be taken when the RETIRQ/RETNMI/RETURN is
					; executed at the end of the subroutine

; get byte from input device, no waiting
; returns with carry set if byte in A

==INGET==
	JSR	V_inpt		; call scan input device
	BCS	Lab_FB95	; if byte go reset timer

	LDA	Ccnull		; get countdown
	BEQ	Lab_FB96	; exit if empty

	LDA	Ccbyte		; get last received byte
	SEC				; flag we got a byte
==Lab_FB95==
	LDX	#0x00			; clear X
	STX	Ccnull		; clear timer because we got a byte
==Lab_FB96==
	RTS

; these routines only enable the interrupts if the set-up flag is set
; if not they have no effect

; perform IRQ {ON|OFF|CLEAR}

==Lab_IRQ==
	LDX	#irqbase		; set pointer to IRQ values
	DATA	0x2C			; make next line BIT abs.

; perform NMI {ON|OFF|CLEAR}

==Lab_NMI==
	LDX	#nmibase		; set pointer to NMI values
	CMP	#tk_ON		; compare with token for ON
	BEQ	Lab_INON		; go turn on interrupt

	CMP	#tk_OFF		; compare with token for OFF
	BEQ	Lab_IOFF		; go turn off interrupt

	EOR	#tk_CLEAR		; compare with token for CLEAR, A = 0x00 if = tk_CLEAR
	BEQ	Lab_INEX		; go clear interrupt flags and return

	JMP	Lab_SNER		; do syntax error then warm start

==Lab_IOFF==
	LDA	#0x7F			; clear A
	AND	plus_0,X		; AND with interrupt setup flag
	BPL	Lab_INEX		; go clear interrupt enabled flag and return

==Lab_INON==
	LDA	plus_0,X		; get interrupt setup flag
	ASL A				; Shift bit to enabled flag
	ORA	plus_0,X		; OR with flag byte
==Lab_INEX==
	STA	plus_0,X		; save interrupt flag byte
	JMP	lab_igby		; update BASIC execute pointer and return

; these routines set up the pointers and flags for the interrupt routines
; note that the interrupts are also enabled by these commands

; perform ON IRQ

==Lab_SIRQ==
	CLI				; enable interrupts
	LDX	#irqbase		; set pointer to IRQ values
	DATA	0x2C			; make next line BIT abs.

; perform ON NMI

==Lab_SNMI==
	LDX	#nmibase		; set pointer to NMI values

	STX	tempb			; save interrupt pointer
	JSR	0x00BC		; increment and scan memory (past token)
	JSR	Lab_GFPN		; get fixed-point number into temp integer
	LDA	smeml			; get start of mem low byte
	LDX	smemh			; get start of mem high byte
	JSR	Lab_SHLN		; search Basic for temp integer line number from AX
	BCS	Lab_LFND		; if carry set go set-up interrupt

	JMP	Lab_16F7		; else go do "Undefined statement" error and warm start

==Lab_LFND==
	LDX	tempb			; get interrupt pointer
	LDA	baslnl		; get pointer low byte
	SBC	#0x01			; -1 (carry already set for subtract)
	STA	plus_1,X		; save as interrupt pointer low byte
	LDA	baslnh		; get pointer high byte
	SBC	#0x00			; subtract carry
	STA	plus_2,X		; save as interrupt pointer high byte

	LDA	#0xC0			; set interrupt enabled/setup bits
	STA	plus_0,X		; set interrupt flags
==Lab_IRTS==
	RTS

; return from IRQ service, restores the enabled flag.

; perform RETIRQ

==Lab_RETIRQ==
	BNE	Lab_IRTS		; exit if following token (to allow syntax error)

	LDA	irqbase		; get interrupt flags
	ASL A				; copy setup to enabled (b7)
	ORA	irqbase		; OR in setup flag
	STA	irqbase		; save enabled flag
	JMP	Lab_16E8		; go do rest of RETURN

; return from NMI service, restores the enabled flag.

; perform RETNMI

==Lab_RETNMI==
	BNE	Lab_IRTS		; exit if following token (to allow syntax error)

	LDA	nmibase		; get set-up flag
	ASL A				; copy setup to enabled (b7)
	ORA	nmibase		; OR in setup flag
	STA	nmibase		; save enabled flag
	JMP	Lab_16E8		; go do rest of RETURN

; MAX() MIN() pre process

==Lab_MMPP==
	JSR	Lab_EVEZ		; process expression
	JMP	Lab_CTNM		; check if source is numeric, else do type mismatch

; perform MAX()

==Lab_MAX==
	JSR	Lab_PHFA		; push FAC1, evaluate expression,
					; pull FAC2 and compare with FAC1
	BPL	Lab_MAX		; branch if no swap to do

	LDA	fac2_1		; get FAC2 mantissa1
	ORA	#0x80			; set top bit (clear sign from compare)
	STA	fac2_1		; save FAC2 mantissa1
	JSR	Lab_279B		; copy FAC2 to FAC1
	BEQ	Lab_MAX		; go do next (branch always)

; perform MIN()

==Lab_MIN==
	JSR	Lab_PHFA		; push FAC1, evaluate expression,
					; pull FAC2 and compare with FAC1
	BMI	Lab_MIN		; branch if no swap to do

	BEQ	Lab_MIN		; branch if no swap to do

	LDA	fac2_1		; get FAC2 mantissa1
	ORA	#0x80			; set top bit (clear sign from compare)
	STA	fac2_1		; save FAC2 mantissa1
	JSR	Lab_279B		; copy FAC2 to FAC1
	BEQ	Lab_MIN		; go do next (branch always)

; exit routine. don't bother returning to the loop code
; check for correct exit, else so syntax error

==Lab_MMEC==
	CMP	#0x28			; is it end of function?
	BNE	Lab_MMSE		; if not do MAX MIN syntax error

	PLA				; dump return address low byte
	PLA				; dump return address high byte
	JMP	lab_igby		; update BASIC execute pointer (to chr past ")")

==Lab_MMSE==
	JMP	Lab_SNER		; do syntax error then warm start

; check for next, evaluate and return or exit
; this is the routine that does most of the work

==Lab_PHFA==
	JSR	Lab_gbyt		; get next BASIC byte
	CMP	#0x3B			; is there more ?
	BNE	Lab_MMEC		; if not go do end check

					; push FAC1
	JSR	Lab_27BA		; round FAC1
	LDA	fac1_s		; get FAC1 sign
	ORA	#0x7F			; set all non sign bits
	AND	fac1_1		; AND FAC1 mantissa1 (AND in sign bit)
	PHA				; push on stack
	LDA	fac1_2		; get FAC1 mantissa2
	PHA				; push on stack
	LDA	fac1_3		; get FAC1 mantissa3
	PHA				; push on stack
	LDA	fac1_e		; get FAC1 exponent
	PHA				; push on stack

	JSR	0x00BC		; scan and get next BASIC byte (after ",")
	JSR	Lab_EVNM		; evaluate expression and check is numeric,
					; else do type mismatch

					; pop FAC2 (MAX/MIN expression so far)
	PLA				; pop exponent
	STA	fac2_e		; save FAC2 exponent
	PLA				; pop mantissa3
	STA	fac2_3		; save FAC2 mantissa3
	PLA				; pop mantissa1
	STA	fac2_2		; save FAC2 mantissa2
	PLA				; pop sign/mantissa1
	STA	fac2_1		; save FAC2 sign/mantissa1
	STA	fac2_s		; save FAC2 sign

					; compare FAC1 with (packed) FAC2
	LDA	#fac2_e.LO		; set pointer low byte to FAC2
	LDY	#fac2_e.HI		; set pointer high byte to FAC2
	JMP	Lab_27F8		; compare FAC1 with FAC2 (AY) and return
					; returns A=0x00 if FAC1 = (AY)
					; returns A=0x01 if FAC1 > (AY)
					; returns A=0xFF if FAC1 < (AY)

; perform WIDTH

==Lab_WDTH==
	CMP	#0x3B			; is next byte ","
	BEQ	Lab_TBSZ		; if so do tab size

	JSR	Lab_GTBY		; get byte parameter
	TXA				; copy width to A
	BEQ	Lab_NSTT		; branch if set for infinite line

	CPX	#0x10			; else make min width = 16d
	BCC	TabErr		; if less do function call error and exit

; this next compare ensures that we can't exit WIDTH via an error leaving the
; tab size greater than the line length.

	CPX	tabsiz		; compare with tab size
	BCS	Lab_NSTT		; branch if >= tab size

	STX	tabsiz		; else make tab size = terminal width
==Lab_NSTT==
	STX	tWidth		; set the terminal width
	JSR	Lab_gbyt		; get BASIC byte back
	BEQ	WExit			; exit if no following

	CMP	#0x3B			; else is it ","
	BNE	Lab_MMSE		; if not do syntax error

==Lab_TBSZ==
	JSR	Lab_SGBY		; scan and get byte parameter
	TXA				; copy TAB size
	BMI	TabErr		; if 127.HI do function call error and exit

	CPX	#0x01			; compare with min-1
	BCC	TabErr		; if <=1 do function call error and exit

	LDA	tWidth		; set flags for width
	BEQ	Lab_SVTB		; skip check if infinite line

	CPX	tWidth		; compare TAB with width
	BEQ	Lab_SVTB		; ok if =

	BCS	TabErr		; branch if too big

==Lab_SVTB==
	STX	tabsiz		; save TAB size

; calculate tab column limit from TAB size. The iclim is set to the last tab
; position on a line that still has at least one whole tab width between it
; and the end of the line.

==WExit==
	LDA	tWidth		; get width
	BEQ	Lab_SULP		; branch if infinite line

	CMP	tabsiz		; compare with tab size
	BCS	Lab_WDLP		; branch if >= tab size

	STA	tabsiz		; else make tab size = terminal width
==Lab_SULP==
	SEC				; set carry for subtract
==Lab_WDLP==
	SBC	tabsiz		; subtract tab size
	BCS	Lab_WDLP		; loop while no borrow

	ADC	tabsiz		; add tab size back
	CLC				; clear carry for add
	ADC	tabsiz		; add tab size back again
	STA	iclim			; save for now
	LDA	tWidth		; get width back
	SEC				; set carry for subtract
	SBC	iclim			; subtract remainder
	STA	iclim			; save tab column limit
==Lab_NOSQ==
	RTS

==TabErr==
	JMP	Lab_FCER		; do function call error then warm start

; perform SQR()

==Lab_SQR==
	LDA	fac1_s		; get FAC1 sign
	BMI	TabErr		; if -ve do function call error

	LDA	fac1_e		; get exponent
	BEQ	Lab_NOSQ		; if zero just return

					; else do root
	JSR	Lab_27AB		; round and copy FAC1 to FAC2
	LDA	#0x00			; clear A

	STA	fact_3		; clear remainder
	STA	fact_2		; ..
	STA	fact_1		; ..
	STA	tempb			; ..

	STA	fac1_3		; clear root
	STA	fac1_2		; ..
	STA	fac1_1		; ..

	LDX	#0x18			; 24 pairs of bits to do
	LDA	fac2_e		; get exponent
	LSR				; check odd/even
	BCS	Lab_SQE2		; if odd only 1 shift first time

==Lab_SQE1==
	ASL	fac2_3		; shift highest bit of number ..
	ROL	fac2_2		; ..
	ROL	fac2_1		; ..
	ROL	fact_3		; .. into remainder
	ROL	fact_2		; ..
	ROL	fact_1		; ..
	ROL	tempb			; .. never overflows
==Lab_SQE2==
	ASL	fac2_3		; shift highest bit of number ..
	ROL	fac2_2		; ..
	ROL	fac2_1		; ..
	ROL	fact_3		; .. into remainder
	ROL	fact_2		; ..
	ROL	fact_1		; ..
	ROL	tempb			; .. never overflows

	ASL	fac1_3		; root = root * 2
	ROL	fac1_2		; ..
	ROL	fac1_1		; .. never overflows

	LDA	fac1_3		; get root low byte
	ROL				; *2
	STA	temp3			; save partial low byte
	LDA	fac1_2		; get root low mid byte
	ROL				; *2
	STA	temp3+1		; save partial low mid byte
	LDA	fac1_1		; get root high mid byte
	ROL				; *2
	STA	temp3+2		; save partial high mid byte
	LDA	#0x00			; get root high byte (always 0x00)
	ROL				; *2
	STA	temp3+3		; save partial high byte

					; carry clear for subtract +1
	LDA	fact_3		; get remainder low byte
	SBC	temp3			; subtract partial low byte
	STA	temp3			; save partial low byte

	LDA	fact_2		; get remainder low mid byte
	SBC	temp3+1		; subtract partial low mid byte
	STA	temp3+1		; save partial low mid byte

	LDA	fact_1		; get remainder high mid byte
	SBC	temp3+2		; subtract partial high mid byte
	TAY				; copy partial high mid byte

	LDA	tempb			; get remainder high byte
	SBC	temp3+3		; subtract partial high byte
	BCC	Lab_SQNS		; skip sub if remainder smaller

	STA	tempb			; save remainder high byte

	STY	fact_1		; save remainder high mid byte

	LDA	temp3+1		; get remainder low mid byte
	STA	fact_2		; save remainder low mid byte

	LDA	temp3			; get partial low byte
	STA	fact_3		; save remainder low byte

	INC	fac1_3		; increment root low byte (never any rollover)
==Lab_SQNS==
	DEX				; decrement bit pair count
	BNE	Lab_SQE1		; loop if not all done

	SEC				; set carry for subtract
	LDA	fac2_e		; get exponent
	SBC	#0x80			; normalise
	ROR				; /2 and re-bias to 0x80
	ADC	#0x00			; add bit zero back in (allow for half shift)
	STA	fac1_e		; save it
	JMP	Lab_24D5		; normalise FAC1 and return

; perform VARPTR()

==Lab_VARPTR==
	JSR	0x00BC		; increment and scan memory
	JSR	Lab_GVAR		; get var address
	JSR	Lab_1BFB		; scan for ")" , else do syntax error then warm start
	LDY	cvaral		; get var address low byte
	LDA	cvarah		; get var address high byte
	JMP	Lab_AYFC		; save and convert integer AY to FAC1 and return

; perform PI

==Lab_PI==
	LDA	#Lab_2C7C.LO		; set (2*pi) pointer low byte
	LDY	#Lab_2C7C.HI		; set (2*pi) pointer high byte
	JSR	Lab_UFAC		; unpack memory (AY) into FAC1
	DEC	fac1_e		; make result = PI
	RTS

; perform TWOPI

==Lab_TWOPI==
	LDA	#Lab_2C7C.LO		; set (2*pi) pointer low byte
	LDY	#Lab_2C7C.HI		; set (2*pi) pointer high byte
	JMP	Lab_UFAC		; unpack memory (AY) into FAC1 and return

; system dependant i/o vectors
; these are in RAM and are set by the monitor at start-up

==V_inpt==
	JMP	(Vec_in)		; non halting scan input device
==V_outp==
	JMP	(Vec_out)		; send byte to output device
==V_LOAD==
	JMP	(Vec_ld)		; load BASIC program
==V_SAVE==
	JMP	(Vec_sv)		; save BASIC program

; The rest are tables messages and code for RAM

; the rest of the code is tables and BASIC start-up code

==Pg2_tabs==
	DATA	0x00			; ctrl-c flag		-	0x00 = enabled
	DATA	0x00			; ctrl-c byte		-	GET needs this
	DATA	0x00			; ctrl-c byte timeout	-	GET needs this
	DATA	CTRLC			; ctrl c check vector
;	DATA	xxxx			; non halting key input	-	monitor to set this
;	DATA	xxxx			; output vector		-	monitor to set this
;	DATA	xxxx			; load vector		-	monitor to set this
;	DATA	xxxx			; save vector		-	monitor to set this
==Pg2_tabe==

; character get subroutine for zero page

; For a 1.8432MHz 6502 including the JSR and RTS
; fastest (>=":")	=  29 cycles =  15.7uS
; slowest (<":")	=  40 cycles =  21.7uS
; space skip	= +21 cycles = +11.4uS
; inc across page	=  +4 cycles =  +2.2uS

; the target address for the LDA at Lab_2CF4 becomes the BASIC execute pointer once the
; block is copied to it's destination, any non zero page address will do at assembly
; time, to assemble a three byte instruction.

; page 0 initialisation table from 0xBC
; increment and scan memory

==Lab_2CEE==
	INC	bpntrl		; increment BASIC execute pointer low byte
	BNE	Lab_2CF4		; branch if no carry
					; else
	INC	bpntrh		; increment BASIC execute pointer high byte

; page 0 initialisation table from 0xC2
; scan memory

==Lab_2CF4==
	LDA	0xFFFF			; get byte to scan (addr set by call routine)
	CMP	#tk_ELSE		; compare with the token for ELSE
	BEQ	Lab_2D05		; exit if ELSE, not numeric, carry set

	CMP	#58			; compare with ":"
	BCS	Lab_2D05		; exit if >= ":", not numeric, carry set

	CMP	#32			; compare with " "
	BEQ	Lab_2CEE		; if " " go do next

	SEC				; set carry for SBC
	SBC	#48			; subtract "0"
	SEC				; set carry for SBC
	SBC	#0xD0			; subtract -"0"
					; clear carry if byte = "0"-"9"
==Lab_2D05==
	RTS

; page zero initialisation table 0x00-0x12 inclusive

==StrTab==
	DATA	0x4C			; JMP opcode
	DATA Lab_cold		; initial warm start vector (cold start)

	DATA	0x00			; these bytes are not used by BASIC
	DATA	0x0000			; 
	DATA	0x0000			; 
	DATA	0x0000			; 

	DATA	0x4C			; JMP opcode
	DATA	Lab_FCER		; initial user function vector ("Function call" error)
	DATA	0x00			; default NULL count
	DATA	0x00			; clear terminal position
	DATA	0x00			; default terminal width byte
	DATA	0xF2			; default limit for TAB = 14
	DATA	Ram_base		; start of user RAM
==EndTab==

==Lab_MSZM==
	DATA	"\0D\0AMemory size \00"

==Lab_SMSG==
	DATA	" Bytes free\0D\0A\0A"
	DATA	"Enhanced BASIC 2.22\0A\00"

; numeric constants and series

					; constants and series for LOG(n)
==Lab_25A0==
	DATA	0x02			; counter
	DATA	0x80,0x19,0x56,0x62	; 0.59898
	DATA	0x80,0x76,0x22,0xF3	; 0.96147
;##	DATA	0x80,0x76,0x22,0xF1	; 0.96147
	DATA	0x82,0x38,0xAA,0x40	; 2.88539
;##	DATA	0x82,0x38,0xAA,0x45	; 2.88539

==Lab_25AD==
	DATA	0x80,0x35,0x04,0xF3	; 0.70711	1/root 2
==Lab_25B1==
	DATA	0x81,0x35,0x04,0xF3	; 1.41421	root 2
==Lab_25B5==
	DATA	0x80,0x80,0x00,0x00	; -0.5
==Lab_25B9==
	DATA	0x80,0x31,0x72,0x18	; 0.69315	LOG(2)

					; numeric PRINT constants
==Lab_2947==
	DATA	0x91,0x43,0x4F,0xF8	; 99999.9375 (max value with at least one decimal)
==Lab_294B==
	DATA	0x94,0x74,0x23,0xF7	; 999999.4375 (max value before scientific notation)
==Lab_294F==
	DATA	0x94,0x74,0x24,0x00	; 1000000

					; EXP(n) constants and series
==Lab_2AFA==
	DATA	0x81,0x38,0xAA,0x3B	; 1.4427	(1/LOG base 2 e)
==Lab_2AFE==
	DATA	0x06			; counter
	DATA	0x74,0x63,0x90,0x8C	; 2.17023e-4
	DATA	0x77,0x23,0x0C,0xAB	; 0.00124
	DATA	0x7A,0x1E,0x94,0x00	; 0.00968
	DATA	0x7C,0x63,0x42,0x80	; 0.05548
	DATA	0x7E,0x75,0xFE,0xD0	; 0.24023
	DATA	0x80,0x31,0x72,0x15	; 0.69315
	DATA	0x81,0x00,0x00,0x00	; 1.00000

;##	DATA	0x07			; counter
;##	DATA	0x74,0x94,0x2E,0x40	; -1/7! (-1/5040)
;##	DATA	0x77,0x2E,0x4F,0x70	;  1/6! ( 1/720)
;##	DATA	0x7A,0x88,0x02,0x6E	; -1/5! (-1/120)
;##	DATA	0x7C,0x2A,0xA0,0xE6	;  1/4! ( 1/24)
;##	DATA	0x7E,0xAA,0xAA,0x50	; -1/3! (-1/6)
;##	DATA	0x7F,0x7F,0xFF,0xFF	;  1/2! ( 1/2)
;##	DATA	0x81,0x80,0x00,0x00	; -1/1! (-1/1)
;##	DATA	0x81,0x00,0x00,0x00	;  1/0! ( 1/1)

					; trigonometric constants and series
==Lab_2C78==
	DATA	0x81,0x49,0x0F,0xDB	; 1.570796371 (pi/2) as floating #
==Lab_2C84==
	DATA	0x04			; counter
	DATA	0x86,0x1E,0xD7,0xFB	; 39.7109
;##	DATA	0x86,0x1E,0xD7,0xBA	; 39.7109
	DATA	0x87,0x99,0x26,0x65	;-76.575
;##	DATA	0x87,0x99,0x26,0x64	;-76.575
	DATA	0x87,0x23,0x34,0x58	; 81.6022
	DATA	0x86,0xA5,0x5D,0xE1	;-41.3417
;##	DATA	0x86,0xA5,0x5D,0xE0	;-41.3417
==Lab_2C7C==
	DATA	0x83,0x49,0x0F,0xDB	; 6.28319 (2*pi) as floating #
;##	DATA	0x83,0x49,0x0F,0xDA	; 6.28319 (2*pi) as floating #

==Lab_2CC9==
	DATA	0x08			; counter
	DATA	0x78,0x3A,0xC5,0x37	; 0.00285
	DATA	0x7B,0x83,0xA2,0x5C	;-0.0160686
	DATA	0x7C,0x2E,0xDD,0x4D	; 0.0426915
	DATA	0x7D,0x99,0xB0,0x1E	;-0.0750429
	DATA	0x7D,0x59,0xED,0x24	; 0.106409
	DATA	0x7E,0x91,0x72,0x00	;-0.142036
	DATA	0x7E,0x4C,0xB9,0x73	; 0.199926
	DATA	0x7F,0xAA,0xAA,0x53	;-0.333331

;##	DATA	0x08			; counter
;##	DATA	0x78,0x3B,0xD7,0x4A	; 1/17
;##	DATA	0x7B,0x84,0x6E,0x02	;-1/15
;##	DATA	0x7C,0x2F,0xC1,0xFE	; 1/13
;##	DATA	0x7D,0x9A,0x31,0x74	;-1/11
;##	DATA	0x7D,0x5A,0x3D,0x84	; 1/9
;##	DATA	0x7E,0x91,0x7F,0xC8	;-1/7
;##	DATA	0x7E,0x4C,0xBB,0xE4	; 1/5
;##	DATA	0x7F,0xAA,0xAA,0x6C	;-1/3

Lab_1D96	= *+1			; 0x00,0x00 used for undefined variables
==Lab_259C==
	DATA	0x81,0x00,0x00,0x00	; 1.000000, used for INC
==Lab_2AFD==
	DATA	0x81,0x80,0x00,0x00	; -1.00000, used for DEC. must be on the same page as +1.00

					; misc constants
==Lab_1DF7==
	DATA	0x90			;-32768 (uses first three bytes from 0.5)
==Lab_2A96==
	DATA	0x80,0x00,0x00,0x00	; 0.5
==Lab_2C80==
	DATA	0x7F,0x00,0x00,0x00	; 0.25
==Lab_26B5==
	DATA	0x84,0x20,0x00,0x00	; 10.0000 divide by 10 constant

; This table is used in converting numbers to ASCII.

==Lab_2A9A==
Lab_2A9B = Lab_2A9A+1
Lab_2A9C = Lab_2A9B+1
	DATA	0xFE,0x79,0x60		; -100000
	DATA	0x00,0x27,0x10		; 10000
	DATA	0xFF,0xFC,0x18		; -1000
	DATA	0x00,0x00,0x64		; 100
	DATA	0xFF,0xFF,0xF6		; -10
	DATA	0x00,0x00,0x01		; 1

==Lab_CTBL==
	DATA	Lab_END-1		; END
	DATA	Lab_FOR-1		; FOR
	DATA	Lab_NEXT-1		; NEXT
	DATA	Lab_DATA-1		; DATA
	DATA	Lab_INPUT-1		; INPUT
	DATA	Lab_DIM-1		; DIM
	DATA	Lab_READ-1		; READ
	DATA	Lab_LET-1		; LET
	DATA	Lab_DEC-1		; DEC			new command
	DATA	Lab_GOTO-1		; GOTO
	DATA	Lab_RUN-1		; RUN
	DATA	Lab_IF-1		; IF
	DATA	Lab_RESTORE-1	; RESTORE		modified command
	DATA	Lab_GOSUB-1		; GOSUB
	DATA	Lab_RETIRQ-1	; RETIRQ		new command
	DATA	Lab_RETNMI-1	; RETNMI		new command
	DATA	Lab_RETURN-1	; RETURN
	DATA	Lab_REM-1		; REM
	DATA	Lab_STOP-1		; STOP
	DATA	Lab_ON-1		; ON			modified command
	DATA	Lab_NULL-1		; NULL		modified command
	DATA	Lab_INC-1		; INC			new command
	DATA	Lab_WAIT-1		; WAIT
	DATA	V_LOAD-1		; LOAD
	DATA	V_SAVE-1		; SAVE
	DATA	Lab_DEF-1		; DEF
	DATA	Lab_POKE-1		; POKE
	DATA	Lab_DOKE-1		; DOKE		new command
	DATA	Lab_CALL-1		; CALL		new command
	DATA	Lab_DO-1		; DO			new command
	DATA	Lab_LOOP-1		; LOOP		new command
	DATA	Lab_PRINT-1		; PRINT
	DATA	Lab_CONT-1		; CONT
	DATA	Lab_LIST-1		; LIST
	DATA	Lab_CLEAR-1		; CLEAR
	DATA	Lab_NEW-1		; NEW
	DATA	Lab_WDTH-1		; WIDTH		new command
	DATA	Lab_GET-1		; GET			new command
	DATA	Lab_SWAP-1		; SWAP		new command
	DATA	Lab_BITSET-1	; BITSET		new command
	DATA	Lab_BITCLR-1	; BITCLR		new command
	DATA	Lab_IRQ-1		; IRQ			new command
	DATA	Lab_NMI-1		; NMI			new command

; function pre process routine table

==Lab_FTPL==
Lab_FTPM	= Lab_FTPL+0x01
	DATA	Lab_PPFN-1		; SGN(n)	process numeric expression in ()
	DATA	Lab_PPFN-1		; INT(n)		"
	DATA	Lab_PPFN-1		; ABS(n)		"
	DATA	Lab_EVEZ-1		; USR(x)	process any expression
	DATA	Lab_1BF7-1		; FRE(x)		"
	DATA	Lab_1BF7-1		; POS(x)		"
	DATA	Lab_PPFN-1		; SQR(n)	process numeric expression in ()
	DATA	Lab_PPFN-1		; RND(n)		"
	DATA	Lab_PPFN-1		; LOG(n)		"
	DATA	Lab_PPFN-1		; EXP(n)		"
	DATA	Lab_PPFN-1		; COS(n)		"
	DATA	Lab_PPFN-1		; SIN(n)		"
	DATA	Lab_PPFN-1		; TAN(n)		"
	DATA	Lab_PPFN-1		; ATN(n)		"
	DATA	Lab_PPFN-1		; PEEK(n)		"
	DATA	Lab_PPFN-1		; DEEK(n)		"
	DATA	0x0000			; SADD()	none
	DATA	Lab_PPFS-1		; LEN(0x)	process string expression in ()
	DATA	Lab_PPFN-1		; STR0x(n)	process numeric expression in ()
	DATA	Lab_PPFS-1		; VAL(0x)	process string expression in ()
	DATA	Lab_PPFS-1		; ASC(0x)		"
	DATA	Lab_PPFS-1		; UCASE0x(0x)		"
	DATA	Lab_PPFS-1		; LCASE0x(0x)		"
	DATA	Lab_PPFN-1		; CHR0x(n)	process numeric expression in ()
	DATA	Lab_BHSS-1		; HEX0x(n)		"
	DATA	Lab_BHSS-1		; BIN0x(n)		"
	DATA	0x0000			; BITTST()	none
	DATA	Lab_MMPP-1		; MAX()	process numeric expression
	DATA	Lab_MMPP-1		; MIN()		"
	DATA	Lab_PPBI-1		; PI		advance pointer
	DATA	Lab_PPBI-1		; TWOPI		"
	DATA	0x0000			; VARPTR()	none
	DATA	Lab_LRMS-1		; LEFT0x()	process string expression
	DATA	Lab_LRMS-1		; RIGHT0x()		"
	DATA	Lab_LRMS-1		; MID0x()		"

; action addresses for functions

==Lab_FTBL==
Lab_FTBM	= Lab_FTBL+0x01
	DATA	Lab_SGN-1		; SGN()
	DATA	Lab_INT-1		; INT()
	DATA	Lab_ABS-1		; ABS()
	DATA	Lab_USR-1		; USR()
	DATA	Lab_FRE-1		; FRE()
	DATA	Lab_POS-1		; POS()
	DATA	Lab_SQR-1		; SQR()
	DATA	Lab_RND-1		; RND()		modified function
	DATA	Lab_LOG-1		; LOG()
	DATA	Lab_EXP-1		; EXP()
	DATA	Lab_COS-1		; COS()
	DATA	Lab_SIN-1		; SIN()
	DATA	Lab_TAN-1		; TAN()
	DATA	Lab_ATN-1		; ATN()
	DATA	Lab_PEEK-1		; PEEK()
	DATA	Lab_DEEK-1		; DEEK()		new function
	DATA	Lab_SADD-1		; SADD()		new function
	DATA	Lab_LENS-1		; LEN()
	DATA	Lab_STRS-1		; STR0x()
	DATA	Lab_VAL-1		; VAL()
	DATA	Lab_ASC-1		; ASC()
	DATA	Lab_UCASE-1		; UCASE0x()		new function
	DATA	Lab_LCASE-1		; LCASE0x()		new function
	DATA	Lab_CHRS-1		; CHR0x()
	DATA	Lab_HEXS-1		; HEX0x()		new function
	DATA	Lab_BINS-1		; BIN0x()		new function
	DATA	Lab_BTST-1		; BITTST()		new function
	DATA	Lab_MAX-1		; MAX()		new function
	DATA	Lab_MIN-1		; MIN()		new function
	DATA	Lab_PI-1		; PI			new function
	DATA	Lab_TWOPI-1		; TWOPI		new function
	DATA	Lab_VARPTR-1	; VARPTR()		new function
	DATA	Lab_LEFT-1		; LEFT0x()
	DATA	Lab_RIGHT-1		; RIGHT0x()
	DATA	Lab_MIDS-1		; MID0x()

; hierarchy and action addresses for operator

==Lab_OPPT==
	DATA	0x79			; +
	DATA	Lab_ADD-1
	DATA	0x79			; -
	DATA	Lab_SUBTRACT-1
	DATA	0x7B			; *
	DATA	Lab_MULTIPLY-1
	DATA	0x7B			; /
	DATA	Lab_DIVIDE-1
	DATA	0x7F			; ^
	DATA	Lab_POWER-1
	DATA	0x50			; AND
	DATA	Lab_AND-1
	DATA	0x46			; EOR			new operator
	DATA	Lab_EOR-1
	DATA	0x46			; OR
	DATA	Lab_OR-1
	DATA	0x56			; >>			new operator
	DATA	Lab_RSHIFT-1
	DATA	0x56			; <<			new operator
	DATA	Lab_LSHIFT-1
	DATA	0x7D			; >
	DATA	Lab_GTHAN-1
	DATA	0x5A			; =
	DATA	Lab_EQUAL-1
	DATA	0x64			; <
	DATA	Lab_LTHAN-1

; keywords start with ..
; this is the first character table and must be in alphabetic order

==Tab_1STC==
	DATA	"*"
	DATA	"+"
	DATA	"-"
	DATA	"/"
	DATA	"<"
	DATA	"="
	DATA	">"
	DATA	"?"
	DATA	"A"
	DATA	"B"
	DATA	"C"
	DATA	"D"
	DATA	"E"
	DATA	"F"
	DATA	"G"
	DATA	"H"
	DATA	"I"
	DATA	"L"
	DATA	"M"
	DATA	"N"
	DATA	"O"
	DATA	"P"
	DATA	"R"
	DATA	"S"
	DATA	"T"
	DATA	"U"
	DATA	"V"
	DATA	"W"
	DATA	"^"
	DATA	0x00			; table terminator

; pointers to keyword tables

==Tab_CHRT==
	DATA	Tab_STAR		; table for "*"
	DATA	Tab_PLUS		; table for "+"
	DATA	Tab_MNUS		; table for "-"
	DATA	Tab_SLAS		; table for "/"
	DATA	Tab_LESS		; table for "<"
	DATA	Tab_EQUL		; table for "="
	DATA	Tab_MORE		; table for ">"
	DATA	Tab_QEST		; table for "?"
	DATA	Tab_ASCA		; table for "A"
	DATA	Tab_ASCB		; table for "B"
	DATA	Tab_ASCC		; table for "C"
	DATA	Tab_ASCD		; table for "D"
	DATA	Tab_ASCE		; table for "E"
	DATA	Tab_ASCF		; table for "F"
	DATA	Tab_ASCG		; table for "G"
	DATA	Tab_ASCH		; table for "H"
	DATA	Tab_ASCI		; table for "I"
	DATA	Tab_ASCL		; table for "L"
	DATA	Tab_ASCM		; table for "M"
	DATA	Tab_ASCN		; table for "N"
	DATA	Tab_ASCO		; table for "O"
	DATA	Tab_ASCP		; table for "P"
	DATA	Tab_ASCR		; table for "R"
	DATA	Tab_ASCS		; table for "S"
	DATA	Tab_ASCT		; table for "T"
	DATA	Tab_ASCU		; table for "U"
	DATA	Tab_ASCV		; table for "V"
	DATA	Tab_ASCW		; table for "W"
	DATA	Tab_POWR		; table for "^"

; tables for each start character, note if a longer keyword with the same start
; letters as a shorter one exists then it must come first, else the list is in
; alphabetical order as follows ..

; [keyword,token
; [keyword,token]]
; end marker (#0x00)

==Tab_STAR==
	DATA tk_MUL,0x00		; *
==Tab_PLUS==
	DATA tk_PLUS,0x00		; +
==Tab_MNUS==
	DATA tk_MINUS,0x00	; -
==Tab_SLAS==
	DATA tk_DIV,0x00		; /
==Tab_LESS==
==Lbb_LSHIFT==
	DATA	"<"
	DATA	tk_LSHIFT	; <<	note - "<<" must come before "<"
	DATA tk_LT			; <
	DATA	0x00
==Tab_EQUL==
	DATA tk_EQUAL,0x00	; =
==Tab_MORE==
==Lbb_RSHIFT==
	DATA	">"
	DATA	tk_RSHIFT	; >>	note - ">>" must come before ">"
	DATA tk_GT			; >
	DATA	0x00
==Tab_QEST==
	DATA tk_PRINT,0x00	; ?
==Tab_ASCA==
==Lbb_ABS==
	DATA	"BS("
	DATA	tk_ABS	; ABS(
==Lbb_AND==
	DATA	"ND"
	DATA	tk_AND		; AND
==Lbb_ASC==
	DATA	"SC("
	DATA	tk_ASC	; ASC(
==Lbb_ATN==
	DATA	"TN("
	DATA	tk_ATN	; ATN(
	DATA	0x00
==Tab_ASCB==
==Lbb_BINS==
	DATA	"IN0x("
	DATA	tk_BINS	; BIN0x(
==Lbb_BITCLR==
	DATA	"ITCLR"
	DATA	tk_BITCLR	; BITCLR
==Lbb_BITSET==
	DATA	"ITSET"
	DATA	tk_BITSET	; BITSET
==Lbb_BITTST==
	DATA	"ITTST("
	DATA	tk_BITTST
					; BITTST(
	DATA	0x00
==Tab_ASCC==
==Lbb_CALL==
	DATA	"ALL"
	DATA	tk_CALL	; CALL
==Lbb_CHRS==
	DATA	"HR0x("
	DATA	tk_CHRS	; CHR0x(
==Lbb_CLEAR==
	DATA	"LEAR"
	DATA	tk_CLEAR	; CLEAR
==Lbb_CONT==
	DATA	"ONT"
	DATA	tk_CONT	; CONT
==Lbb_COS==
	DATA	"OS("
	DATA	tk_COS	; COS(
	DATA	0x00
==Tab_ASCD==
==Lbb_DATA==
	DATA	"ATA"
	DATA	tk_DATA	; DATA
==Lbb_DEC==
	DATA	"EC"
	DATA	tk_DEC		; DEC
==Lbb_DEEK==
	DATA	"EEK("
	DATA	tk_DEEK	; DEEK(
==Lbb_DEF==
	DATA	"EF"
	DATA	tk_DEF		; DEF
==Lbb_DIM==
	DATA	"IM"
	DATA	tk_DIM		; DIM
==Lbb_DOKE==
	DATA	"OKE"
	DATA	tk_DOKE	; DOKE note - "DOKE" must come before "DO"
==Lbb_DO==
	DATA	"O"
	DATA	tk_DO		; DO
	DATA	0x00
==Tab_ASCE==
==Lbb_ELSE==
	DATA	"LSE"
	DATA	tk_ELSE	; ELSE
==Lbb_END==
	DATA	"ND"
	DATA	tk_END		; END
==Lbb_EOR==
	DATA	"OR"
	DATA	tk_EOR		; EOR
==Lbb_EXP==
	DATA	"XP("
	DATA	tk_EXP	; EXP(
	DATA	0x00
==Tab_ASCF==
==Lbb_FN==
	DATA	"N"
	DATA	tk_FN		; FN
==Lbb_FOR==
	DATA	"OR"
	DATA	tk_FOR		; FOR
==Lbb_FRE==
	DATA	"RE("
	DATA	tk_FRE	; FRE(
	DATA	0x00
==Tab_ASCG==
==Lbb_GET==
	DATA	"ET"
	DATA	tk_GET		; GET
==Lbb_GOSUB==
	DATA	"OSUB"
	DATA	tk_GOSUB	; GOSUB
==Lbb_GOTO==
	DATA	"OTO"
	DATA	tk_GOTO	; GOTO
	DATA	0x00
==Tab_ASCH==
==Lbb_HEXS==
	DATA	"EX0x("
	DATA	tk_HEXS	; HEX0x(
	DATA	0x00
==Tab_ASCI==
==Lbb_IF==
	DATA	"F"
	DATA	tk_IF		; IF
==Lbb_INC==
	DATA	"NC"
	DATA	tk_INC		; INC
==Lbb_INPUT==
	DATA	"NPUT"
	DATA	tk_INPUT	; INPUT
==Lbb_INT==
	DATA	"NT("
	DATA	tk_INT	; INT(
==Lbb_IRQ==
	DATA	"RQ"
	DATA	tk_IRQ		; IRQ
	DATA	0x00
==Tab_ASCL==
==Lbb_LCASES==
	DATA	"CASE0x("
	DATA	tk_LCASES
					; LCASE0x(
==Lbb_LEFTS==
	DATA	"EFT0x("
	DATA	tk_LEFTS	; LEFT0x(
==Lbb_LEN==
	DATA	"EN("
	DATA	tk_LEN	; LEN(
==Lbb_LET==
	DATA	"ET"
	DATA	tk_LET		; LET
==Lbb_LIST==
	DATA	"IST"
	DATA	tk_LIST	; LIST
==Lbb_LOAD==
	DATA	"OAD"
	DATA	tk_LOAD	; LOAD
==Lbb_LOG==
	DATA	"OG("
	DATA	tk_LOG	; LOG(
==Lbb_LOOP==
	DATA	"OOP"
	DATA	tk_LOOP	; LOOP
	DATA	0x00
==Tab_ASCM==
==Lbb_MAX==
	DATA	"AX("
	DATA	tk_MAX	; MAX(
==Lbb_MIDS==
	DATA	"ID0x("
	DATA	tk_MIDS	; MID0x(
==Lbb_MIN==
	DATA	"IN("
	DATA	tk_MIN	; MIN(
	DATA	0x00
==Tab_ASCN==
==Lbb_NEW==
	DATA	"EW"
	DATA	tk_NEW		; NEW
==Lbb_NEXT==
	DATA	"EXT"
	DATA	tk_NEXT	; NEXT
==Lbb_NMI==
	DATA	"MI"
	DATA	tk_NMI		; NMI
==Lbb_NOT==
	DATA	"OT"
	DATA	tk_NOT		; NOT
==Lbb_NULL==
	DATA	"ULL"
	DATA	tk_NULL	; NULL
	DATA	0x00
==Tab_ASCO==
==Lbb_OFF==
	DATA	"FF"
	DATA	tk_OFF		; OFF
==Lbb_ON==
	DATA	"N"
	DATA	tk_ON		; ON
==Lbb_OR==
	DATA	"R"
	DATA	tk_OR		; OR
	DATA	0x00
==Tab_ASCP==
==Lbb_PEEK==
	DATA	"EEK("
	DATA	tk_PEEK	; PEEK(
==Lbb_PI==
	DATA	"I"
	DATA	tk_PI		; PI
==Lbb_POKE==
	DATA	"OKE"
	DATA	tk_POKE	; POKE
==Lbb_POS==
	DATA	"OS("
	DATA	tk_POS	; POS(
==Lbb_PRINT==
	DATA	"RINT"
	DATA	tk_PRINT	; PRINT
	DATA	0x00
==Tab_ASCR==
==Lbb_READ==
	DATA	"EAD"
	DATA	tk_READ	; READ
==Lbb_REM==
	DATA	"EM"
	DATA	tk_REM		; REM
==Lbb_RESTORE==
	DATA	"ESTORE"
	DATA	tk_RESTORE
					; RESTORE
==Lbb_RETIRQ==
	DATA	"ETIRQ"
	DATA	tk_RETIRQ	; RETIRQ
==Lbb_RETNMI==
	DATA	"ETNMI"
	DATA	tk_RETNMI	; RETNMI
==Lbb_RETURN==
	DATA	"ETURN"
	DATA	tk_RETURN	; RETURN
==Lbb_RIGHTS==
	DATA	"IGHT0x("
	DATA	tk_RIGHTS
					; RIGHT0x(
==Lbb_RND==
	DATA	"ND("
	DATA	tk_RND	; RND(
==Lbb_RUN==
	DATA	"UN"
	DATA	tk_RUN		; RUN
	DATA	0x00
==Tab_ASCS==
==Lbb_SADD==
	DATA	"ADD("
	DATA	tk_SADD	; SADD(
==Lbb_SAVE==
	DATA	"AVE"
	DATA	tk_SAVE	; SAVE
==Lbb_SGN==
	DATA	"GN("
	DATA	tk_SGN	; SGN(
==Lbb_SIN==
	DATA	"IN("
	DATA	tk_SIN	; SIN(
==Lbb_SPC==
	DATA	"PC("
	DATA	tk_SPC	; SPC(
==Lbb_SQR==
	DATA	"QR("
	DATA	tk_SQR	; SQR(
==Lbb_STEP==
	DATA	"TEP"
	DATA	tk_STEP	; STEP
==Lbb_STOP==
	DATA	"TOP"
	DATA	tk_STOP	; STOP
==Lbb_STRS==
	DATA	"TR0x("
	DATA	tk_STRS	; STR0x(
==Lbb_SWAP==
	DATA	"WAP"
	DATA	tk_SWAP	; SWAP
	DATA	0x00
==Tab_ASCT==
==Lbb_TAB==
	DATA	"AB("
	DATA	tk_TAB	; TAB(
==Lbb_TAN==
	DATA	"AN("
	DATA	tk_TAN	; TAN(
==Lbb_THEN==
	DATA	"HEN"
	DATA	tk_THEN	; THEN
==Lbb_TO==
	DATA	"O"
	DATA	tk_TO		; TO
==Lbb_TWOPI==
	DATA	"WOPI"
	DATA	tk_TWOPI	; TWOPI
	DATA	0x00
==Tab_ASCU==
==Lbb_UCASES==
	DATA	"CASE0x("
	DATA	tk_UCASES
					; UCASE0x(
==Lbb_UNTIL==
	DATA	"NTIL"
	DATA	tk_UNTIL	; UNTIL
==Lbb_USR==
	DATA	"SR("
	DATA	tk_USR	; USR(
	DATA	0x00
==Tab_ASCV==
==Lbb_VAL==
	DATA	"AL("
	DATA	tk_VAL	; VAL(
==Lbb_VPTR==
	DATA	"ARPTR("
	DATA	tk_VPTR	; VARPTR(
	DATA	0x00
==Tab_ASCW==
==Lbb_WAIT==
	DATA	"AIT"
	DATA	tk_WAIT	; WAIT
==Lbb_WHILE==
	DATA	"HILE"
	DATA	tk_WHILE	; WHILE
==Lbb_WIDTH==
	DATA	"IDTH"
	DATA	tk_WIDTH	; WIDTH
	DATA	0x00
==Tab_POWR==
	DATA	tk_POWER,0x00	; ^

; new decode table for LIST
; Table is ..
; byte - keyword length, keyword first character
; word - pointer to rest of keyword from dictionary

; note if length is 1 then the pointer is ignored

==Lab_KEYT==
	DATA	"\03E"
	DATA	Lbb_END		; END
	DATA	"\03F"
	DATA	Lbb_FOR		; FOR
	DATA	"\04N"
	DATA	Lbb_NEXT		; NEXT
	DATA	"\04D"
	DATA	Lbb_DATA		; DATA
	DATA	"\05I"
	DATA	Lbb_INPUT		; INPUT
	DATA	"\03D"
	DATA	Lbb_DIM		; DIM
	DATA	"\04R"
	DATA	Lbb_READ		; READ
	DATA	"\03L"
	DATA	Lbb_LET		; LET
	DATA	"\03D"
	DATA	Lbb_DEC		; DEC
	DATA	"\04G"
	DATA	Lbb_GOTO		; GOTO
	DATA	"\03R"
	DATA	Lbb_RUN		; RUN
	DATA	"\02I"
	DATA	Lbb_IF		; IF
	DATA	"\07R"
	DATA	Lbb_RESTORE		; RESTORE
	DATA	"\05G"
	DATA	Lbb_GOSUB		; GOSUB
	DATA	"\06R"
	DATA	Lbb_RETIRQ		; RETIRQ
	DATA	"\06R"
	DATA	Lbb_RETNMI		; RETNMI
	DATA	"\06R"
	DATA	Lbb_RETURN		; RETURN
	DATA	"\03R"
	DATA	Lbb_REM		; REM
	DATA	"\04S"
	DATA	Lbb_STOP		; STOP
	DATA	"\02O"
	DATA	Lbb_ON		; ON
	DATA	"\04N"
	DATA	Lbb_NULL		; NULL
	DATA	"\03I"
	DATA	Lbb_INC		; INC
	DATA	"\04W"
	DATA	Lbb_WAIT		; WAIT
	DATA	"\04L"
	DATA	Lbb_LOAD		; LOAD
	DATA	"\04S"
	DATA	Lbb_SAVE		; SAVE
	DATA	"\03D"
	DATA	Lbb_DEF		; DEF
	DATA	"\04P"
	DATA	Lbb_POKE		; POKE
	DATA	"\04D"
	DATA	Lbb_DOKE		; DOKE
	DATA	"\04C"
	DATA	Lbb_CALL		; CALL
	DATA	"\02D"
	DATA	Lbb_DO		; DO
	DATA	"\04L"
	DATA	Lbb_LOOP		; LOOP
	DATA	"\05P"
	DATA	Lbb_PRINT		; PRINT
	DATA	"\04C"
	DATA	Lbb_CONT		; CONT
	DATA	"\04L"
	DATA	Lbb_LIST		; LIST
	DATA	"\05C"
	DATA	Lbb_CLEAR		; CLEAR
	DATA	"\03N"
	DATA	Lbb_NEW		; NEW
	DATA	"\05W"
	DATA	Lbb_WIDTH		; WIDTH
	DATA	"\03G"
	DATA	Lbb_GET		; GET
	DATA	"\04S"
	DATA	Lbb_SWAP		; SWAP
	DATA	"\06B"
	DATA	Lbb_BITSET		; BITSET
	DATA	"\06B"
	DATA	Lbb_BITCLR		; BITCLR
	DATA	"\03I"
	DATA	Lbb_IRQ		; IRQ
	DATA	"\03N"
	DATA	Lbb_NMI		; NMI

; secondary commands (can't start a statement)

	DATA	"\04T"
	DATA	Lbb_TAB		; TAB
	DATA	"\04E"
	DATA	Lbb_ELSE		; ELSE
	DATA	"\02T"
	DATA	Lbb_TO		; TO
	DATA	"\02F"
	DATA	Lbb_FN		; FN
	DATA	"\04S"
	DATA	Lbb_SPC		; SPC
	DATA	"\04T"
	DATA	Lbb_THEN		; THEN
	DATA	"\03N"
	DATA	Lbb_NOT		; NOT
	DATA	"\04S"
	DATA	Lbb_STEP		; STEP
	DATA	"\05U"
	DATA	Lbb_UNTIL		; UNTIL
	DATA	"\05W"
	DATA	Lbb_WHILE		; WHILE
	DATA	"\03O"
	DATA	Lbb_OFF		; OFF

; opperators

	DATA	"\01+"
	DATA	0x0000			; +
	DATA	"\01-"
	DATA	0x0000			; -
	DATA	"\01*"
	DATA	0x0000			; *
	DATA	"\01/"
	DATA	0x0000			; /
	DATA	"\01^"
	DATA	0x0000			; ^
	DATA	"\03A"
	DATA	Lbb_AND		; AND
	DATA	"\03E"
	DATA	Lbb_EOR		; EOR
	DATA	"\02O"
	DATA	Lbb_OR		; OR
	DATA	"\02>"
	DATA	Lbb_RSHIFT		; >>
	DATA	"\02<"
	DATA	Lbb_LSHIFT		; <<
	DATA	"\01>"
	DATA	0x0000			; >
	DATA	"\01="
	DATA	0x0000			; =
	DATA	"\01<"
	DATA	0x0000			; <

; functions

	DATA	"\04S"			;
	DATA	Lbb_SGN		; SGN
	DATA	"\04I"			;
	DATA	Lbb_INT		; INT
	DATA	"\04A"			;
	DATA	Lbb_ABS		; ABS
	DATA	"\04U"			;
	DATA	Lbb_USR		; USR
	DATA	"\04F"			;
	DATA	Lbb_FRE		; FRE
	DATA	"\04P"			;
	DATA	Lbb_POS		; POS
	DATA	"\04S"			;
	DATA	Lbb_SQR		; SQR
	DATA	"\04R"			;
	DATA	Lbb_RND		; RND
	DATA	"\04L"			;
	DATA	Lbb_LOG		; LOG
	DATA	"\04E"			;
	DATA	Lbb_EXP		; EXP
	DATA	"\04C"			;
	DATA	Lbb_COS		; COS
	DATA	"\04S"			;
	DATA	Lbb_SIN		; SIN
	DATA	"\04T"			;
	DATA	Lbb_TAN		; TAN
	DATA	"\04A"			;
	DATA	Lbb_ATN		; ATN
	DATA	"\05P"			;
	DATA	Lbb_PEEK		; PEEK
	DATA	"\05D"			;
	DATA	Lbb_DEEK		; DEEK
	DATA	"\05S"			;
	DATA	Lbb_SADD		; SADD
	DATA	"\04L"			;
	DATA	Lbb_LEN		; LEN
	DATA	"\05S"			;
	DATA	Lbb_STRS		; STR0x
	DATA	"\04V"			;
	DATA	Lbb_VAL		; VAL
	DATA	"\04A"			;
	DATA	Lbb_ASC		; ASC
	DATA	"\07U"			;
	DATA	Lbb_UCASES		; UCASE0x
	DATA	"\07L"			;
	DATA	Lbb_LCASES		; LCASE0x
	DATA	"\05C"			;
	DATA	Lbb_CHRS		; CHR0x
	DATA	"\05H"			;
	DATA	Lbb_HEXS		; HEX0x
	DATA	"\05B"			;
	DATA	Lbb_BINS		; BIN0x
	DATA	"\07B"			;
	DATA	Lbb_BITTST		; BITTST
	DATA	"\04M"			;
	DATA	Lbb_MAX		; MAX
	DATA	"\04M"			;
	DATA	Lbb_MIN		; MIN
	DATA	"\02P"			;
	DATA	Lbb_PI		; PI
	DATA	"\05T"			;
	DATA	Lbb_TWOPI		; TWOPI
	DATA	"\07V"			;
	DATA	Lbb_VPTR		; VARPTR
	DATA	"\06L"			;
	DATA	Lbb_LEFTS		; LEFT0x
	DATA	"\07R"			;
	DATA	Lbb_RIGHTS		; RIGHT0x
	DATA	"\05M"			;
	DATA	Lbb_MIDS		; MID0x

; BASIC messages, mostly error messages

==Lab_BAER==
	DATA	Err_NF		;0x00 NEXT without FOR
	DATA	Err_SN		;0x02 syntax
	DATA	Err_RG		;0x04 RETURN without GOSUB
	DATA	Err_OD		;0x06 out of data
	DATA	Err_FC		;0x08 function call
	DATA	Err_OV		;0x0A overflow
	DATA	Err_OM		;0x0C out of memory
	DATA	Err_US		;0x0E undefined statement
	DATA	Err_BS		;0x10 array bounds
	DATA	Err_DD		;0x12 double dimension array
	DATA	Err_D0		;0x14 divide by 0
	DATA	Err_ID		;0x16 illegal direct
	DATA	Err_TM		;0x18 type mismatch
	DATA	Err_LS		;0x1A long string
	DATA	Err_ST		;0x1C string too complex
	DATA	Err_CN		;0x1E continue error
	DATA	Err_UF		;0x20 undefined function
	DATA	Err_LD		;0x22 LOOP without DO

; I may implement these two errors to force definition of variables and
; dimensioning of arrays before use.

;	DATA Err_UV		;0x24 undefined variable

; the above error has been tested and works (see code and comments below Lab_1D8B)

;	DATA Err_UA		;0x26 undimensioned array

Err_NF	DATA	"NEXT without FOR\00"
Err_SN	DATA	"Syntax\00"
Err_RG	DATA	"RETURN without GOSUB\00"
Err_OD	DATA	"Out of DATA\00"
Err_FC	DATA	"Function call\00"
Err_OV	DATA	"Overflow\00"
Err_OM	DATA	"Out of memory\00"
Err_US	DATA	"Undefined statement\00"
Err_BS	DATA	"Array bounds\00"
Err_DD	DATA	"Double dimension\00"
Err_D0	DATA	"Divide by zero\00"
Err_ID	DATA	"Illegal direct\00"
Err_TM	DATA	"Type mismatch\00"
Err_LS	DATA	"String too long\00"
Err_ST	DATA	"String too complex\00"
Err_CN	DATA	"Can't continue\00"
Err_UF	DATA	"Undefined function\00"
Err_LD	DATA	"LOOP without DO\00"

;Err_UV	DATA	"Undefined variable\00"

; the above error has been tested and works (see code and comments below Lab_1D8B)

;Err_UA	DATA	"Undimensioned array\00"

Lab_BMSG	DATA	"\0D\0ABreak\00"
Lab_EMSG	DATA	" Error\00"
Lab_LMSG	DATA	" in line \00"
Lab_RMSG	DATA	"\0D\0AReady\0D\0A\00"

Lab_IMSG	DATA	" Extra ignored\0D\0A\00"
Lab_REDO	DATA	" Redo from start\0D\0A\00"

==AA_end_basic==







==Reset==
	CLD				; clear decimal mode
	LDX	#$FF		; empty stack
	TXS





==Irq==

==End_code==


LAB_mess
	.byte	$0D,$0A,"6502 EhBASIC [C]old/[W]arm ?",$00
					; sign on string

; system vectors

@$FFFA
	DATA	Irq		; NMI vector
	DATA	Reset		; RESET vector
	DATA	Irq		; IRQ vector