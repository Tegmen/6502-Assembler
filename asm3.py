#OPCODES
OPC_TABLE = {
	'ADC':[  -1,  -1,0x69,0x6d,0x7d,0x79,0x65,0x75,  -1,  -1,0x61,0x71,  -1],
	'AND':[  -1,  -1,0x29,0x2d,0x3d,0x39,0x25,0x35,  -1,  -1,0x21,0x31,  -1],
	'ASL':[  -1,0x0a,  -1,0x0e,0x1e,  -1,0x06,0x16,  -1,  -1,  -1,  -1,  -1],
	'BCC':[  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,0x90],
	'BCS':[  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,0xb0],
	'BEQ':[  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,0xf0],
	'BIT':[  -1,  -1,  -1,0x2c,  -1,  -1,0x24,  -1,  -1,  -1,  -1,  -1,  -1],
	'BMI':[  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,0x30],
	'BNE':[  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,0xd0],
	'BPL':[  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,0x10],
	'BRK':[0x00,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'BVC':[  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,0x50],
	'BVS':[  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,0x70],
	'CLC':[0x18,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'CLD':[0xd8,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'CLI':[0x58,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'CLV':[0xb8,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'CMP':[  -1,  -1,0xc9,0xcd,0xdd,0xd9,0xc5,0xd5,  -1,  -1,0xc1,0xd1,  -1],
	'CPX':[  -1,  -1,0xe0,0xec,  -1,  -1,0xe4,  -1,  -1,  -1,  -1,  -1,  -1],
	'CPY':[  -1,  -1,0xc0,0xcc,  -1,  -1,0xc4,  -1,  -1,  -1,  -1,  -1,  -1],
	'DEC':[  -1,  -1,  -1,0xce,0xde,  -1,0xc6,0xd6,  -1,  -1,  -1,  -1,  -1],
	'DEX':[0xca,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'DEY':[0x88,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'EOR':[  -1,  -1,0x49,0x4d,0x5d,0x59,0x45,0x55,  -1,  -1,0x41,0x51,  -1],
	'INC':[  -1,  -1,  -1,0xee,0xfe,  -1,0xe6,0xf6,  -1,  -1,  -1,  -1,  -1],
	'INX':[0xe8,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'INY':[0xc8,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'JMP':[  -1,  -1,  -1,0x4c,  -1,  -1,  -1,  -1,  -1,0x6c,  -1,  -1,  -1],
	'JSR':[  -1,  -1,  -1,0x20,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'LDA':[  -1,  -1,0xa9,0xad,0xbd,0xb9,0xa5,0xb5,  -1,  -1,0xa1,0xb1,  -1],
	'LDX':[  -1,  -1,0xa2,0xae,  -1,0xbe,0xa6,  -1,0xb6,  -1,  -1,  -1,  -1],
	'LDY':[  -1,  -1,0xa0,0xac,0xbc,  -1,0xa4,0xb4,  -1,  -1,  -1,  -1,  -1],
	'LSR':[  -1,0x4a,  -1,0x4e,0x5e,  -1,0x46,0x56,  -1,  -1,  -1,  -1,  -1],
	'NOP':[0xea,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'ORA':[  -1,  -1,0x09,0x0d,0x1d,0x19,0x05,0x15,  -1,  -1,0x01,0x11,  -1],
	'PHA':[0x48,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'PHP':[0x08,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'PLA':[0x68,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'PLP':[0x28,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'ROL':[  -1,0x2a,  -1,0x2e,0x3e,  -1,0x26,0x36,  -1,  -1,  -1,  -1,  -1],
	'ROR':[  -1,0x6a,  -1,0x6e,0x7e,  -1,0x66,0x76,  -1,  -1,  -1,  -1,  -1],
	'RTI':[0x40,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'RTS':[0x60,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'SBC':[  -1,  -1,0xe9,0xed,0xfd,0xf9,0xe5,0xf5,  -1,  -1,0xe1,0xf1,  -1],
	'SEC':[0x38,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'SED':[0xf8,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'SEI':[0x78,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'STA':[  -1,  -1,  -1,0x8d,0x9d,0x99,0x85,0x95,  -1,  -1,0x81,0x91,  -1],
	'STX':[  -1,  -1,  -1,0x8e,  -1,  -1,0x86,  -1,0x96,  -1,  -1,  -1,  -1],
	'STY':[  -1,  -1,  -1,0x8c,  -1,  -1,0x84,0x94,  -1,  -1,  -1,  -1,  -1],
	'TAX':[0xaa,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'TAY':[0xa8,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'TSX':[0xba,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'TXA':[0x8a,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'TXS':[0x9a,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1],
	'TYA':[0x98,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1]}

#SIZE OF ADDRESS MODES
ADDR_SIZE = [0,0,1,2,2,2,1,1,1,2,1,1,1]

#ADDRESS MODES
IMP=0	# OPC
ACC=1	# OPC A
IMM=2	# OPC #byte
ABS=3	# OPC word
ABX=4	# OPC word,X
ABY=5	# OPC word,Y
ZPG=6	# OPC byte
ZPX=7	# OPC byte,X
ZPY=8	# OPC byte,Y
IND=9	# OPC (word)
INX=10	# OPC (byte,x)
INY=11	# OPC (byte),y
REL=12	# OPC byte

def handle_opc(opc):
	if opc in OPC_TABLE:
		modes = OPC_TABLE[opc]
		if modes[IMP] != -1:
			write_fp(modes[IMP],OPC)
		else:
			addr = pop_token()
			if addr.upper() == 'A' or addr == '!eol':
				if modes[ACC] != -1:
					write_fp(modes[ACC],OPC)
				else:
					error('Missing address mode: '+opc)
			if re.compile('^#'+OPERAND+'+$').match(addr):
					write_fp(modes[IMM],OPC)
					write_fp(addr[1:],BYTE)
					
			elif re.compile('^'+OPERAND+'+$').match(addr):
				if OPC_TABLE[REL] != -1:
					write_fp(modes[REL],OPC)
					write_fp(addr,RELA)
				elif get_type(addr) == BYTE:
					write_fp(modes[ZPG],OPC)
					write_fp(addr,BYTE)
				else:
					write_fp(modes[ABS],OPC)
					write_fp(addr,LO)
					write_fp(addr,HI)
					
			elif re.compile('^'+OPERAND+'+\,[Xx]$').match(addr):
				if get_type(addr[:-2]) == BYTE:
					write_fp(modes[ZPX],OPC)
					write_fp(addr[:-2],BYTE)
				else:
					write_fp(modes[ABX],OPC)
					write_fp(addr[:-2],LO)
					write_fp(addr[:-2],HI)
					
			elif re.compile('^'+OPERAND+'+\,[Yy]$').match(addr):
				if get_type(addr[:-2]) == BYTE:
					write_fp(modes[ZPY],OPC)
					write_fp(addr[:-2],BYTE)
				else:
					write_fp(modes[ABY],OPC)
					write_fp(addr[:-2],LO)
					write_fp(addr[:-2],HI)
					
			elif re.compile('^\('+OPERAND+'+\,[Xx]\)$').match(addr):
				write_fp(modes[INX],OPC)
				write_fp(addr[1:-3],BYTE)
					
			elif re.compile('^\('+OPERAND+'+\)\,[Yy]$').match(addr):
				if modes[INY] != -1:
					write_fp(modes[INY],OPC)
					write_fp(addr[1:-3],BYTE)
			
			elif re.compile('^\('+OPERANDo+'\)$').match(addr) and OPC_TABLE[IND] != -1:
				write_fp(modes[IND],OPC)
				write_fp(addr[1:-1],LO)
				write_fp(addr[1:-1],HI)		
	else:
		error('Unknown Operator: '+opc)
		
		
		
	
	
	
class Lexer:
	def __init__(self, sourceDir):
		self.lines = open(sourceDir, 'r').read().splitlines()
		self.line = 1
	def next_token():
	
	def ge
		
		
		
lex = Lexer("test.txt")
print(lexer.lines)
