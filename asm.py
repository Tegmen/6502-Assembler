import sys
import shlex
import re


#TYPES
BYTE = 0
WORD = 1
LOW = 2
HIGH = 3
RELATIVE = 4

TYPES = ['BYTE','WORD','LO','HI','REL']

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

#SIZE OF ADDRESS MODES
ADDR_SIZE = [0,0,1,2,2,2,1,1,1,2,1,1,1]


# Pattern of a word
WP = r'(?:(?:[A-Z][A-Za-z0-9_]*)|(?:0x[0-9a-fA-F]{4})|(?:0b[01]{16})|([0-9]{1,4}|[1-5][0-9]{4}|6[0-4][0-9]{3}|65[0-4][0-9]{2}|655[0-2][0-9]|6553[0-5]))'


#Pattern of a byte
BP = r'(?:(?:[a-z][A-Za-z0-9_]*)|(?:0x[0-9a-fA-F]{2})|(?:0b[01]{8})|(?:(?:'+WP+r')\.(?:HI|LO))|(?:1?[0-9]?[0-9])|(?:2(?:5[0-5])|(?:[0-4][0-9])))'

COMB_PATTERN = re.compile(r'^\S+[+-]\S+$')
BYTE_PATTERN = re.compile('^'+BP+'(?:[+-]'+BP+')*$')   
WORD_PATTERN = re.compile('^'+WP+'(?:[+-]'+WP+')*$')
BYTE_IDENT_PATTERN = re.compile('^[a-z][A-Za-z0-9_]*$')
WORD_IDENT_PATTERN = re.compile('^[A-Z][A-Za-z0-9_]*$')
ARRAY_PATTERN = re.compile(r'^([A-Za-z][A-Za-z0-9_]*)\[('+BP+r')\]$')
STRING_PATTERN = re.compile(r'^"[\s\S]*"$')
HEX_PATTERN = re.compile('^0x(?:[0-9A-Fa-f]{2})+$')
HEXFILE_PATTERN = re.compile(r'^[\w\/]+.hex')
BIN_PATTERN = re.compile('^0b(?:[01]{8})+$')
DEC_PATTERN = re.compile('^(?:([1-9][0-9]*)|(?:0))$')
LO_PATTERN = re.compile('^(?:'+WP+').LO$')
HI_PATTERN = re.compile('^(?:'+WP+').HI$')
BRACKET_OP_PATTERN = re.compile(r'^(?:(?:BR,)?(?:(?:BR)|(?:RE)|(?:SK)))$')

ADDR_PATTERNS = [
	re.compile('$a'),
	re.compile('^([aA])$'),
	re.compile('^#('+BP+'(?:[+-]'+BP+')*'+')$'),
	re.compile('^((?:(?:BR,)?(?:(?:BR)|(?:RE)|(?:SK)))|'+WP+'(?:[+-]'+WP+')*'+')$'),
	re.compile('^('+WP+'(?:[+-]'+WP+')*'+'),[Xx]$'),
	re.compile('^('+WP+'(?:[+-]'+WP+')*'+'),[Yy]$'),
	re.compile('^('+BP+'(?:[+-]'+BP+')*'+')$'),
	re.compile('^('+BP+'(?:[+-]'+BP+')*'+'),[Xx]$'),
	re.compile('^('+BP+'(?:[+-]'+BP+')*'+'),[Yy]$'),
	re.compile(r'^\(('+WP+'(?:[+-]'+WP+')*'+r')\)$'),
	re.compile(r'^\(('+BP+'(?:[+-]'+BP+')*'+r'),[Xx]\)$'),
	re.compile(r'^\(('+BP+'(?:[+-]'+BP+')*'+r')\),[Yy]$'),
	re.compile('^((?:(?:BR,)?(?:(?:BR)|(?:RE)|(?:SK)))|'+BP+'(?:[+-]'+BP+')*'+'|'+WP+'(?:[+-]'+WP+')*'+')$')]

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
	
JUMPERS = [
	'ASL',
	'BCC',
	'BCS',
	'BEQ',
	'BMI',
	'BNE',
	'BPL',
	'BVC',
	'BVS',
	'JMP',
	'RTI',
	'RTS']

def handle_label_declaration(label):
	if label[-1] == ':':
		define_word(label[:-1],current_address)
		
	else:
		if last_opc in JUMPERS:
			define_word(label[2:-2],current_address)
		else:
			error('Label =='+label[2:-2]+'== reachable from last OPC: '+last_opc)
		
def handle_var(token):	
	global current_address
	if origin == 0:
		name = next_token()
		match = ARRAY_PATTERN.match(name)
		if match:
			name = match.group(1)
			length,b = get_int_value(match.group(2))
		else:
			length = 1
		if BYTE_IDENT_PATTERN.match(name):
			if current_address > 255:
				error('Byte variables must be at Zero Page: '+current_address)
			else:
				define_byte(name,current_address)
				current_address += length
			
		elif WORD_IDENT_PATTERN.match(name):
			define_word(name,current_address)
			current_address += length
		else:
			error('Identifier expected after VAR, "'+name+'" found')
	else:
		error('Variables cant be declared withing code. Must be BEFORE setting origin')
	
	
def handle_byte_identifier(var_name):
	token = next_token()
	if token == '=':
		var_value,isByte = get_int_value(next_token())
		if not isByte:
			error('Cant assign word to "'+var_name+'", byte expected')
		define_byte(var_name,var_value)
	else:
		error('Byte declaration, "=" expected, "'+token+" found")

def handle_word_identifier(var_name):
	token = next_token()
	if token == '=':
		var_value,isByte = get_int_value(next_token())
		if isByte:
			error('Cant assign byte to "'+var_name+'", word expected')
		define_word(var_name,var_value)
	else:
		error('Word declaration, "=" expected, "'+token+" found")

def handle_data(token):
	data = re.split(r', (?=(?:"[^"]*?(?: [^"]*)*))|, (?=[^",]+(?:,|$))', next_token())
	for d in data:
		if STRING_PATTERN.match(d):
			content = d[1:-1]
			c = 0;
			while c < len(content):
				if content[c] == '\\':
					write_byte_fp(int(content[c+1:c+3],16),BYTE)
					c += 3
				else:
					write_byte_fp(ord(content[c]),BYTE)
					c += 1
		elif HEXFILE_PATTERN.match(d):
			try:
				file = open(path+d, 'r').read()
				for b in range(0,len(file),2):
					write_byte_fp(int(file[b:b+2],16),BYTE)		
			except IOError:
				error('IO error for file '+path+d)
		
		elif HEX_PATTERN.match(d):
			c = 2
			while c < len(d):
				write_byte_fp(int(d[c:c+2],16),BYTE)
				c += 2
		elif WORD_PATTERN.match(d):
			write_byte_fp(d,LOW)
			write_byte_fp(d,HIGH)
		elif BYTE_PATTERN.match(d):
			write_byte_fp(d,BYTE)
		else:
			error('Unknown data: '+d)
		
def handle_bracket_open(token):
	brackets.append(Bracket(brackets[-1], current_address))

def handle_bracket_close(token):
	bracket = brackets.pop()
	define_word('BOP'+bracket.id,bracket.open_address)
	define_word('BCL'+bracket.id,current_address)

def handle_memory_location(token):
	global current_address
	location = next_token()
	if location == 'NPAGE':
		while (current_address % 256 > 0):
			write_byte_fp(0,BYTE)
	else:
		address,isByte = get_int_value(location)
		#if isByte:
		#	error("Invalid location "+location)
		if address >= current_address:
			while (address > current_address):
				write_byte_fp(0,BYTE)
		else:
			error('New address ('+str(address)+') cant be smaller than current address ('+str(current_address)+')')
		
def handle_origin(token):
	global current_address,origin
	if not next_token() == '=':
		error('Syntax error: "=" expected')
	location = next_token()
	address,isByte = get_int_value(location)
	if isByte:
		error('Invalid location for origin: '+location)
	elif current_address <= address and origin == 0:
		current_address = address
		origin = address
	else:
		error('ORIGIN must be set before the first OPC/ DATA')
	
def handle_opc(opc):
	global last_opc
	opc_modes = OPC_TABLE.get(opc)
	if opc_modes == None:
		error("Unknown Opcode")
	last_opc = opc
	if opc_modes[IMP] != -1:
		write_byte_fp(opc_modes[IMP],BYTE)
	else:
		addr = next_token()
		match = None
		for addres_mode,pattern in enumerate(ADDR_PATTERNS):
			if opc_modes[addres_mode] != -1:
				match = pattern.match(addr)
				if match:
					write_byte_fp(opc_modes[addres_mode],BYTE)
					addr = match.group(1)
					if BRACKET_OP_PATTERN.match(addr):
						addr = get_name_of_bracket(addr)	
					if ADDR_SIZE[addres_mode] == 0:
						break
					elif ADDR_SIZE[addres_mode] == 1:
						if addres_mode == REL:
							write_byte_fp(addr,RELATIVE)
						else:
							write_byte_fp(addr,BYTE)
						break
					else:
						write_byte_fp(addr,LOW)
						write_byte_fp(addr,HIGH)
						break
		if not match:
			error("Invalid addres mode "+addr+" for "+opc)
	
TOKEN_HANDLERS = [
	(re.compile(r'^VAR$'),handle_var),
	(re.compile(r'^DATA$'),handle_data),
	(re.compile(r'^ORIGIN$'),handle_origin),
	(re.compile(r'^==[A-Z][A-Za-z0-9_]*==$'),handle_label_declaration),
	(re.compile(r'^[A-Z][A-Za-z0-9_]*:$'),handle_label_declaration),
	(re.compile(r'^[a-z][A-Za-z0-9_]*$'),handle_byte_identifier),
	(re.compile(r'^[A-Z][A-Za-z0-9_]*$'),handle_word_identifier),
	(re.compile(r'^{$'),handle_bracket_open),
	(re.compile(r'^}$'),handle_bracket_close),
	(re.compile(r'@$'),handle_memory_location)
	]

def get_name_of_bracket(token):
	if token == 'BR':
		return 'BCL'+brackets[-1].id
	elif token == 'BR,BR':
		return 'BCL'+brackets[-2].id
	elif token == 'RE':
		return 'BOP'+brackets[-1].id
	elif token == 'BR,RE':
		return 'BOP'+brackets[-2].id
	elif token == 'SK':
		return 'BCL'+brackets[-1].id+'_'+str(brackets[-1].children+1)
	elif token == 'BR,SK':
		return 'BCL'+brackets[-2].id+'_'+str(brackets[-2].children+1)
	else:
		error_internal('Bracket behaviour '+token)
	
	
def error(msg):
	print('')
	print('Error on line {}: {}'.format(current_line,msg))
	print('')
	input("Press Enter to exit...")
	sys.exit()
	
def error_internal(msg):
	print('Error:')
	print('Second Pass: '+msg)
	

def define_byte(name, value):
	if value < 0 or value > 255:
		error('Value out of range for a byte: '+str(value))
	if name in constants:
		error('Cant define constant "'+name+'", already defined at line '+str(constants[name].defined_at))
	constants[name] = Constant(name, value, BYTE, current_line)
	const_log.write('BYTE: {:.<20}..{:02X}\n'.format(name,value))

def define_word(name, value):
	if value < 0 or value > 65535:
		error('Value out of range for a word: '+str(value))
	if name in constants:
		error('Cant define constant "'+name+'", already defined at line '+str(constants[name].defined_at))
	constants[name] = Constant(name, value, WORD, current_line)
	const_log.write('WORD: {:.<20}{:04X}\n'.format(name,value))

	
	
	
		
class Constant:
	def __init__(self, name, value, type, defined_at):
		self.name = name
		self.value = value
		self.type = type
		self.defined_at = defined_at

class Byte_fp:
	def __init__(self,value,type,source_line):
		self.value = value
		self.type = type
		self.source_line = source_line
	
class Bracket:
	def __init__(self, parent, open_address):
		if parent:
			parent.children += 1
			self.id = parent.id + '_'+str(parent.children)
		else:
			self.id = ''
		self.open_address = open_address
		self.children = 0

	
def write_byte_fp(value,type):
	global current_address
	memory_fp.append(Byte_fp(value,type,current_line))
	current_address += 1
	fp_log.write('{:<15} {:<4} {:>4}\n'.format(value,TYPES[type],current_line))
	
def write_byte_sp(value):
	global current_address
	if value >= 0 and value < 256:
		memory_sp.append(int(value))
		current_address += 1
	else:
		error_internal("!!!Value out of range: "+str(value)+'    '+str(current_address))
	
def get_int_value(token):
	if COMB_PATTERN.match(token):
		subtokens = re.split('([+-])',token)
		value,is_byte = get_int_value(subtokens[0])
		s = 2
		while s < len(subtokens):
			v,b = get_int_value(subtokens[s])
			if subtokens[s-1] == '+':
				value += v
			elif subtokens[s-1] == '-':
				value -= v
			else:
				error('Syntax error')
			is_byte = is_byte and b
			s += 2
		return (value,is_byte and value >= 0 and value <= 255) 	
	elif HEX_PATTERN.match(token) and len(token) <= 6:
		return (int(token,16), len(token)==4)
	elif BIN_PATTERN.match(token) and len(token) <=18:
		return (int(token,2), len(token)==10)
	elif DEC_PATTERN.match(token) and int(token) <= 65536:
		return (int(token), int(token) <= 255)
	elif BYTE_IDENT_PATTERN.match(token) or WORD_IDENT_PATTERN.match(token):
		if token in constants:
			return (constants[token].value, constants[token].type == BYTE)
		else:
			error('Constant "'+token+'" not defined')
	elif HI_PATTERN.match(token):
		val = get_int_value(token[:-3])[0]
		return val//256, True
	elif LO_PATTERN.match(token):
		val = get_int_value(token[:-3])[0]
		return val%256, True
	else:
		error_internal('cant get int value of "'+token+'"')

	
#Used to fixe shlex's broken lineno
def next_token():
	global current_line
	token = lexer.get_token()
	current_line = lexer.lineno		
	return token
	
def memory_as_hex():
	hex_string = ''
	for byte in memory_sp:
		hex_string += '{:02X}'.format(byte)
	return hex_string

def first_pass():
	token = next_token()
	while token != lexer.eof:
		if OPC_TABLE.get(token.upper()) != None:
			handle_opc(token.upper())
		else:
			token_handled = False
			for pattern,handle in TOKEN_HANDLERS:
				if pattern.match(token):
					handle(token)
					token_handled = True
					break
			if not token_handled:
				error('Syntax error: '+token)
		token = next_token()
	
	
def second_pass():
	global current_address,current_line
	current_address = origin
	
	for byte in memory_fp:
		current_line = byte.source_line
		if isinstance(byte.value,int):
			write_byte_sp(byte.value)
		elif byte.type == BYTE:
			val,is_byte = get_int_value(byte.value)
			write_byte_sp(val)
		elif byte.type == LOW:
			val,is_byte = get_int_value(byte.value)
			write_byte_sp(val%256)
		elif byte.type == HIGH:
			val,is_byte = get_int_value(byte.value)
			write_byte_sp(val//256)
		elif byte.type == RELATIVE:
			val,is_byte = get_int_value(byte.value)
			if is_byte:
				write_byte_sp(val)
			else:
				delta = val - current_address
				if delta < 0:
					if delta < -128:
						error('Relative jump too big from address {:04X} to {:04X} ({:5d})'.format(current_address,val,delta))
					else:
						write_byte_sp(delta+255)
				else:
					if delta > 127:
						error('Relative jump too big from address {:04X} to {:04X} ({:5d})'.format(current_address,val,delta))
					else:
						write_byte_sp(delta-1)
		else:
			error_internal('unknown byte')
			
def generate_listing():
	line_nr = 1
	line = ''
	for index,byte in enumerate(memory_fp):
		if line_nr < byte.source_line:
			while line_nr < byte.source_line:
				if (len(line) <= 14):
					listing.write('{:<14} |{}\n'.format(line,sourceList[line_nr-1]))
				else:
					listing.write('{:<12}.. |{}\n'.format(line[0:12],sourceList[line_nr-1]))
				line_nr += 1
				line = ''
			line += '{:04X}:'.format(index+origin)
		line += ' {:02X}'.format(memory_sp[index])
	if (len(line) <= 14):
		listing.write('{:<14} |{}\n'.format(line,sourceList[line_nr-1]))
	else:
		listing.write('{:<12}.. |{}\n'.format(line[0:12],sourceList[line_nr-1]))
	
		
# print(BRACKET_OP_PATTERN.match('BR,SK'))	
# sys.exit()		

path = re.search(r'^[\w\/]+/', sys.argv[1])
if path:
	path = path.group(0)
else:
	path = ''

	
	
source = open(sys.argv[1], 'r').read()
sourceList = source.replace('\r', '').split('\n')
sourceFixed = re.sub(r'\r?\n','; \r\n ', source)			#end each line with ' ;' to fix shlex.lineno bug
#sourceFixed = re.sub(r'\r?\n',' \r\n ', source)
lexer = shlex.shlex(sourceFixed)
lexer.commenters = [';']
lexer.quotes = ['"']
lexer.wordchars += r'=()#,.+-<>/[]:'

fp_log = open('log/fp_log.txt','w')
const_log = open('log/const_log.txt','w')
listing = open('log/listing.txt','w')

origin = 0
current_address = 0
current_line = 0

brackets = [Bracket(None,0)]
memory_fp = []
memory_sp = []
constants = {}

last_opc = 'JMP'

first_pass()
second_pass()
generate_listing()

hex = memory_as_hex()


if len(sys.argv) == 2:
	import pyperclip
	pyperclip.copy(hex)
	print('Done. Copied to clipboard.')
elif len(sys.argv) == 3:
	file_name = sys.argv[2]
	if re.compile(r'^[\w\/]+\.((hex)|(txt))$').match(file_name):
		file = open(file_name,'w')
		file.write(hex)
	else:
		b = bytes(memory_sp)
		with open(file_name, 'bw') as f:
			f.write(b)