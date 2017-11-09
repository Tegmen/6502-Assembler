import sys
import csv
hex_string = ''
name = sys.argv[1]
out = open(name+'.hex','w')
inp = list(csv.reader(open(name+'.csv').read().split()))
for y in inp:
	for x in y:
		hex_string += '{:02X}'.format(int(x))
out.write(hex_string)
