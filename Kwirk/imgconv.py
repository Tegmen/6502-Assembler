from PIL import Image
import sys

name = sys.argv[1]
out = open(name+'.hex','w')
img = Image.open(name+'.png').convert('RGB')
width, height = img.size
for row in range (0,32):
	for col in range (0,32):
		char = 0xC0
		if max(img.getpixel((col*2,row*2))) > 127:
			char = char | 1
		if max(img.getpixel((col*2+1,row*2))) > 127:
			char = char | 2
		if max(img.getpixel((col*2,row*2+1))) > 127:
			char = char | 4
		if max(img.getpixel((col*2+1,row*2+1))) > 127:
			char = char | 8	
		out.write('{:02X}'.format(char))
