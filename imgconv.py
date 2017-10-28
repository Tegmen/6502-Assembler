from PIL import Image
import sys

name = sys.argv[1]
out = open(name+'.hex','w')
img = Image.open(name+'.png').convert('RGB')
width, height = img.size
for row in range (0,height//8):
	for col in range (0,width//8):
		for byte in range(0,8):
			val = 0
			pow = 128
			for p in range(0,8):
				if max(img.getpixel((col*8+p,row*8+byte))) > 127:
					val += pow
				pow = pow // 2
			out.write('{:02X}'.format(val))
