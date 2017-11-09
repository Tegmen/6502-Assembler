import xml.etree.ElementTree as ET
import sys
import re
import os

def convert_level(file):
	tiles = ''
	levelTree = ET.parse(file)
	root = levelTree.getroot()
	for map in root.findall('layer'):
		for data in map.findall('data'):
			tiles = "".join(["{:02X}".format(int(i)-1) for i in data.text.replace('\n', '').split(',')])
	out = open(file[:-4]+'.hex','w')
	out.write(tiles)


#print(convert_level('Level1.tmx'))
	

for file in os.listdir('./'):
    if file.endswith(".tmx"):
        convert_level(file)

		

