import sys
import xml.etree.ElementTree as ET
import math
import json

tree = ET.parse(sys.argv[1])

root = tree.getroot()
bricks = None
for child in root:
	if 'id' in child.attrib:
		if child.attrib['id'] == 'bricks':
			bricks = child

json_rep = []
json_temp = {}
ind = 1
count = 0
for brick in bricks:
	x = float(brick.attrib['x'])
	y = float(brick.attrib['y'])
	width = float(brick.attrib['width'])
	height = float(brick.attrib['height'])
	brick_x = x + width / 2
	brick_y = y + height / 2
	angle = 0
	if height > width:
		angle += math.pi / 2
	if 'transform' in brick.attrib:
		vals = brick.attrib['transform'].split()
		vals[0] = vals[0][7:]
		vals[-1] = vals[-1][:-1]
		vals = [float(i) if abs(float(i)) > 0.001 else 0.0 for i in vals]
		vals[4:] = [round(i) for i in vals[4:]]
		angle -= math.acos(vals[0])
	if 'id' in brick.attrib:
		name = 'brick_0'
	else:
		name = 'brick_' + str(ind)
		ind += 1
	json_temp[name] = {
		'name': name,
		'x': brick_x / 100,
		'y': brick_y / 100,
		'angle': angle
	}

for i in range(ind):
	json_rep.append(json_temp['brick_'+str(i)])

json.dump(json_rep, open(sys.argv[2], 'w'))

print ind