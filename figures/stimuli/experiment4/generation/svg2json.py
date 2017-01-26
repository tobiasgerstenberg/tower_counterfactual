import sys
import xml.etree.ElementTree as ET
import math
import json

tree = ET.parse(sys.argv[1])

root = tree.getroot()
bricks = None
for child in root:
	if 'id' in child.attrib and child.attrib['id'] == 'bricks':
		bricks = child

json_rep = []
for brick in bricks:
	ind = 0
	x = float(brick.attrib['x'])
	y = float(brick.attrib['y'])
	width = float(brick.attrib['width'])
	height = float(brick.attrib['height'])
	brick_x = x + width / 2
	brick_y = y + height / 2
	angle = 0
	if 'transform' in brick.attrib:
		vals = brick.attrib['transform'].split()
		vals[0] = vals[0][7:]
		vals[-1] = vals[-1][:-1]
		vals = [float(i) if abs(float(i)) > 0.001 else 0.0 for i in vals]
		vals[4:] = [round(i) for i in vals[4:]]

		angle = math.acos(vals[0])

	brick_rep = {
		'name': 'brick_' + str(ind),
		'x': brick_x / 100,
		'y': brick_y / 100,
		'angle': angle
	}
	ind += 1

	json_rep.append(brick_rep)

json.dump(json_rep, open(sys.argv[2], 'w'))