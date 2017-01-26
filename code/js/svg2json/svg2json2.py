import sys
import xml.etree.ElementTree as ET
import math
import json
import copy

tree = ET.parse(sys.argv[1])

root = tree.getroot()
bricks = None
for child in root:
	if 'id' in child.attrib:
		if child.attrib['id'] == 'bricks':
			bricks = child

json_rep = []
json_temp = {}
ind = 0
for brick in bricks:
	x = float(brick.attrib['x'])
	y = float(brick.attrib['y'])
	width = float(brick.attrib['width'])
	height = float(brick.attrib['height'])
	brick_x = x + width / 2
	brick_y = y + height / 2
	angle = 0
	if height > width:
		angle -= math.pi / 2
	if 'transform' in brick.attrib:
		vals = brick.attrib['transform'].split()
		vals[0] = vals[0][7:]
		vals[-1] = vals[-1][:-1]
		vals = [float(i) if abs(float(i)) > 0.001 else 0.0 for i in vals]
		vals[4:] = [round(i) for i in vals[4:]]
		angle += math.acos(vals[0])
	name = 'brick_' + str(ind)
	json_temp[ind] = {
		'name': name,
		'x': brick_x / 100,
		'y': brick_y / 100,
		'angle': angle
	}
	ind += 1
all_combos = []
for i in range(ind):
	json_temp = copy.deepcopy(json_temp)
	json_temp[0], json_temp[i] = json_temp[i], json_temp[0]
	json_temp[0]['name'], json_temp[i]['name'] = json_temp[i]['name'], json_temp[0]['name']
	json_wow = [json_temp[j] for j in sorted(json_temp.keys())]
	json_rep.append(json_wow)
for i in range(ind):
	json.dump(json_rep[i], open(sys.argv[2]+'_'+str(i)+'.json', 'w'))
#print json_rep

print ind