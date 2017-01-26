import sys
import json

threshold = 0.2

svgf = sys.argv[1]
jsonf = sys.argv[2]

with open(svgf) as data_file:    
    data = json.load(data_file)
    svg_data = {}
    for brick in data:
        svg_data[brick['name']] = (brick['x'], brick['y'])
with open(jsonf) as data_file:    
    data = json.load(data_file)
    json_data = {}
    for brick in data:
        json_data[brick['name']] = (brick['x'], brick['y'])

count = 0
for brick in json_data:
    if json_data[brick][0] != -5:
        count += 1
print 'Your stimulus (after settling) has ' + str(count) + ' bricks.'

for brick in svg_data:
    dx = abs(svg_data[brick][0] - json_data[brick][0])
    dy = abs(svg_data[brick][1] - json_data[brick][1])
    if dx > threshold or dy > threshold:
        print 'WARNING: Brick ' + str(brick.split('_')[-1]) + ' has moved significantly (' + str(dx) + ', ' + str(dy) + ') after settling. Consider redrawing the SVG to be more stable.'
