import sys
import json

name = sys.argv[1]
model = name.split('_')[0]

with open('simulations/'+name) as data_file:    
    data = json.load(data_file)

count = 0.0

for i in data:
	total = len(data[i])
	for sim in data[i]:
		count += len(sim)

count /= total

print model + '\t' + str(count)