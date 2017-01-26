import sys
import math
import json
import copy


with open(sys.argv[1]) as data_file:    
    data = json.load(data_file)

json_rep = []
for i in range(len(data)):
	json_temp = copy.deepcopy(data)
	json_temp[0], json_temp[i] = json_temp[i], json_temp[0]
	json_temp[0]['name'], json_temp[i]['name'] = json_temp[i]['name'], json_temp[0]['name']
	json_rep.append(json_temp)

for i in range(len(data)):
	json.dump(json_rep[i], open(sys.argv[2]+'_'+str(i)+'.json', 'w'))