#!/bin/bash

numbricks=$(python svg2json.py $1 tmp.json)
echo $numbricks
node brick_settle.js tmp2.json

for i in $(seq 0 $(($numbricks-1))); do
	python brick_permute.py tmp2.json tmp
done
#rm tmp.json