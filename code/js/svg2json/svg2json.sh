#!/bin/bash

python svg2json.py $1 tmp.json
node brick_settle.js $2
#rm tmp.json