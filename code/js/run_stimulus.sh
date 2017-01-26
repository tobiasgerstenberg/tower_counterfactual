#!/bin/bash

name=$(echo $1 | cut -f 1 -d '.' | rev | cut -f 1 -d '/' | rev)

local_noise=7.2
global_noise=3.6
localext_noise=10.4

fric=0.5

cd svg2json
bash svg2json.sh ../$1 ../simulations/$name.json
cp tmp.json ../interface/data/${name}_initial.json
cp ../simulations/$name.json ../interface/data/${name}_final.json
cd ..

python get_num_bricks.py svg2json/tmp.json simulations/$name.json

echo
node noise.js none 0 $fric simulations/$name.json 1
echo 'Simulating impulse-local model...'
node noise.js impulse-local $local_noise $fric simulations/$name.json 1
echo 'Simulating impulse-global model...'
node noise.js impulse-global $global_noise $fric simulations/$name.json 1
echo 'Simulating impulse-local-above-extended model...'
node noise.js impulse-local-above-extended $localext_noise $fric simulations/$name.json 1

echo -e '\nAverage number of bricks fallen for each model:'
python print_simulations.py none_0_$fric.json
python print_simulations.py impulse-local_${local_noise}_$fric.json
python print_simulations.py impulse-global_${global_noise}_$fric.json
python print_simulations.py impulse-local-above-extended_${localext_noise}_$fric.json