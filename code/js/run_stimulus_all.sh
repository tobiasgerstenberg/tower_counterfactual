#!/bin/bash

name=$(echo $1 | cut -f 1 -d '.' | rev | cut -f 1 -d '/' | rev)

local_noise=7.2
global_noise=3.6
localext_noise=10.4

fric=0.5

echo "You are testing $name"
cd svg2json
numbricks=$(bash svg2json2.sh ../$1 ../simulations/$name)
echo "You have $numbricks bricks in your SVG."
cp tmp.json ../interface/data/${name}_initial.json
cd ..

python get_num_bricks.py svg2json/tmp.json svg2json/tmp2.json
for i in $(seq 0 $(($numbricks-1))); do
	cd svg2json
	cp tmp_$i.json ../simulations/${name}_${i}.json
	cp ../simulations/${name}_$i.json ../interface/data/${name}_${i}.json
	cd ..

	#python get_num_bricks.py svg2json/tmp.json simulations/${name}.json
	echo "Simulating mode ${i}"
	node noise.js none 0 $fric simulations/${name}_$i.json 1
	node noise.js impulse-local $local_noise $fric simulations/${name}_$i.json 100
	node noise.js impulse-global $global_noise $fric simulations/${name}_$i.json 100
	node noise.js impulse-local-above-extended $localext_noise $fric simulations/${name}_$i.json 100
	python print_simulations.py none_0_$fric.json
	python print_simulations.py impulse-local_${local_noise}_$fric.json
	python print_simulations.py impulse-global_${global_noise}_$fric.json
	python print_simulations.py impulse-local-above-extended_${localext_noise}_$fric.json

done