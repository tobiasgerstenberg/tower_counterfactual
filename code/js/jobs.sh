#!/bin/bash

if [ ! -d "simulations" ]; then
	mkdir simulations
fi

trial=0

if [ $# -gt 0 ]; then
	trial=$1
fi
echo \#!/bin/bash > run.sh

#noiseType=(torque-local-above dxy-local dx-local-above impulse-local impulse-local-above impulse-global)
noiseType=(impulse-global impulse-local impulse-local-above-extended)
sims=100
fric=0.5

for type in ${noiseType[@]}; do
	for level in `seq 0.1 0.1 15`; do
		#for fric in `seq 0.5 0.1 0.5`; do
			echo python noise.js $type $level $fric $trial $sims >> run.sh
		#done
	done
done

chmod +x run.sh
