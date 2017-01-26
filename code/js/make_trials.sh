#!/bin/bash
echo "trials = {}" > data_source2.js

for i in $(seq 1 42); do
	while IFS='' read -r line || [[ -n "$line" ]]; do
		echo "trials['trial_$i'] = $line" >> data_source2.js
	done < ./exp4stimuli/json/trial$i.json
	echo ./exp4stimuli/json/trial$i.json

done