#!/bin/bash
{
	read
	while IFS='' read -r line || [[ -n "$line" ]]; do
	    cp $(echo $line | awk -F',' '{print "../simulations/trial" $2+1 "_" $4 ".json"}') $(echo $line | awk -F',' '{print "json/trial" $1 ".json"}')
	done
} < $1