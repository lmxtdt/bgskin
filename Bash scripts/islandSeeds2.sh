#!/bin/bash -l
#PBS -l walltime=12:00:00,nodes=1:ppn=24,mem=2GB
#PBS -m abe 
#PBS -M thoms352@umn.edu 

cd ~/backgroundKinship

let "rep = 0"

#cost: 0.0 benefit: 0.20
let "c = 10"
let "b = 20"
let "f = -10"

for iter in {0..9}
	do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
			-d "K = 10" -d "popSize = 10" -d "numPops = 100" -d "mRate100 = 10"\
			-d "f = $f" -d "b = $b" -d "c = $c" \
			-d "outputPath = 'islandSeeds.csv'" -d "makeSeeds = 1" \
			-m -t BGSandKinshipIsland.slim
	let "rep += 1"
done;
