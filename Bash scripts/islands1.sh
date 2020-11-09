#!/bin/bash -l
#PBS -l walltime=48:00:00,nodes=1:ppn=24,mem=2GB
#PBS -m abe 
#PBS -M thoms352@umn.edu 

cd ~/backgroundKinship

let "rep = 0"

#cost: 0.0 benefit: 0.20
let "c = 10"
let "b = 20"

echo "island1: 70,000 total, 10,000 iterations of each fitness" > timeIsland.txt
date >> timeIsland.txt

for j in {1..1000}
	do for f in 0 -1000 -6 -8 10 -12 -14
		do for iter in {0..9}
			do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
					-d "K = 10" -d "popSize = 10" -d "numPops = 100" -d "mRate100 = 10"\
					-d "f = $f" -d "b = $b" -d "c = $c" \
					-d "outputPath = 'island01.csv'" -d "makeSeeds = 0" \
					-m -t BGSandKinshipIsland.slim
			let "rep += 1"
		done;
	done;
done;

date >> timeIsland.txt