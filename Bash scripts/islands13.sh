#!/bin/bash -l
#PBS -l walltime=60:00:00,nodes=1:ppn=24,mem=2GB
#PBS -m abe 
#PBS -M thoms352@umn.edu 

cd ~/backgroundKinship

let "rep = 2220000"

#cost: 0.1
let "c = 10"
#fitness = -0.01
let "f = -10"

echo "islands13 start: 280,000 total, 80,000 iterations of each fitness" >> timeIsland.txt
date >> timeIsland.txt

for j in {1..2000}
	do for b in 19 20 21
		do for iter in {0..9}
			do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
					-d "K = 10" -d "popSize = 10" -d "numPops = 100" -d "mRate100 = 10"\
					-d "f = $f" -d "b = $b" -d "c = $c" \
					-d "outputPath = 'island13.csv'" -d "makeSeeds = 0" \
					BGSandKinshipIsland.slim
			let "rep += 1"
		done;
	done;
done

echo "islands13 end" >> timeIsland.txt
date >> timeIsland.txt