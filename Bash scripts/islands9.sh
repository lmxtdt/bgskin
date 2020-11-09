#!/bin/bash -l
#PBS -l walltime=60:00:00,nodes=1:ppn=24,mem=2GB
#PBS -m abe 
#PBS -M thoms352@umn.edu 

cd ~/backgroundKinship

let "rep = 1540000"

#cost: 0.0 benefit: 0.21
let "c = 10"
let "b = 21"

echo "islands9 start: 280,000 total, 40,000 iterations of each fitness" >> timeIsland.txt
date >> timeIsland.txt

for j in {1..4000}
	do for f in 0 -1000 -6 -8 10 -12 -14
		do for iter in {0..9}
			do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
					-d "K = 10" -d "popSize = 10" -d "numPops = 100" -d "mRate100 = 10"\
					-d "f = $f" -d "b = $b" -d "c = $c" \
					-d "outputPath = 'island09.csv'" -d "makeSeeds = 0" \
					BGSandKinshipIsland.slim
			let "rep += 1"
		done;
	done;
done

echo "islands9 end" >> timeIsland.txt
date >> timeIsland.txt