#!/bin/bash -l
#PBS -l walltime=48:00:00,nodes=1:ppn=16,mem=2GB
#PBS -m abe 
#PBS -M thoms352@umn.edu 

cd ~/backgroundKinship

let "rep = 4500000"

#cost: 0 benefit: 21
let "c = 10"
let "b = 21"

#fitness: -0.008
let "f = -8"

for j in {1..50000}
	do for iter in {0..9}
		do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
				-d "f = $f" -d "b = $b" -d "c = $c" \
				-d "outputPath = 'BGSKLate16.csv'" -d "makeSeeds = 0" \
				-m -t BGSandKinshipLate.slim
		let "rep += 1"
	done;
done

