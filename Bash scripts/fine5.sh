#!/bin/bash -l
#PBS -l walltime=72:00:00,nodes=1:ppn=24,mem=2GB
#PBS -m abe 
#PBS -M thoms352@umn.edu 

cd ~/backgroundKinship

let "rep = 1120000"

#cost: 0.1
let "c = 100"
let "f = 0"

let "l = 4"

for j in {1..1000}
	do for b in {190..198}
		do for iter in {0..9}
			do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
					-d "f = $f" -d "b = $b" -d "c = $c" \
					-d "outputPath = 'BGSKFine4.csv'" -d "makeSeeds = 0" \
					-d "litterSize = $l" \
					BGSandKinshipFine.slim
			let "rep += 1"
		done;
	done;
done
