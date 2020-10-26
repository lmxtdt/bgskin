#!/bin/bash -l
#PBS -l walltime=48:00:00,nodes=1:ppn=16,mem=2GB
#PBS -m abe 
#PBS -M thoms352@umn.edu 

cd ~/backgroundKinship

let "rep = 0"

#cost: 0 benefit: 20
let "c = 100"
#let "b = 200"

#fitness: 0
let "f = 0"

let "l = 4"

for j in {1..4000}
	do for f in 0 -6 -8 -10 -12 -14 -1000
		do for b in 199 200 201
			do for iter in {0..9}
				do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
						-d "f = $f" -d "b = $b" -d "c = $c" \
						-d "outputPath = 'BGSKFine0.csv'" -d "makeSeeds = 0" \
						-d "litterSize = $l" \
						-m -t BGSandKinshipFine.slim
				let "rep += 1"
			done;
		done;
	done;
done
