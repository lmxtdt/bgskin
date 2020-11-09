#!/bin/bash -l
#PBS -l walltime=72:00:00,nodes=1:ppn=24,mem=2GB
#PBS -m abe 
#PBS -M thoms352@umn.edu 

cd ~/backgroundKinship

let "rep = 6750000"

#cost: 0.1 benefit: 0.21
let "c = 10"
let "b = 21"

#fitness: -1
let "f = -1000"

for iter in {3..9}
	do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
			-d "f = $f" -d "b = $b" -d "c = $c" \
			-d "outputPath = 'BGSKLate21.csv'" -d "makeSeeds = 0" \
			-m -t BGSandKinshipLate.slim
	let "rep += 1"
done

for j in {1..26903}
	do for iter in {0..9}
		do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
				-d "f = $f" -d "b = $b" -d "c = $c" \
				-d "outputPath = 'BGSKLate21.csv'" -d "makeSeeds = 0" \
				-m -t BGSandKinshipLate.slim
		let "rep += 1"
	done;
done

