#!/bin/bash -l
#PBS -l walltime=72:00:00,nodes=1:ppn=24,mem=2GB
#PBS -m abe 
#PBS -M thoms352@umn.edu 

cd ~/backgroundKinship

let "rep = 7300000"

#cost: 0.1 benefit: 0.21
let "c = 10"
let "b = 21"

#fitness: -0.012
let "f = -12"

for iter in {2..9}
	do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
			-d "f = $f" -d "b = $b" -d "c = $c" \
			-d "outputPath = 'BGSKLate23.csv'" -d "makeSeeds = 0" \
			-m -t BGSandKinshipLate.slim
	let "rep += 1"
done

for j in {1..24807}
	do for iter in {0..9}
		do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
				-d "f = $f" -d "b = $b" -d "c = $c" \
				-d "outputPath = 'BGSKLate23.csv'" -d "makeSeeds = 0" \
				-m -t BGSandKinshipLate.slim
		let "rep += 1"
	done;
done

