#!/bin/bash -l
#PBS -l walltime=12:00:00,nodes=2:ppn=128,mem=2GB
#PBS -m abe 
#PBS -M thoms352@umn.edu 

cd ~/backgroundKinship

let "rep = 0"

#cost: 0.1
let "c = 10"

function run {
	for f in {1..1000}
		do for b in 19 20 21
			do for f in 0 -1000 -6 -8 -10 -12 -14
				do for iter in {1..9}
					do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
									-d "f = $f" -d "b = $b" -d "c = $c" \
									-d "outputPath = 'testIters2.csv'" -d "makeSeeds = 0" \
									BGSandKinship.slim
				done;
			done;
		done;
	done
}

{ time run &>> resourcesOutput.txt ; } &>> resources.txt
echo "nodes: 2, ppn: 128" >> resources.txt

#amdlarge