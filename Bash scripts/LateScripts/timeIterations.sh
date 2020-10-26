#!/bin/bash -l
#PBS -l walltime=12:00:00,nodes=1:ppn=16,mem=2GB
#PBS -m abe 
#PBS -M thoms352@umn.edu 

cd ~/backgroundKinship

let "rep = 0"

#cost: 0 benefit: 20
let "c = 100"
let "b = 200"

#fitness: 0
let "f = 0"

echo "results" > timeLitters2.txt
echo "errors" > timeErrors.txt


for l in 4 6 8
	do 
		echo "litter size = $l" &>> timeLitters2.txt

		for j in {1..2}
			do for iter in {1..9}
				do { time ../build/slim -d "replicate = $rep" -d "iter = $iter" \
										-d "f = $f" -d "b = $b" -d "c = $c" \
										-d "outputPath = 'testy.csv'" -d "makeSeeds = 0" \
										-d "litterSize = $l" \
										BGSandKinshipFine.slim &>> timeErrors.txt ; } &>> timeLitters2.txt
			done;
		done;
done
