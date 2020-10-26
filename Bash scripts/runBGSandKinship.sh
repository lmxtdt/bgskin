#!/bin/bash

#cost = c / 100
let "c = 10"
let "rep = 35000"

for j in {1..900}
	#benefit = b / 100
	do for b in 50 40 30 20 10
		#fitness = f / 1000
		do for f in 0 -6 -8 -10 -12 -14 -1000
			do for iter in {0..9}
				do slim -d "replicate = $rep" -d "f = $f" -d "iter = $iter" -d "b = $b" -d "c = $c" BGSandKinship.slim
				let "rep += 1"
			done;
		done;
	done;
done