#!/bin/bash

let "rep = 14735520"

#fitness: 0 benefit: 0 cost: 0
let "c = 0"
let "f = 0"
let "b = 0"

for iter in {4..9}
	do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
			-d "f = $f" -d "b = $b" -d "c = $c" \
			-d "outputPath = 'BGSKinBoundary62.csv'" -d "makeSeeds = 1" \
			-m -t BGSandKinship.slim
	let "rep += 1"
done

for j in {1..2663}
	do for iter in {0..9}
		do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
				-d "f = $f" -d "b = $b" -d "c = $c" \
				-d "outputPath = 'BGSKinBoundary62.csv'" -d "makeSeeds = 1" \
				-m -t BGSandKinship.slim
		let "rep += 1"
	done;
done

#benefit: 0.21 cost: 0.1
let "c = 10"
let "b = 21"

#fitness: -1
let "f = -1000"

for iter in {5..9}
	do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
			-d "f = $f" -d "b = $b" -d "c = $c" \
			-d "outputPath = 'BGSKinBoundary62.csv'" -d "makeSeeds = 1" \
			-m -t BGSandKinship.slim
	let "rep += 1"
done

for j in {1..59}
	do for iter in {0..9}
		do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
				-d "f = $f" -d "b = $b" -d "c = $c" \
				-d "outputPath = 'BGSKinBoundary62.csv'" -d "makeSeeds = 1" \
				-m -t BGSandKinship.slim
		let "rep += 1"
	done;
done

#fitness: -0.006
let "f = -6"

for iter in {3..9}
	do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
			-d "f = $f" -d "b = $b" -d "c = $c" \
			-d "outputPath = 'BGSKinBoundary62.csv'" -d "makeSeeds = 1" \
			-m -t BGSandKinship.slim
	let "rep += 1"
done

for j in {1..58}
	do for iter in {0..9}
		do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
				-d "f = $f" -d "b = $b" -d "c = $c" \
				-d "outputPath = 'BGSKinBoundary62.csv'" -d "makeSeeds = 1" \
				-m -t BGSandKinship.slim
		let "rep += 1"
	done;
done

#fitness: 0
let "f = 0"

for iter in {7..9}
	do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
			-d "f = $f" -d "b = $b" -d "c = $c" \
			-d "outputPath = 'BGSKinBoundary62.csv'" -d "makeSeeds = 1" \
			-m -t BGSandKinship.slim
	let "rep += 1"
done

for j in {1..58}
	do for iter in {0..9}
		do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
				-d "f = $f" -d "b = $b" -d "c = $c" \
				-d "outputPath = 'BGSKinBoundary62.csv'" -d "makeSeeds = 1" \
				-m -t BGSandKinship.slim
		let "rep += 1"
	done;
done

#fitness: -0.010
let "f = -10"

for iter in 9
	do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
			-d "f = $f" -d "b = $b" -d "c = $c" \
			-d "outputPath = 'BGSKinBoundary62.csv'" -d "makeSeeds = 1" \
			-m -t BGSandKinship.slim
	let "rep += 1"
done

for j in {1..58}
	do for iter in {0..9}
		do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
				-d "f = $f" -d "b = $b" -d "c = $c" \
				-d "outputPath = 'BGSKinBoundary62.csv'" -d "makeSeeds = 1" \
				-m -t BGSandKinship.slim
		let "rep += 1"
	done;
done

#fitness: -0.008
let "f = -8"

for iter in {5..9}
	do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
			-d "f = $f" -d "b = $b" -d "c = $c" \
			-d "outputPath = 'BGSKinBoundary62.csv'" -d "makeSeeds = 1" \
			-m -t BGSandKinship.slim
	let "rep += 1"
done

for j in {1..56}
	do for iter in {0..9}
		do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
				-d "f = $f" -d "b = $b" -d "c = $c" \
				-d "outputPath = 'BGSKinBoundary62.csv'" -d "makeSeeds = 1" \
				-m -t BGSandKinship.slim
		let "rep += 1"
	done;
done

#fitness: -0.012
let "f = -12"

for j in {1..52}
	do for iter in {0..9}
		do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
				-d "f = $f" -d "b = $b" -d "c = $c" \
				-d "outputPath = 'BGSKinBoundary62.csv'" -d "makeSeeds = 1" \
				-m -t BGSandKinship.slim
		let "rep += 1"
	done;
done


#fitness: -0.014

let "f = -14"

for iter in {3..9}
	do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
			-d "f = $f" -d "b = $b" -d "c = $c" \
			-d "outputPath = 'BGSKinBoundary62.csv'" -d "makeSeeds = 1" \
			-m -t BGSandKinship.slim
	let "rep += 1"
done

for j in {1..50}
	do for iter in {0..9}
		do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
				-d "f = $f" -d "b = $b" -d "c = $c" \
				-d "outputPath = 'BGSKinBoundary62.csv'" -d "makeSeeds = 1" \
				-m -t BGSandKinship.slim
		let "rep += 1"
	done;
done

#benefit: 0.22
let "b = 22"

#fitness: -1
let "f = -1000"

for iter in {0..9}
	do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
			-d "f = $f" -d "b = $b" -d "c = $c" \
			-d "outputPath = 'BGSKinBoundary62.csv'" -d "makeSeeds = 1" \
			-m -t BGSandKinship.slim
	let "rep += 1"
done

#fitness: -0.014
let "f = -14"

for iter in {4..9}
	do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
			-d "f = $f" -d "b = $b" -d "c = $c" \
			-d "outputPath = 'BGSKinBoundary62.csv'" -d "makeSeeds = 1" \
			-m -t BGSandKinship.slim
	let "rep += 1"
done

#fitness: -0.010
let "f = -10"

for iter in 9
	do ../build/slim -d "replicate = $rep" -d "iter = $iter" \
			-d "f = $f" -d "b = $b" -d "c = $c" \
			-d "outputPath = 'BGSKinBoundary62.csv'" -d "makeSeeds = 1" \
			-m -t BGSandKinship.slim
	let "rep += 1"
done
