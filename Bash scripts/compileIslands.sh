#!/bin/bash

echo "generation,fitness,cost,benefit,num_neutral,pi_neutral,percent_m1,status,replicate,seed_pop" > islandTotal.csv

for i in {1..6}
        do cat island0$i.csv >> islandTotal.csv
done
