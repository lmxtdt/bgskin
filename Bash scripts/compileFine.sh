#!/bin/bash

echo "generation,fitness,cost,benefit,num_neutral,pi_neutral,percent_m1,status,replicate,seed_pop" > fineTotal.csv

for i in {0..1}
        do cat BGSKFine$i.csv >> fineTotal.csv
done
