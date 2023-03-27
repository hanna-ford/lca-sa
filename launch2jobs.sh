#!/bin/bash


for i in `seq 1 2`; do
   export JOBGROUP=$i
   sbatch --export=ALL runRgenerate-lcps.sl
done

