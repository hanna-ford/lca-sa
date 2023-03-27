#!/bin/bash

#SBATCH --job-name=LCA.SA
#SBATCH --partition=himem72
#SBATCH --mail-user=$USER@uark.edu
#SBATCH --mail-type=ALL
#SBATCH --output=slurm-%j.out-%N
#SBATCH --nodes=1
#SBATCH --tasks-per-node=24
#SBATCH --time=72:00:00

cd $SLURM_SUBMIT_DIR

module purge; module load os/el7 gcc/8.3.1 mkl/19.0.5 gdal/3.3.1 geos/3.6.2 R/4.2.2

Rscript generate-lcps.R