#!/bin/bash
#SBATCH --account=def-wjwelch    # replace this with your own account
#SBATCH --mem-per-cpu=2000M      # memory; default unit is megabytes
#SBATCH --time=0-00:15           # time (DD-HH:MM)
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jsa378@sfu.ca
#SBATCH --output=name%j.out

module load StdEnv/2023 gcc/12.3 r/4.4.0     # Adjust version and add the gcc module used for installing packages.

printf "The current job ID is: $SLURM_JOB_ID\n"

Rscript /home/jsa378/bo_partition/code/bo_runs/cedar_test.R
