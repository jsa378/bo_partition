#!/bin/bash
#SBATCH --account=def-wjwelch    # replace this with your own account
#SBATCH --mem-per-cpu=4000M      # memory; default unit is megabytes
#SBATCH --time=1-00:00           # time (DD-HH:MM)
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jsa378@sfu.ca
#SBATCH --output=name%j.out

ulimit -c unlimited
module purge
module load StdEnv/2023 gcc/12.3 r/4.4.0 openmpi/4.1.5 valgrind-mpi/3.21.0    # Adjust version and add the gcc module used for installing packages.

printf "The current job ID is: $SLURM_JOB_ID\n"
# mkdir /home/jsa378/scratch/ego_valgrind/

R -d "valgrind --tool=memcheck --leak-check=yes --show-reachable=yes" < /home/jsa378/bo_partition/code/research/debug/bo_partition.R
