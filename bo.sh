#!/bin/bash
#SBATCH --account=def-wjwelch    # replace this with your own account
#SBATCH --mem-per-cpu=2000M      # memory; default unit is megabytes
#SBATCH --array=1-2             # number of array jobs, inclusive
#SBATCH --time=0-00:05           # time (DD-HH:MM)
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jsa378@sfu.ca
#SBATCH --output=name%j.out

module purge
module load StdEnv/2023 gcc/12.3 r/4.4.0     # Adjust version and add the gcc module used for installing packages.

SEED=$SLURM_ARRAY_TASK_ID
TEST_FUNC="rastr"
DIM=2
NUM_INIT_OBS=5
NUM_OBS=5
NUM_RUNS=1
SAVE_DIR=/home/jsa378/scratch/rastr_${DIM}_${NUM_INIT_OBS}_${NUM_OBS}_${NUM_RUNS}/

printf "The current job ID is: $SLURM_JOB_ID\n"
printf "The current array task ID is: $SLURM_ARRAY_TASK_ID\n"

if [ ! -d "$SAVE_DIR" ]
then
  echo "$SAVE_DIR does not exist, therefore will create it."
  mkdir $SAVE_DIR
else
  echo "$SAVE_DIR exists, therefore no need to create it."
fi

# mkdir $SAVE_DIR

# Rscript /home/jsa378/bo_partition/code/bo_runs/test.R
# Rscript /home/jsa378/bo_partition/code/bo_runs/cedar_test.R
# Rscript /home/jsa378/bo_partition/code/bo_runs/cedar_test.R 1 rastr 2 10 10 1
Rscript /home/jsa378/bo_partition/code/bo_runs/cedar_test.R $SEED $TEST_FUNC $DIM $NUM_INIT_OBS $NUM_OBS $NUM_RUNS $SAVE_DIR
