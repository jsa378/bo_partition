#!/bin/bash
#SBATCH --account=def-wjwelch    # replace this with your own account
#SBATCH --mem-per-cpu=4000M      # memory; default unit is megabytes
#SBATCH --array=2,5,10         # number of array jobs, inclusive
#SBATCH --time=1-00:00           # time (DD-HH:MM)
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jsa378@sfu.ca
#SBATCH --output=name%j.out

ulimit -c unlimited
module purge
module load StdEnv/2023 gcc/12.3 r/4.4.0     # Adjust version and add the gcc module used for installing packages.

SEED=$SLURM_ARRAY_TASK_ID
TEST_FUNC="rastr"
DIM=$SLURM_ARRAY_TASK_ID
NUM_INIT_OBS=$(($DIM * 20)) # submit this script with 5, 10 and 20 here
NUM_RUNS=100
SAVE_DIR=/home/jsa378/scratch/${TEST_FUNC}_${DIM}_dim_${NUM_INIT_OBS}_initobs/
COVTYPE="PowerExponential" # "gauss"
NUGGET=1e-09

printf "The current job ID is: $SLURM_JOB_ID\n"
printf "The current array task ID is: $SLURM_ARRAY_TASK_ID\n"

if [ ! -d "$SAVE_DIR" ]
then
  echo "The save directory $SAVE_DIR does not exist, so we will create it."
  mkdir -p $SAVE_DIR
else
  echo "The save directory $SAVE_DIR exists, so we don't need to create it."
fi

Rscript /home/jsa378/bo_partition/code/research/thetas/thetas.R $SEED $TEST_FUNC $DIM $NUM_INIT_OBS $NUM_RUNS $SAVE_DIR $SLURM_JOB_ID $COVTYPE $NUGGET
