#!/bin/bash
#SBATCH --account=def-wjwelch    # replace this with your own account
#SBATCH --mem-per-cpu=1000M      # memory; default unit is megabytes
#SBATCH --array=1-10             # number of array jobs, inclusive
#SBATCH --time=3-00:00           # time (DD-HH:MM)
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jsa378@sfu.ca
#SBATCH --output=name%j.out

ulimit -c unlimited
module purge
module load StdEnv/2023 gcc/12.3 r/4.4.0     # Adjust version and add the gcc module used for installing packages.

export SEED=$SLURM_ARRAY_TASK_ID
export TEST_FUNC="rastr"
export DIM=10
export NUM_INIT_OBS=40
export NUM_SUBSEQ_OBS=200
export NUM_RUNS=10
export N_MAX=$(($DIM * 5))
export TOL=0.1
export HOW_MANY_EI_POINTS=1000
export TOP_N_EI_VALS=10
export SAVE_DIR=/home/jsa378/scratch/bo_partition_${TEST_FUNC}_${DIM}_${NUM_INIT_OBS}_${NUM_SUBSEQ_OBS}_${NUM_RUNS}/
export NUM_ARRAY_JOBS=10
export NUM_CSVS=$(($NUM_ARRAY_JOBS * 3))

printf "The current job ID is: $SLURM_JOB_ID\n"
printf "The current array task ID is: $SLURM_ARRAY_TASK_ID\n"

if [ ! -d "$SAVE_DIR" ]
then
  echo "The save directory $SAVE_DIR does not exist, so we will create it."
  mkdir $SAVE_DIR
else
  echo "The save directory $SAVE_DIR exists, so we don't need to create it."
fi

Rscript /home/jsa378/bo_partition/code/research/bo_partition.R $SEED $TEST_FUNC $DIM $NUM_INIT_OBS $NUM_SUBSEQ_OBS $NUM_RUNS $N_MAX $TOL $HOW_MANY_EI_POINTS $TOP_N_EI_VALS $SAVE_DIR $SLURM_JOB_ID