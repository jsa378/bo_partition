#!/bin/bash
#SBATCH --account=def-wjwelch    # replace this with your own account
#SBATCH --mem-per-cpu=4000M      # memory; default unit is megabytes
#SBATCH --array=1-10             # number of array jobs, inclusive
#SBATCH --time=3-00:00           # time (DD-HH:MM)
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jsa378@sfu.ca
#SBATCH --output=name%j.out

ulimit -c unlimited
module purge
module load StdEnv/2023 gcc/12.3 r/4.4.0     # Adjust version and add the gcc module used for installing packages.

SEED=$SLURM_ARRAY_TASK_ID
TEST_FUNC="schwef"
DIM=5
NUM_INIT_OBS=40
NUM_SUBSEQ_OBS=5 # 400
NUM_RUNS=10
N_MAX=240 # $(($DIM * 5))
TOL=0.1
HOW_MANY_EI_POINTS=1000
TOP_N_EI_VALS=10
SAVE_DIR=/home/jsa378/scratch/${TEST_FUNC}_v4_${DIM}_dim_${N_MAX}_nmax_${NUM_SUBSEQ_OBS}_numsubseqobs/
COVTYPE="matern5_2"
NUGGET=1e-09
MIN_CONSIDER_REJECT=$(($NUM_SUBSEQ_OBS / 2))
NUM_ARRAY_JOBS=10
NUM_CSVS=$(($NUM_ARRAY_JOBS * 3))

printf "The current job ID is: $SLURM_JOB_ID\n"
printf "The current array task ID is: $SLURM_ARRAY_TASK_ID\n"

if [ ! -d "$SAVE_DIR" ]
then
  echo "The save directory $SAVE_DIR does not exist, so we will create it."
  mkdir -p $SAVE_DIR
else
  echo "The save directory $SAVE_DIR exists, so we don't need to create it."
fi

Rscript /home/jsa378/bo_partition/code/research/v4/bo_partition.R $SEED $TEST_FUNC $DIM $NUM_INIT_OBS $NUM_SUBSEQ_OBS $NUM_RUNS $N_MAX $TOL $HOW_MANY_EI_POINTS $TOP_N_EI_VALS $POINT_SHARE_TOL $SAVE_DIR $SLURM_JOB_ID $COVTYPE $NUGGET $MIN_CONSIDER_REJECT
