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
export R_PACKAGE="ego"
export DIM=10
export NUM_INIT_OBS=40
export NUM_OBS=200
export NUM_RUNS=10
export N_MAX=$(($DIM * 5))
export TOL=0.1
export SPLIT_CRIT="y_min_minus_a_max" # I commented out "avg"; also, "maximizeEI" only works with R_PACKAGE="dice"
export SAVE_DIR=/home/jsa378/scratch/bo_partition_${TEST_FUNC}_${R_PACKAGE}_${DIM}_${NUM_INIT_OBS}_${NUM_OBS}_${NUM_RUNS}/
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

Rscript /home/jsa378/bo_partition/code/research/bo_partition.R $SEED $TEST_FUNC $R_PACKAGE $DIM $NUM_INIT_OBS $NUM_OBS $NUM_RUNS $N_MAX $TOL $SPLIT_CRIT $SAVE_DIR $SLURM_JOB_ID

# NUM_FILES=$(find $SAVE_DIR -type f -name '*.csv' | wc -l)
# if [ "$NUM_FILES" -eq "$NUM_CSVS" ]
# then
#   echo "This is the last of the array jobs to finish, so we will make the plots now."
#   module load python/3.11.5 
#   virtualenv --no-download $SLURM_TMPDIR/env
#   source $SLURM_TMPDIR/env/bin/activate
#   pip install --no-index --upgrade pip
#   pip install --no-index -r /home/jsa378/python/plot_requirements.txt
#   python /home/jsa378/bo_partition/code/research/plots.py $SEED $TEST_FUNC $R_PACKAGE $DIM $NUM_INIT_OBS $NUM_OBS $NUM_RUNS $N_MAX $TOL $SPLIT_CRIT $SAVE_DIR $NUM_ARRAY_JOBS
# else
#   echo "This is not the last job, so we can't make the plots yet."
# fi
