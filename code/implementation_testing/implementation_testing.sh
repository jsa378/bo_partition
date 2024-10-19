#!/bin/bash
#SBATCH --account=def-wjwelch    # replace this with your own account
#SBATCH --mem-per-cpu=4000M      # memory; default unit is megabytes
#SBATCH --array=1-10             # number of array jobs, inclusive
#SBATCH --time=28-00:00          # time (DD-HH:MM)
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jsa378@sfu.ca
#SBATCH --output=name%j.out

ulimit -c unlimited
module purge
module load StdEnv/2023 gcc/12.3 r/4.4.0     # Adjust version and add the gcc module used for installing packages.

SEED=$SLURM_ARRAY_TASK_ID
TEST_FUNC="stybt" # "ackley" # "grie" # "langer" # "levy" # "michal" # "rastr" # "schwef" # "stybt"
DIM=15
NUM_INIT_OBS=150
NUM_SUBSEQ_OBS=15000
NUM_RUNS=10 # This needs to match the "#SBATCH --array="" parameter above
SAVE_DIR=/home/jsa378/scratch/${TEST_FUNC}_dice_${DIM}_dim_${NUM_INIT_OBS}_initobs_${NUM_SUBSEQ_OBS}_subseqobs/
COVTYPE="matern5_2"
NUGGET=1e-09
# NUM_ARRAY_JOBS=100 # I don't think I use these last two variables anymore, do I?
# NUM_CSVS=$(($NUM_ARRAY_JOBS * 4))

printf "The current job ID is: $SLURM_JOB_ID\n"
printf "The current array task ID is: $SLURM_ARRAY_TASK_ID\n"

if [ ! -d "$SAVE_DIR" ]
then
  echo "The save directory $SAVE_DIR does not exist, so we will create it."
  mkdir $SAVE_DIR
else
  echo "The save directory $SAVE_DIR exists, so we don't need to create it."
fi

# Rscript /home/jsa378/bo_partition/code/implementation_testing/ego.R $SEED $TEST_FUNC $DIM $NUM_INIT_OBS $NUM_OBS $NUM_RUNS $SAVE_DIR
Rscript /home/jsa378/bo_partition/code/implementation_testing/dice.R $SEED $TEST_FUNC $DIM $NUM_INIT_OBS $NUM_SUBSEQ_OBS $NUM_RUNS $SAVE_DIR $SLURM_JOB_ID $COVTYPE $NUGGET

# module load python/3.11.5 
# virtualenv --no-download $SLURM_TMPDIR/env
# source $SLURM_TMPDIR/env/bin/activate
# pip install --no-index --upgrade pip
# pip install -r /home/jsa378/python/bo_requirements.txt
# python /home/jsa378/bo_partition/code/implementation_testing/python.py $SEED $TEST_FUNC $DIM $NUM_INIT_OBS $NUM_OBS $NUM_RUNS $SAVE_DIR

# NUM_FILES=$(find $SAVE_DIR -type f -name '*.csv' | wc -l)
# if [ "$NUM_FILES" -eq "$NUM_CSVS" ]
# then
#   echo "This is the last of the array jobs to finish, so we will make the plots now."
#   module load python/3.11.5 
#   virtualenv --no-download $SLURM_TMPDIR/env
#   source $SLURM_TMPDIR/env/bin/activate
#   pip install --no-index --upgrade pip
#   pip install --no-index -r /home/jsa378/python/plot_requirements.txt
#   python /home/jsa378/bo_partition/code/implementation_testing/plots.py $SEED $TEST_FUNC $DIM $NUM_INIT_OBS $NUM_OBS $NUM_RUNS $SAVE_DIR $NUM_ARRAY_JOBS
# else
#   echo "This is not the last job, so we can't make the plots yet."
# fi
