#!/bin/bash
#SBATCH --account=def-wjwelch    # replace this with your own account
#SBATCH --mem-per-cpu=1000M      # memory; default unit is megabytes
#SBATCH --array=1-1              # number of array jobs, inclusive
#SBATCH --time=0-00:10           # time (DD-HH:MM)
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jsa378@sfu.ca
#SBATCH --output=name%j.out

module purge
module load StdEnv/2023 gcc/12.3 r/4.4.0     # Adjust version and add the gcc module used for installing packages.

TEST_FUNC="rastr"
DIM=10
NUM_INIT_OBS=40
NUM_RUNS=10
SAVE_DIR=/home/jsa378/bo_partition/code/implementation_testing/init_points/${TEST_FUNC}_${DIM}_dim_${NUM_RUNS}_runs_${NUM_INIT_OBS}_init_points/

printf "Generating $NUM_INIT_OBS init points for $NUM_RUNS for the $TEST_FUNC test function.\n"
printf "The init points will be saved in $SAVE_DIR.\n"

if [ ! -d "$SAVE_DIR" ]
then
  echo "The save directory $SAVE_DIR does not exist, so we will create it."
  mkdir $SAVE_DIR
else
  echo "The save directory $SAVE_DIR exists, so we don't need to create it."
fi

Rscript /home/jsa378/bo_partition/code/implementation_testing/init_points/gen_init_points.R $TEST_FUNC $DIM $NUM_INIT_OBS $NUM_RUNS $SAVE_DIR

printf "Init points created and saved."
