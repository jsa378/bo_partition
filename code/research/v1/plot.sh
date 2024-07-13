#!/bin/bash
#SBATCH --account=def-wjwelch    # replace this with your own account
#SBATCH --mem-per-cpu=2000M      # memory; default unit is megabytes
#SBATCH --time=0-00:10           # time (DD-HH:MM)
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jsa378@sfu.ca
#SBATCH --output=name%j.out

module load python/3.11.5 
virtualenv --no-download $SLURM_TMPDIR/env
source $SLURM_TMPDIR/env/bin/activate
pip install --no-index --upgrade pip
pip install --no-index -r /home/jsa378/python/plot_requirements.txt
python /home/jsa378/bo_partition/code/research/plots.py $SEED $TEST_FUNC $R_PACKAGE $DIM $NUM_INIT_OBS $NUM_OBS $NUM_RUNS $N_MAX $TOL $SPLIT_CRIT $SAVE_DIR $NUM_ARRAY_JOBS