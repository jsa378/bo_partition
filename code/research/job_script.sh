JOBID1=$(sbatch --parsable /home/jsa378/bo_partition/code/research/bo_partition.sh)
sbatch /home/jsa378/bo_partition/code/research/plot.sh --dependency=afterok:$JOBID1