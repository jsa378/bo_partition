TEST_FUNC="schwef"
DIM=5
NUM_SUBSEQ_OBS=400
N_MAX=100

mkdir /home/jsa378/scratch/${TEST_FUNC}_dice_${DIM}_dim_${NUM_SUBSEQ_OBS}_numsubseqobs
cd /home/jsa378/scratch/${TEST_FUNC}_dice_${DIM}_dim_${NUM_SUBSEQ_OBS}_numsubseqobs
sbatch /home/jsa378/bo_partition/code/implementation_testing/implementation_testing.sh

# mkdir /home/jsa378/scratch/${TEST_FUNC}_v4_${DIM}_dim_${N_MAX}_nmax_${NUM_SUBSEQ_OBS}_numsubseqobs
# cd /home/jsa378/scratch/${TEST_FUNC}_v4_${DIM}_dim_${N_MAX}_nmax_${NUM_SUBSEQ_OBS}_numsubseqobs
# sbatch /home/jsa378/bo_partition/code/research/v4/array.sh
# 
# mkdir /home/jsa378/scratch/${TEST_FUNC}_v5_${DIM}_dim_${N_MAX}_nmax_${NUM_SUBSEQ_OBS}_numsubseqobs
# cd /home/jsa378/scratch/${TEST_FUNC}_v5_${DIM}_dim_${N_MAX}_nmax_${NUM_SUBSEQ_OBS}_numsubseqobs
# sbatch /home/jsa378/bo_partition/code/research/v5/array.sh
