# This is a "master script" that I can submit to sbatch
# on Cedar, for comparison testing of various versions
# of our method, and the baseline Dice package.

# The parameters below need to match those
# in the various .sh files being called below

TEST_FUNC="rastr"
DIM=5
NUM_INIT_OBS=50
NUM_SUBSEQ_OBS=400
N_MAX=250
EPSILON=0.01 # 0.01 # 0.05 # 0.10 # 0.25

# The reason I make the directories below
# and then navigate into them before
# sending the job scripts to the scheduler
# is so that the .out files are in the
# same directory as the .csv files.

# Test regular Dice

mkdir /home/jsa378/scratch/${TEST_FUNC}_dice_${DIM}_dim_${NUM_INIT_OBS}_initobs_${NUM_SUBSEQ_OBS}_subseqobs/
cd /home/jsa378/scratch/${TEST_FUNC}_dice_${DIM}_dim_${NUM_INIT_OBS}_initobs_${NUM_SUBSEQ_OBS}_subseqobs/
sbatch /home/jsa378/bo_partition/code/implementation_testing/implementation_testing.sh

# Test v4

mkdir /home/jsa378/scratch/${TEST_FUNC}_v4_${DIM}_dim_${NUM_INIT_OBS}_initobs_${N_MAX}_nmax_${NUM_SUBSEQ_OBS}_subseqobs/
cd /home/jsa378/scratch/${TEST_FUNC}_v4_${DIM}_dim_${NUM_INIT_OBS}_initobs_${N_MAX}_nmax_${NUM_SUBSEQ_OBS}_subseqobs/
sbatch /home/jsa378/bo_partition/code/research/v4/array.sh

# Test v5

mkdir /home/jsa378/scratch/${TEST_FUNC}_v5_${DIM}_dim_${NUM_INIT_OBS}_initobs_${N_MAX}_nmax_${NUM_SUBSEQ_OBS}_subseqobs/
cd /home/jsa378/scratch/${TEST_FUNC}_v5_${DIM}_dim_${NUM_INIT_OBS}_initobs_${N_MAX}_nmax_${NUM_SUBSEQ_OBS}_subseqobs/
sbatch /home/jsa378/bo_partition/code/research/v5/array.sh

# Test v6
# To run v6 with different values of epsilon,
# comment out regular Dice and v4 and v5 above,
# then modify EPSILON above and in v6/array.sh
# to the same value,
# and then submit this script.

mkdir /home/jsa378/scratch/${TEST_FUNC}_v6_${DIM}_dim_${NUM_INIT_OBS}_initobs_${N_MAX}_nmax_${NUM_SUBSEQ_OBS}_subseqobs_${EPSILON}_epsilon/
cd /home/jsa378/scratch/${TEST_FUNC}_v6_${DIM}_dim_${NUM_INIT_OBS}_initobs_${N_MAX}_nmax_${NUM_SUBSEQ_OBS}_subseqobs_${EPSILON}_epsilon/
sbatch /home/jsa378/bo_partition/code/research/v6/array.sh
