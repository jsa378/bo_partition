library(DiceOptim)
library(lhs)

# Begin timing how long the code takes to run

start <- Sys.time()

# Toggle whether working locally (on MacBook) or remotely (on Cedar)
# This is to set whether parameters are set in this file (local)
# or in a job submission script (remotely)

working <- "local"
if (working == "remote") {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 10) {
    stop("Ten arguments must be supplied:
    seed value (int),
    test function (string),
    dim (int),
    num init obs (int),
    num subseq obs (int),
    num runs (int),
    n_max_param (int),
    tol_param (float),
    save dir (no type),
    slurm job id (int)", call. = FALSE)
  }
  seed_value <- as.integer(args[1])
  test_func_name <- args[2]
  dim <- as.integer(args[3])
  num_init_obs <- as.integer(args[4])
  num_subseq_obs <- as.integer(args[5])
  num_runs <- as.integer(args[6])
  n_max_param <- as.integer(args[7])
  tol_param <- as.numeric(args[8])
  save_dir <- as.character(args[9])
  slurm_job_id <- as.integer(args[10])

  source("/home/jsa378/bo_partition/code/test_funcs.R")
  source("/home/jsa378/bo_partition/code/research/bo_partition_helper_funcs.R")
  
  init_points_loc <- sprintf("/home/jsa378/bo_partition/code/implementation_testing/init_points/%s_%s_dim_%s_runs_%s_init_points/run_%s_init_points.csv",
                             test_func_name, dim, num_runs, num_init_obs, seed_value)
} else if (working == "local") {
  seed_value <- 1
  test_func_name <- "rastr"
  dim <- 2
  num_init_obs <- 20
  num_subseq_obs <- 100
  num_runs <- 10
  n_max_param <- 25
  tol_param <- 0.1
  save_dir <- "/Users/jesse/Downloads/cedar_test_output/research_testing/"
  slurm_job_id <- seed_value
  
  source("/Users/jesse/Downloads/bo_partition/code/test_funcs.R")
  source("/Users/jesse/Downloads/bo_partition/code/research/bo_partition_helper_funcs.R")
  
  init_points_loc <- sprintf("/Users/jesse/Downloads/bo_partition/code/implementation_testing/init_points/%s_%s_dim_%s_runs_%s_init_points/run_%s_init_points.csv",
                             test_func_name, dim, num_runs, num_init_obs, seed_value)
  
  sink_file <- sprintf("/Users/jesse/Downloads/cedar_test_output/research_testing/bo_partition_test.txt")
  # sink(file = sink_file)
}

# A function (and some settings) to dump an .rda file
# in the event of an R error (for debugging purposes)

dump_and_quit <- function() {
  # Save debugging info to file last.dump.rda
  dump.frames(dumpto = sprintf("last.dump%s", slurm_job_id), to.file = TRUE)
  # Quit R with error status
  q(status = 1)
}
options(error = dump_and_quit, CBoundsCheck = TRUE)

# Set the test function and associated information

test_func <- test_func_list[[test_func_name]]$func
test_lbound_scalar <- test_func_list[[test_func_name]]$lbound_scalar
test_ubound_scalar <- test_func_list[[test_func_name]]$ubound_scalar
test_lbound <- test_func_list[[test_func_name]]$lbound
test_ubound <- test_func_list[[test_func_name]]$ubound
test_argmin <- test_func_list[[test_func_name]]$argmin

# Print some summary information concerning the job
# at the beginning of the run

paste(c("Slurm job ID:", slurm_job_id), collapse = " ")
print(sprintf("Job beginning at: %s", Sys.time()))
paste(c("Bayesian optimization with seed value:", seed_value), collapse = " ")
paste(c("Test function:", test_func_name), collapse = " ")
paste(c("Number of dimensions:", dim), collapse = " ")
paste(c("Number of initial observations:", num_init_obs), collapse = " ")
paste(c("Number of subsequent  observations:", num_subseq_obs), collapse = " ")
paste(c("Number of runs:", num_runs), collapse = " ")
paste(c("n_max parameter:", n_max_param), collapse = " ")
paste(c("tol parameter:", tol_param), collapse = " ")
paste(c("Save directory:", save_dir), collapse = " ")
paste(c("Test func. lower bound scalar:", test_lbound_scalar), collapse = " ")
paste(c("Test func. upper bound scalar:", test_ubound_scalar), collapse = " ")
paste(c("Test func. lower bound vector:", test_lbound), collapse = " ")
paste(c("Test func. upper bound vector:", test_ubound), collapse = " ")
paste(c("Test func. argmin:", test_argmin), collapse = " ")

set.seed(seed_value)

# Initialize matrices to hold raw observations (run_obs),
# best observation seen so far (best_so_far),
# the EI value associated with every observation (ei_vals)

run_obs <- matrix(data = NA, nrow = 1, ncol = num_subseq_obs)
best_so_far <- matrix(data = NA, nrow = 1, ncol = num_subseq_obs)
ei_vals <- matrix(data = NA, nrow = 1, ncol = num_subseq_obs)

# Load the initial observation points
# and then evaluate the test function at those points

init_points <- read.table(
  file = init_points_loc,
  header = FALSE,
  sep = "",
  dec = "."
)
init_y <- apply(X = init_points, MARGIN = 1, FUN = test_func)

# Set up the Dice control list

dice_ctrl <- list(
  pop.size = 1024,
  max.generations = 100,
  wait.generations = 10,
  BFGSburnin = 5
)

# Fit Gaussian process model
# and optimize acquisition function

print(sprintf("Fitting gp_model"))
gp_model <- km(
  formula = ~1,
  design = init_points,
  response = init_y,
  covtype = "powexp",
  nugget = 1e-09,
  control = c(dice_ctrl, trace = FALSE),
  optim.method = "gen"
)

print(sprintf("Optimizing acq_func_max"))
acq_func_max <- max_EI(
  model = gp_model,
  type = "UK",
  lower = test_lbound,
  upper = test_ubound,
  control = dice_ctrl
)

# Set up the initial region list

init_region = list(bound_matrix = as.matrix(cbind(test_lbound, test_ubound)),
                   region_x = init_points,
                   region_y = init_y,
                   region_min = min(init_y),
                   region_argmin = init_points[which.min(init_y), ],
                   region_a_max = acq_func_max$value
)

print("Initial region:")
print(init_region)

# Set up region lists
# and keep track of best observation so far

all_regions = list(init_region) # only contains promising regions, i.e. not regions we've rejected
rejected_regions = list() # will contain the regions that we reject
smallest_y_so_far = init_region$region_min
where_smallest_y_so_far = init_region$region_argmin

# Now begin the outer optimization loop

