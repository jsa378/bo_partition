# library(DiceOptim)
library(GaSP)

start <- Sys.time()

working <- "remote"

if (working == "remote") {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 9) {
    stop("Nine arguments must be supplied:
    seed value (int),
    test function (string),
    dim (int),
    num init obs (int),
    num runs (int),
    save dir (no type),
    slurm job id (int),
    covtype (string),
    nugget (float)", call. = FALSE)
  }
  seed_value <- as.integer(args[1])
  test_func_name <- args[2]
  dim <- as.integer(args[3])
  num_init_obs <- as.integer(args[4])
  num_runs <- as.integer(args[5])
  save_dir <- as.character(args[6])
  slurm_job_id <- as.integer(args[7])
  covtype_param <- args[8]
  nugget_param <- as.numeric(args[9])
  
  source("/home/jsa378/bo_partition/code/test_funcs.R")
  
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
paste(c("Test function:", test_func_name), collapse = " ")
paste(c("Number of dimensions:", dim), collapse = " ")
paste(c("Number of initial observations:", num_init_obs), collapse = " ")
paste(c("Number of runs:", num_runs), collapse = " ")
paste(c("Save directory:", save_dir), collapse = " ")
paste(c("Kernel type:", covtype_param), collapse = " ")
paste(c("Nugget value", nugget_param), collapse = " ")
paste(c("Test func. lower bound scalar:", test_lbound_scalar), collapse = " ")
paste(c("Test func. upper bound scalar:", test_ubound_scalar), collapse = " ")
paste(c("Test func. lower bound vector:", test_lbound), collapse = " ")
paste(c("Test func. upper bound vector:", test_ubound), collapse = " ")
paste(c("Test func. argmin:", test_argmin), collapse = " ")

set.seed(seed_value)

# range_params <- matrix(data = NA, nrow = num_runs, ncol = dim)
theta_params <- matrix(data = NA, nrow = num_runs, ncol = dim)

# dice_ctrl <- list(
#   pop.size = 1024,
#   max.generations = 100,
#   wait.generations = 10,
#   BFGSburnin = 5
# )

for (run in 1:num_runs) {
  
  print(sprintf("Beginning run %s of %s", run, num_runs))
  
  init_points_loc <- sprintf("/home/jsa378/bo_partition/code/implementation_testing/init_points/%s_%s_dim_%s_runs_%s_init_points/run_%s_init_points.csv",
                             test_func_name, dim, num_runs, num_init_obs, run)
  
  init_points <- read.table(
    file = init_points_loc,
    header = FALSE,
    sep = "",
    dec = "."
  )
  init_y <- apply(X = init_points, MARGIN = 1, FUN = test_func)
  
  print("The init points are:")
  print(init_points)
  
  print(sprintf("Fitting gp_model"))
  # gp_model <- km(
  #   formula = ~1,
  #   design = init_points,
  #   response = init_y,
  #   covtype = covtype_param,
  #   nugget = nugget_param,
  #   control = c(dice_ctrl, trace = FALSE),
  #   optim.method = "gen"
  # )
  gp_model <- Fit(
    x = init_points,
    y = init_y,
    reg_model = ~1,
    cor_family = covtype_param,
    random_error = FALSE,
    nugget = nugget_param,
    fit_objective = "Likelihood",
    alpha_max = 0,
    model_comparison = "Objective"
  )
  
  print("The range parameters for this set of init points is:")
  # print(gp_model@covariance@range.val)
  print(gp_model$cor_par$Theta)
  # range_params[run, ] <- gp_model@covariance@range.val
  theta_params[run, ] <- gp_model$cor_par$Theta
  
  print("Saving partial progress.")
  
  write.table(theta_params[1:run, ], # range_params[1:run, ],
              file = sprintf("%s%s_init_obs.csv", save_dir, num_init_obs),
              row.names = FALSE,
              col.names = FALSE
  )
  
}

print("The range parameter values (unscaled) are:")
# print(range_params)
print(theta_params)

end <- Sys.time()
duration <- end - start

print("Complete in:")
print(duration)