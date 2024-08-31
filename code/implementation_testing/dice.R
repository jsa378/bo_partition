# library(GaSP)
# library(EGOmod)
library(DiceOptim)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 10) {
  stop("Ten arguments must be supplied:
  seed value (int),
  test function (string),
  dim (int),
  num init obs (int),
  num subseq obs (int),
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
num_subseq_obs <- as.integer(args[5])
num_runs <- as.integer(args[6])
save_dir <- as.character(args[7])
slurm_job_id <- as.integer(args[8])
covtype_param <- args[9]
nugget_param <- as.numeric(args[10])

# A function (and some settings) to dump an .rda file
# in the event of an R error (for debugging purposes)

dump_and_quit <- function() {
  # Save debugging info to file last.dump.rda
  dump.frames(dumpto = sprintf("last.dump%s", slurm_job_id), to.file = TRUE)
  # Quit R with error status
  q(status = 1)
}
options(error = dump_and_quit, CBoundsCheck = TRUE)

working <- "remote"

if (working == "local") {
  
  source("/Users/jesse/Downloads/bo_partition/code/test_funcs.R")
  
  init_points_loc <- sprintf("/Users/jesse/Downloads/bo_partition/code/implementation_testing/init_points/%s_%s_dim_%s_runs_%s_init_points/run_%s_init_points.csv",
                             test_func_name, dim, num_runs, num_init_obs, seed_value)
    
} else if (working == "remote") {
  
  source("/home/jsa378/bo_partition/code/test_funcs.R")
  
  init_points_loc <- sprintf("/home/jsa378/bo_partition/code/implementation_testing/init_points/%s_%s_dim_%s_runs_%s_init_points/run_%s_init_points.csv",
          test_func_name, dim, num_runs, num_init_obs, seed_value)
  
}

  # sink_file <- sprintf("/Users/jesse/Downloads/cedar_test_output/26jul24meeting/10runs/2dim_reg_dice/seed_%s/seed_%s.txt", seed_value, seed_value)
  # sink(file = sink_file)

# source("/home/jsa378/bo_partition/code/new/arbitrary_dim/helper_funcs.R")

paste(c("Bayesian optimization with seed value:", seed_value), collapse = " ")
paste(c("Test function:", test_func_name), collapse = " ")
paste(c("Dimension:", dim), collapse = " ")
paste(c("Number of initial observations:", num_init_obs), collapse = " ")
paste(c("Number of subsequent observations:", num_subseq_obs), collapse = " ")
paste(c("Number of runs:", num_runs), collapse = " ")
paste(c("Save directory:", save_dir), collapse = " ")
paste(c("Kernel type:", covtype_param), collapse = " ")
paste(c("Nugget value", nugget_param), collapse = " ")

set.seed(seed_value)

# test_func_string <- test_func_name
# x_names_arg <- character(0)
# for (d in 1:dim){
#   x_names_arg <- c(x_names_arg, sprintf("x%s", d))
# }
run_obs <- matrix(data = NA, nrow = 1, ncol = num_subseq_obs)
best_so_far <- matrix(data = NA, nrow = 1, ncol = num_subseq_obs)
ei_vals <- matrix(data = NA, nrow = 1, ncol = num_subseq_obs)
thetas <- matrix(data = NA, nrow = num_subseq_obs + 1, ncol = dim)

test_func <- test_func_list[[test_func_name]]$func
test_lbound_scalar <- test_func_list[[test_func_name]]$lbound_scalar
test_ubound_scalar <- test_func_list[[test_func_name]]$ubound_scalar
test_lbound <- test_func_list[[test_func_name]]$lbound
test_ubound <- test_func_list[[test_func_name]]$ubound
# plot_lims <- c(test_lbound_scalar, test_ubound_scalar)
test_argmin <- test_func_list[[test_func_name]]$argmin

paste(c("Test func. lower bound scalar:", test_lbound_scalar), collapse = " ")
paste(c("Test func. upper bound scalar:", test_ubound_scalar), collapse = " ")
paste(c("Test func. lower bound vector:", test_lbound), collapse = " ")
paste(c("Test func. upper bound vector:", test_ubound), collapse = " ")
paste(c("Test func. argmin:", test_argmin), collapse = " ")

# descr <- DescribeX(
#   x_names = x_names_arg,
#   x_min = test_lbound,
#   x_max = test_ubound,
#   support = rep("Continuous", dim)
# )

dice_ctrl <- list(
  pop.size = 1024,
  max.generations = 100,
  wait.generations = 10,
  BFGSburnin = 5
)

start <- Sys.time()

x_points <- read.table(
  file = init_points_loc,
  header = FALSE,
  sep = "",
  dec = "."
)
y_points <- apply(X = x_points, MARGIN = 1, FUN = test_func)

print("Initial x and y points:")
print(cbind(x_points, y_points))

# init <- Initialize(
#   x_design = init_points,
#   x_describe = descr,
#   fun = test_func
# )

# km_x <- init$x_design
# km_y <- init$y_design

gp_model <- km(
  formula = ~1,
  design = x_points,
  response = y_points,
  covtype = covtype_param,
  nugget = nugget_param,
  control = c(dice_ctrl, trace = FALSE),
  optim.method = "gen"
)

print("The theta values for the initial model are:")
print(gp_model@covariance@range.val)

thetas[1, ] <- gp_model@covariance@range.val

# dice_bo <- EGO.nsteps(
#   model = gp_model,
#   fun = test_func,
#   nsteps = num_subseq_obs,
#   lower = test_lbound,
#   upper = test_ubound,
#   control = dice_ctrl
# )

for (i in 1:num_subseq_obs) {
  
  acq_func_max <- max_EI(
    model = gp_model,
    type = "UK",
    lower = test_lbound,
    upper = test_ubound,
    control = dice_ctrl
  )
  
  new_obs <- test_func(acq_func_max$par)
  
  print(paste(c("New observation ", new_obs, "at location ", acq_func_max$par,
                 "with EI value ", acq_func_max$value)))
  
  x_points <- rbind(x_points, acq_func_max$par)
  y_points <- c(y_points, new_obs)
  
  run_obs[1, i] <- new_obs
  best_so_far[1, i] <- min(run_obs[1, (1:i)])
  ei_vals[1, i] <- acq_func_max$value
    
  print(sprintf("Taken %s observations out of a total budget of %s; continuing.",
                i, num_subseq_obs))
  print("Saving partial progress.")
  
  write.table(run_obs[1:i],
              file = sprintf("%sseed_%s_obs.csv", save_dir, seed_value),
              row.names = FALSE,
              col.names = FALSE
  )
  write.table(best_so_far[1:i],
              file = sprintf("%sseed_%s_best_so_far.csv", save_dir, seed_value),
              row.names = FALSE,
              col.names = FALSE
  )
  write.table(ei_vals[1:i],
              file = sprintf("%sseed_%s_ei_vals.csv", save_dir, seed_value),
              row.names = FALSE,
              col.names = FALSE
  )
  
  gp_model <- km(
    formula = ~1,
    design = x_points,
    response = y_points,
    covtype = covtype_param,
    nugget = nugget_param,
    control = c(dice_ctrl, trace = FALSE),
    optim.method = "gen"
  )

  print("The theta values for the current model are:")
  print(gp_model@covariance@range.val)

  thetas[i + 1, ] <- gp_model@covariance@range.val

  write.table(thetas[1:(i + 1), ],
            file = sprintf("%sseed_%s_thetas.csv", save_dir, seed_value),
            row.names = FALSE,
            col.names = FALSE
  )

}

# run_obs[1, ] <- dice_bo$value
# for(obs in 1:num_subseq_obs){
#   best_so_far[obs] <- min(dice_bo$value[(1:obs)])
# }

# write.table(run_obs,
#   file = sprintf("%sseed_%s_obs.csv", save_dir, seed_value),
#   row.names = FALSE,
#   col.names = FALSE
# )
# write.table(best_so_far,
#   file = sprintf("%sseed_%s_best_so_far.csv", save_dir, seed_value),
#   row.names = FALSE,
#   col.names = FALSE
# )

print("The x points are:")
print(x_points)

print("The y points are:")
print(y_points)

print("Bayesian optimization done.")

end <- Sys.time()
duration <- end - start
print(duration)

if (working == "local") {
  sink(file = NULL)
}
