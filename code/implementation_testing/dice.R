# library(GaSP)
# library(EGOmod)
library(DiceOptim)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 7) {
  stop("Seven arguments must be supplied:
  seed value (int),
  test function (string),
  dim (int),
  num init obs (int),
  num obs (int),
  num runs (int),
  save dir (no type)", call. = FALSE)
}

seed_value <- as.integer(args[1])
test_func_name <- args[2]
dim <- as.integer(args[3])
num_init_obs <- as.integer(args[4])
num_obs <- as.integer(args[5])
num_runs <- as.integer(args[6])
save_dir <- as.character(args[7])

working <- "local"

if (working == "local") {
  
  source("/Users/jesse/Downloads/bo_partition/code/test_funcs.R")
  
  init_points_loc <- sprintf("/Users/jesse/Downloads/bo_partition/code/implementation_testing/init_points/%s_%s_dim_%s_runs_%s_init_points/run_%s_init_points.csv",
                             test_func_name, dim, num_runs, num_init_obs, seed_value)
    
} else if (working == "remote") {
  
  source("/home/jsa378/bo_partition/code/test_funcs.R")
  
  init_points_loc <- sprintf("/home/jsa378/bo_partition/code/implementation_testing/init_points/%s_%s_dim_%s_runs_%s_init_points/run_%s_init_points.csv",
          test_func_name, dim, num_runs, num_init_obs, seed_value)
  
}


# source("/home/jsa378/bo_partition/code/new/arbitrary_dim/helper_funcs.R")

paste(c("Bayesian optimization with seed value:", seed_value), collapse = " ")
paste(c("Test function:", test_func_name), collapse = " ")
paste(c("Dimension:", dim), collapse = " ")
paste(c("Number of initial observations:", num_init_obs), collapse = " ")
paste(c("Number of observations:", num_obs), collapse = " ")
paste(c("Number of runs:", num_runs), collapse = " ")
paste(c("Save directory:", save_dir), collapse = " ")

set.seed(seed_value)

# test_func_string <- test_func_name
# x_names_arg <- character(0)
# for (d in 1:dim){
#   x_names_arg <- c(x_names_arg, sprintf("x%s", d))
# }
run_obs <- matrix(data = NA, nrow = 1, ncol = num_obs)
best_so_far <- matrix(data = NA, nrow = 1, ncol = num_obs)

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

init_points <- read.table(
  file = init_points_loc,
  header = FALSE,
  sep = "",
  dec = "."
)
init_y <- apply(X = init_points, MARGIN = 1, FUN = test_func)

# init <- Initialize(
#   x_design = init_points,
#   x_describe = descr,
#   fun = test_func
# )

# km_x <- init$x_design
# km_y <- init$y_design

gp_model <- km(
  formula = ~1,
  design = init_points,
  response = init_y,
  covtype = "powexp",
  nugget = 1e-09,
  control = c(dice_ctrl, trace = FALSE),
  optim.method = "gen"
)
dice_bo <- EGO.nsteps(
  model = gp_model,
  fun = test_func,
  nsteps = num_obs,
  lower = test_lbound,
  upper = test_ubound,
  control = dice_ctrl
)

run_obs[1, ] <- dice_bo$value
for(obs in 1:num_obs){
  best_so_far[obs] <- min(dice_bo$value[(1:obs)])
}

write.table(run_obs,
  file = sprintf("%sseed_%s_obs.csv", save_dir, seed_value),
  row.names = FALSE,
  col.names = FALSE
)
write.table(best_so_far,
  file = sprintf("%sseed_%s_best_so_far.csv", save_dir, seed_value),
  row.names = FALSE,
  col.names = FALSE
)

end <- Sys.time()
duration <- end - start
print(duration)
