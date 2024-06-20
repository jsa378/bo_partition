library(GaSP)
library(EGOmod3)

# args <- commandArgs(trailingOnly = TRUE)
# if (length(args) < 7) {
#   stop("Seven arguments must be supplied:
#   seed value (int),
#   test function (string),
#   dim (int),
#   num init obs (int),
#   num obs (int),
#   num runs (int),
#   save dir (no type)", call. = FALSE)
# }
# 
# seed_value <- as.integer(args[1])
# test_func_name <- args[2]
# dim <- as.integer(args[3])
# num_init_obs <- as.integer(args[4])
# num_obs <- as.integer(args[5])
# num_runs <- as.integer(args[6])
# save_dir <- as.character(args[7])

seed_value = 1
test_func_name = "rastr"
dim = 2
num_init_obs = 20
num_obs = 100
num_runs = 10
save_dir = "/Users/jesse/Downloads/cedar_test_output/research_testing/"

# source("/home/jsa378/bo_partition/code/test_funcs.R")
# source("/home/jsa378/bo_partition/code/new/arbitrary_dim/helper_funcs.R")

source("/Users/jesse/Downloads/bo_partition/code/test_funcs.R")
source("/Users/jesse/Downloads/bo_partition/code/new/arbitrary_dim/helper_funcs.R")

paste(c("Bayesian optimization with seed value:", seed_value), collapse = " ")
paste(c("Test function:", test_func_name), collapse = " ")
paste(c("Dimension:", dim), collapse = " ")
paste(c("Number of initial observations:", num_init_obs), collapse = " ")
paste(c("Number of observations:", num_obs), collapse = " ")
paste(c("Number of runs:", num_runs), collapse = " ")
paste(c("Save directory:", save_dir), collapse = " ")

set.seed(seed_value)

test_func_string <- test_func_name
x_names_arg <- character(0)
for (d in 1:dim){
  x_names_arg <- c(x_names_arg, sprintf("x%s", d))
}
run_obs <- matrix(data = NA, nrow = num_runs, ncol = num_obs)
best_so_far <- matrix(data = NA, nrow = num_runs, ncol = num_obs)

test_func <- test_func_list[[test_func_name]]$func
test_lbound_scalar <- test_func_list[[test_func_name]]$lbound_scalar
test_ubound_scalar <- test_func_list[[test_func_name]]$ubound_scalar
test_lbound <- test_func_list[[test_func_name]]$lbound
test_ubound <- test_func_list[[test_func_name]]$ubound
plot_lims <- c(test_lbound_scalar, test_ubound_scalar)
test_argmin <- test_func_list[[test_func_name]]$argmin

paste(c("Test func. lower bound scalar:", test_lbound_scalar), collapse = " ")
paste(c("Test func. upper bound scalar:", test_ubound_scalar), collapse = " ")
paste(c("Test func. lower bound vector:", test_lbound), collapse = " ")
paste(c("Test func. upper bound vector:", test_ubound), collapse = " ")
paste(c("Test func. argmin:", test_argmin), collapse = " ")

ctrl <- EGO.control(
  alg = "genoud",
  rel_tol = 0,
  wait_iter = 10,
  acq_control = list(type = "EI"),
  GaSP_control = list(cor_family = "Matern"),
  genoud_control = list(pop.size = 1024,
                        max.generations = 100,
                        wait.generations = 10,
                        BFGSburnin = 5,
                        trace = FALSE
  ),
  print_level = 2
)
start <- Sys.time()

init_points <- read.table(
  # file = sprintf("/home/jsa378/bo_partition/code/implementation_testing/init_points/%s_%s_dim_%s_runs_%s_init_points/run_%s_init_points.csv",
  #                test_func_name, dim, num_runs, num_init_obs, seed_value),
  file = sprintf("/Users/jesse/Downloads/bo_partition/code/implementation_testing/init_points/%s_%s_dim_%s_runs_%s_init_points/run_%s_init_points.csv",
                 test_func_name, dim, num_runs, num_init_obs, seed_value),
  header = FALSE,
  sep = "",
  dec = "."
)



# run_obs[run, ] <- bo$y[-(1:num_init_obs)]
# for(obs in 1:num_obs){
#   best_so_far[run, obs] <- min(bo$y[-(1:num_init_obs)][(1:obs)])
# }

descr <- DescribeX(
  x_names = x_names_arg,
  x_min = test_lbound,
  x_max = test_ubound,
  support = rep("Continuous", dim)
)
init <- Initialize(
  x_design = init_points,
  x_describe = descr,
  fun = test_func
)
init_bo <- EGO(
  fun = test_func,
  reg_model = ~1,
  ego_init = init,
  x_describe = descr,
  nsteps = 1,
  control = ctrl
)

init_region = list(bound_matrix = as.matrix(cbind(test_lbound, test_ubound)),
                   region_x = init_bo$x,
                   region_y = init_bo$y,
                   region_min = min(init_bo$y),
                   region_argmin = init_bo$x[which.min(init_bo$y), ],
                   region_a_max = init_bo$ac_val_track
)
all_regions = list(init_region)
smallest_y_so_far = min(init_bo$y)

n_tot = 100

while (length(all_regions) > 0) {
  region_values = matrix(data = NA, nrow = 1, ncol = length((all_regions)))
  for (region_index in 1:length(all_regions)){
    current_region = all_regions[[region_index]]
    region_values[region_index] = current_region$region_min - current_region$region_a_max
  }
  index_of_region_to_explore <- which.min(region_values)
  region_to_explore = all_regions[[index_of_region_to_explore]]
  results = explore_region(region_to_explore)
  # i think I need some way of discerning
  # whether split_and_fit was called during explore_region or not
  # because I may need to remove region_to_explore from all_regions
  if (results$split_called == 0) {
    all_regions = all_regions[-index_of_region_to_explore]
    smallest_y_so_far = results$best_y
  }
  if (results$split_called == 1) {
    all_regions = all_regions[-index_of_region_to_explore]
    new_region_1 = results[[1]]
    new_region_2 = results[[2]]
    all_regions = c(all_regions, list(new_region_1, new_region_2))
    smallest_y_so_far = results$best_y
  }
}

end <- Sys.time()
duration <- end - start
print(duration)