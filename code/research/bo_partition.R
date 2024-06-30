library(GaSP)
library(EGOmod)

# args <- commandArgs(trailingOnly = TRUE)
# if (length(args) < 10) {
#   stop("Ten arguments must be supplied:
#   seed value (int),
#   test function (string),
#   dim (int),
#   num init obs (int),
#   num obs (int),
#   num runs (int),
#   n_max_param (int),
#   tol_param (float),
#   split_crit_param (string),
#   save dir (no type)", call. = FALSE)
# }
# 
# seed_value <- as.integer(args[1])
# test_func_name <- args[2]
# dim <- as.integer(args[3])
# num_init_obs <- as.integer(args[4])
# num_obs <- as.integer(args[5])
# num_runs <- as.integer(args[6])
# n_max_param <- as.integer(args[7])
# tol_param <- as.numeric(args[8])
# split_crit_param <- args[9]
# save_dir <- as.character(args[10])

seed_value = 1
test_func_name = "rastr"
dim = 2
num_init_obs = 20
num_obs = 100
num_runs = 10
n_max_param <- 25
tol_param <- 0.1
split_crit_param <- "avg"
# split_crit_param <- "y_min_minus_a_max"
save_dir = "/Users/jesse/Downloads/cedar_test_output/research_testing/"

# source("/home/jsa378/bo_partition/code/test_funcs.R")
# source("/home/jsa378/bo_partition/code/new/arbitrary_dim/helper_funcs.R")

source("/Users/jesse/Downloads/bo_partition/code/test_funcs.R")
source("/Users/jesse/Downloads/bo_partition/code/research/bo_partition_helper_funcs.R")

paste(c("Bayesian optimization with seed value:", seed_value), collapse = " ")
paste(c("Test function:", test_func_name), collapse = " ")
paste(c("Dimension:", dim), collapse = " ")
paste(c("Number of initial observations:", num_init_obs), collapse = " ")
paste(c("Number of observations:", num_obs), collapse = " ")
paste(c("Number of runs:", num_runs), collapse = " ")
paste(c("n_max parameter:", n_max_param), collapse = " ")
paste(c("tol parameter:", tol_param), collapse = " ")
paste(c("Save directory:", save_dir), collapse = " ")

set.seed(seed_value)

test_func_string <- test_func_name
x_names_arg <- character(0)
for (d in 1:dim){
  x_names_arg <- c(x_names_arg, sprintf("x%s", d))
}
run_obs <- matrix(data = NA, nrow = 1, ncol = num_obs)
best_so_far <- matrix(data = NA, nrow = 1, ncol = num_obs)

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
  GaSP_control = list(cor_family = "PowerExponential"),
  genoud_control = list(pop.size = 1024,
                        max.generations = 100,
                        hard.generation.limit = TRUE,
                        wait.generations = 5,
                        BFGSburnin = 5,
                        print.level = 0,
                        debug = FALSE,
                        trace = FALSE
  ),
  print_level = 0
)
start <- Sys.time()
sink_file <- sprintf("/Users/jesse/Downloads/cedar_test_output/research_testing/bo_partition_test_%s.txt", split_crit_param)
sink(file = sink_file)

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

first_NA_index <- min(which(is.na(run_obs)))
latest_obs <- tail(init_bo$y, n = 1)
run_obs[first_NA_index] <- latest_obs
best_so_far[first_NA_index] <- min(run_obs[(1:first_NA_index)])
sprintf("New observation: %s", latest_obs)
sprintf("Best so far: %s", best_so_far[first_NA_index])

init_region = list(bound_matrix = as.matrix(cbind(test_lbound, test_ubound)),
                   region_x = init_bo$x,
                   region_y = init_bo$y,
                   region_min = min(init_bo$y),
                   region_argmin = init_bo$x[which.min(init_bo$y), ],
                   region_a_max = init_bo$ac_val_track
)
# print("Initial region:")
# print(init_region)
all_regions = list(init_region) # only contains promising regions, i.e. not regions we've rejected
smallest_y_so_far = init_region$region_min
where_smallest_y_so_far = init_region$region_argmin
rejected_regions = list() # contains the regions that we've rejected

n_tot = 100

while (length(all_regions) > 0) {
  region_values = matrix(data = NA, nrow = 1, ncol = length((all_regions)))
  n_obs = min(which(is.na(run_obs))) - 1
  if ( n_obs >= n_tot ) {
    print("Total observation budget reached; terminating.")
    break
  }
  for (region_index in 1:length(all_regions)){
    # n_obs = n_obs + nrow(all_regions[[region_index]]$region_x)
    current_region = all_regions[[region_index]]
    region_values[region_index] = current_region$region_min - current_region$region_a_max
  }
  # if (length(rejected_regions) > 0) {
  #   for (region_index in 1:length(rejected_regions)){
  #     n_obs = n_obs + nrow(rejected_regions[[region_index]]$region_x)
  #   }
  # }
  index_of_region_to_explore <- which.min(region_values)
  region_to_explore = all_regions[[index_of_region_to_explore]]
  print("Region to explore:")
  print(region_to_explore)
  results = explore_region(region = region_to_explore,
                           best_y_so_far = smallest_y_so_far,
                           where_best_y_so_far = where_smallest_y_so_far,
                           run_obs_vec = run_obs,
                           best_so_far_vec = best_so_far,
                           n_max = n_max_param,
                           tol = tol_param,
                           split_crit = split_crit_param)
  
  # I need explore_region to return updated run_obs and best_so_far
  # and then I need to re-bind run_obs and best_so_far to those updated vectors
  
  all_regions = all_regions[-index_of_region_to_explore]
  run_obs <- results$run_obs
  best_so_far <- results$best_so_far
  smallest_y_so_far = results$best_y
  where_smallest_y_so_far = results$where_best_y
  
  if (results$split_called == 0) {
    print("Region rejected, split not called.")
    rejected_regions = c(rejected_regions, list(results$region))
    # all_regions = all_regions[-index_of_region_to_explore]
    # smallest_y_so_far = results$best_y
    # where_smallest_y_so_far = results$where_best_y
  } else if (results$split_called == 1) {
    print("Region split.")
    # all_regions = all_regions[-index_of_region_to_explore]
    new_region_1 = results$new_region_1
    new_region_2 = results$new_region_2
    print("First new subregion:")
    print(new_region_1)
    print("Second new subregion:")
    print(new_region_2)
    
    all_regions = c(all_regions, list(new_region_1, new_region_2))
    # smallest_y_so_far = results$best_y
    # where_smallest_y_so_far = results$where_best_y
  }
}

if (length(all_regions) == 0) {
  print("List of promising regions empty; terminating.")
}

print("Best y observed:")
print(smallest_y_so_far)
print("at location:")
print(where_smallest_y_so_far)

write.table(run_obs,
            file = sprintf("%sbo_partition_seed_%s_obs_%s.csv", save_dir, seed_value, split_crit_param),
            row.names = FALSE,
            col.names = FALSE
)
write.table(best_so_far,
            file = sprintf("%sbo_partition_seed_%s_best_so_far_%s.csv", save_dir, seed_value, split_crit_param),
            row.names = FALSE,
            col.names = FALSE
)

end <- Sys.time()
sink(file = NULL)
duration <- end - start
print(duration)