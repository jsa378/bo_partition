library(GaSP)
library(EGOmod3)

library(GaSP)
library(EGOmod2)

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

source("/home/jsa378/bo_partition/code/test_funcs.R")
source("/home/jsa378/bo_partition/code/new/arbitrary_dim/helper_funcs.R")

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
  file = sprintf("/home/jsa378/bo_partition/code/implementation_testing/init_points/%s_%s_dim_%s_runs_%s_init_points/run_%s_init_points.csv",
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
  x_design = init_pts,
  n_design = num_init_obs,
  x_describe = descr,
  fun = test_func,
  n_rep = 0
)

smallest_y_so_far = min(init$y_design)
entire_region = list(as.matrix(cbind(test_lbound, test_ubound)))
entire_region_points = filter_points_region(entire_region[[1]])
entire_region = c(entire_region, entire_region_points, min(entire_region_points[[2]]))
all_regions = list(entire_region)

n_tot = 100

split = function(region) {
  
  region_x = reg[[2]][[1]]
  region_y = reg[[2]][[2]]
  
  for(d in 1:dim){
    med = median(region_x[, d])
    region_1_x = region_x[region_x[, d] < med, ]
    region_2_x = region_x[region_x[, d] >= med, ]
    
    
    first_region = region
    second_region = region
    # split_midpoint = mean(region[d, ])
    
    first_region[d, 2] = split_midpoint
    second_region[d, 1] = split_midpoint
    
    points_in_first_region = filter_points_region(first_region, x_points_in_region, y_vals_in_region)
    x_points_in_first_region = points_in_first_region[[1]]
    y_vals_in_first_region = points_in_first_region[[2]]
    y_avg_vals_in_first_region = mean(y_vals_in_first_region)
    
    points_in_second_region = filter_points_region(second_region, x_points_in_region, y_vals_in_region)
    x_points_in_second_region = points_in_second_region[[1]]
    y_vals_in_second_region = points_in_second_region[[2]]
    y_avg_vals_in_second_region = mean(y_vals_in_second_region)
    
    if(y_avg_vals_in_first_region < lowest_y_avg_val | y_avg_vals_in_second_region < lowest_y_avg_val){
      lowest_y_avg_val = min(y_avg_vals_in_first_region, y_avg_vals_in_second_region)
      dim_to_split = d
      region_1 = first_region
      region_2 = second_region
      x_points_in_region_1 = x_points_in_first_region
      x_points_in_region_2 = x_points_in_second_region
      y_vals_in_region_1 = y_vals_in_first_region
      y_vals_in_region_2 = y_vals_in_second_region
    }
  }
  
  region_1_model = km(~1,
                      design = x_points_in_region_1,
                      response = y_vals_in_region_1,
                      covtype = "matern5_2",
                      control = km_control_list,
                      optim.method = "gen"
  )
  region_2_model = km(~1,
                      design = x_points_in_region_2,
                      response = y_vals_in_region_2,
                      covtype = "matern5_2",
                      control = km_control_list,
                      optim.method = "gen"
  )
  
  return(list(list(rbind(x_points, new_points_in_region[[1]]),
                   c(y_vals, new_points_in_region[[2]])
  ),
  list(region_1,
       region_2
  ),
  list(region_1_model,
       region_2_model,
  )
  )
  )
}

explore_region <- function(reg,
                           n_max = 10,
                           tol = 1) {
  region_lbound = reg[[1]][, 1]
  region_ubound = reg[[1]][, 2]
  region_x = reg[[2]][[1]]
  region_y = reg[[2]][[2]]
  n = nrow(region_x)
  descr <- DescribeX(
    x_names = x_names_arg,
    x_min = region_lbound,
    x_max = region_ubound,
    support = rep("Continuous", dim)
  )
  while (n <= n_max) {
    init <- Initialize(
      x_design = region_x,
      x_describe = descr,
      fun = test_func
    )
    bo <- EGO(
      fun = test_func,
      reg_model = ~1,
      ego_init = init,
      x_describe = descr,
      nsteps = 1,
      control = ctrl
    )
    region_x = bo$x
    region_y = bo$y
    n = nrow(region_x)
    if (tail(region_y, n = 1) < reg[[3]]) {
      reg[[3]] = tail(region_y, n = 1)
      if (tail(region_y, n = 1) < smallest_y_so_far) {
        smallest_y_so_far = tail(region_y, n = 1)
      }
    }
    if (bo$ac_val_track < tol) {
      reg[[2]][[1]] = region_x
      reg[[2]][[2]] = region_y 
      return(list(reg, smallest_y_so_far))
      # need a way to remove this region from the list
      # my dice_loop.R might be helpful here
    }
  }
  # the while loop completed, so now
  # we need to split the region into 2 subregions
}

end <- Sys.time()
duration <- end - start
print(duration)