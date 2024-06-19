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
  n_design = num_init_obs,
  x_describe = descr,
  fun = test_func,
  n_rep = 0
)

smallest_y_so_far = min(init$y_design)
entire_region = list(bound_matrix = as.matrix(cbind(test_lbound, test_ubound)),
                     region_x = init$x_design,
                     region_y = init$y_design,
                     region_min = min(init$y_design),
                     region_argmin = init$x_design[which.min(init$y_design), ]
                     )
all_regions = list(entire_region)

n_tot = 100

split = function(region) {
  
  region_x = region$region_x
  region_y = region$region_y
  lowest_y_avg_val = 1e+10
  
  for(d in 1:dim){
    med = median(region_x[, d])
    
    region_1_x = region_x[region_x[, d] < med, ]
    region_1_y = region_y[which(region_x[, d] < med)]
    region_2_x = region_x[region_x[, d] >= med, ]
    region_2_y = region_y[which(region_x[, d] >= med)]
    
    region_1_y_avg = mean(region_1_y)
    region_2_y_avg = mean(region_2_y)
    
    if(region_1_y_avg < lowest_y_avg_val | region_2_y_avg < lowest_y_avg_val){
      lowest_y_avg_val = min(region_1_y_avg, region_2_y_avg)
      dim_to_split = d
      split_point = med

      split_region_1_x = region_1_x
      split_region_1_y = region_1_y
      split_region_2_x = region_2_x
      split_region_2_y = region_2_y
    }
  }

  region_1_return = region
  region_1_return$bound_matrix[dim_to_split, 2] = split_point
  region_1_return$region_x = split_region_1_x
  region_1_return$region_y = split_region_1_y
  region_1_return$region_min = min(split_region_1_y)
  region_1_return$region_argmin = split_region_1_x[which.min(split_region_1_y), ]

  region_2_return = region
  region_2_return$bound_matrix[dim_to_split, 1] = split_point
  region_2_return$region_x = split_region_2_x
  region_2_return$region_y = split_region_2_y
  region_2_return$region_min = min(split_region_2_y)
  region_2_return$region_argmin = split_region_2_x[which.min(split_region_2_y), ]
  
  return(list(region_1_return,
              region_2_return)
  )
  # the two regions this function returns
  # need to be added to our list of candidate regions
}

explore_region <- function(region,
                           n_max = 10,
                           tol = 1) {
  region_lbound = region$bound_matrix[, 1]
  region_ubound = region$bound_matrix[, 2]
  region_x = region$region_x
  region_y = region$region_y
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
    new_observed_y = tail(region_y, n = 1)
    if (new_observed_y < region$region_min) {
      region$region_min = new_observed_y
      region$region_argmin = region_x[n, ]
      if (new_observed_y < smallest_y_so_far) {
        smallest_y_so_far = new_observed_y
      }
    }
    a_max = bo$ac_val_track
    
    region$region_x = region_x
    region$region_y = region_y
    region$region_min = min(region_y)
    region$region_argmin = which.min(region_y)
    if (a_max < tol) {
      # i should prepare the updated region and return it
      # and return the smallest y value, i think
      # and remove this region from my list of promising regions
      # my dice_loop.R might be helpful for the last part

      return(list(region,
                  smallest_y_so_far)
             )
    }
  }
  # the while loop completed, so now
  # we need to split the region into 2 subregions
  new_subregions = split(region)
  new_subregion_1 = new_subregions[[1]]
  new_subregion_2 = new_subregions[[2]]
  return(list(new_subregion_1,
              new_subregion_2)
         )
}

end <- Sys.time()
duration <- end - start
print(duration)