library(DiceOptim)
library(lhs)
library(doBy)
library(collapse)
library(dplyr)

# Begin timing how long the code takes to run

start <- Sys.time()

# Toggle whether working locally (on MacBook) or remotely (on Cedar)
# This is to set whether parameters are set in this file (local)
# or in a job submission script (remotely)

working <- "remote"

if (working == "remote") {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 12) {
    stop("Twelve arguments must be supplied:
    seed value (int),
    test function (string),
    dim (int),
    num init obs (int),
    num subseq obs (int),
    num runs (int),
    n_max_param (int),
    tol_param (float),
    how_many_EI_points_param (int),
    top_n_EI_vals_param (int),
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
  how_many_EI_points_param <- as.integer(args[9])
  top_n_EI_vals_param <- as.integer(args[10])
  save_dir <- as.character(args[11])
  slurm_job_id <- as.integer(args[12])

  # source("/home/jsa378/bo_partition/code/test_funcs.R")
  # source("/home/jsa378/bo_partition/code/research/bo_partition_helper_funcs.R")
  
  # init_points_loc <- sprintf("/home/jsa378/bo_partition/code/implementation_testing/init_points/%s_%s_dim_%s_runs_%s_init_points/run_%s_init_points.csv",
  #                            test_func_name, dim, num_runs, num_init_obs, seed_value)

  source("/Users/jesse/Downloads/bo_partition/code/test_funcs.R")
  source("/Users/jesse/Downloads/bo_partition/code/research/v3/bo_partition_helper_funcs.R")
  
  init_points_loc <- sprintf("/Users/jesse/Downloads/bo_partition/code/implementation_testing/init_points/%s_%s_dim_%s_runs_%s_init_points/run_%s_init_points.csv",
                             test_func_name, dim, num_runs, num_init_obs, seed_value)
  
  sink_file <- sprintf("/Users/jesse/Downloads/cedar_test_output/26jul24meeting/10runs/2dim_v3/seed_%s/seed_%s.txt", seed_value, seed_value)
  sink(file = sink_file)

} else if (working == "local") {
  seed_value <- 1
  test_func_name <- "rastr"
  dim <- 2
  num_init_obs <- 20
  num_subseq_obs <- 100
  num_runs <- 10
  n_max_param <- 25
  tol_param <- 0.1
  how_many_EI_points_param <- 1000
  top_n_EI_vals_param <- 10
  save_dir <- "/Users/jesse/Downloads/cedar_test_output/research_testing/"
  slurm_job_id <- seed_value
  
  source("/Users/jesse/Downloads/bo_partition/code/test_funcs.R")
  source("/Users/jesse/Downloads/bo_partition/code/research/bo_partition_helper_funcs.R")
  
  init_points_loc <- sprintf("/Users/jesse/Downloads/bo_partition/code/implementation_testing/init_points/%s_%s_dim_%s_runs_%s_init_points/run_%s_init_points.csv",
                             test_func_name, dim, num_runs, num_init_obs, seed_value)
  
  sink_file <- sprintf("/Users/jesse/Downloads/cedar_test_output/research_testing/bo_partition_test.txt")
  sink(file = sink_file)

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
paste(c("Partition-Bayesian optimization v3 (minor changes plus switching) with seed value:", seed_value), collapse = " ")
paste(c("Test function:", test_func_name), collapse = " ")
paste(c("Number of dimensions:", dim), collapse = " ")
paste(c("Number of initial observations:", num_init_obs), collapse = " ")
paste(c("Number of subsequent observations:", num_subseq_obs), collapse = " ")
paste(c("Number of runs:", num_runs), collapse = " ")
paste(c("n_max parameter:", n_max_param), collapse = " ")
paste(c("tol parameter:", tol_param), collapse = " ")
paste(c("how_many_EI_points_param:", how_many_EI_points_param), collapse = " ")
paste(c("top_n_EI_vals_param:", top_n_EI_vals_param), collapse = " ")
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

run_obs <- matrix(data = NA, nrow = 1, ncol = 2 * num_subseq_obs)
best_so_far <- matrix(data = NA, nrow = 1, ncol = 2 * num_subseq_obs)
ei_vals <- matrix(data = NA, nrow = 1, ncol = 2 * num_subseq_obs)

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

# The region_a_par value is only created
# when creating a new region,
# so the initial region here and
# when splitting a region, at the end
# of split_and_fit()

# It is used to take the initial sample
# at the beginning of explore_region().
# region_a_par is never updated,
# because I don't think doing so
# would serve any purpose;
# once a region goes into
# explore_region(), we will never
# explore it again, since we either
# reach our total observation budget,
# we reject the region, or
# we split the region

init_region = list(bound_matrix = as.matrix(cbind(test_lbound, test_ubound)),
                   region_x = init_points,
                   region_y = init_y,
                   region_min = min(init_y),
                   region_argmin = init_points[which.min(init_y), ],
                   region_a_max = acq_func_max$value,
                   region_a_par = acq_func_max$par
)

print("Initial region:")
print(init_region)

# Keep track of best observation so far
# and where it was observed

smallest_y_so_far = init_region$region_min
where_smallest_y_so_far = init_region$region_argmin

# Set up region lists

all_regions = list(init_region) # only contains promising regions, i.e. not regions we've rejected
rejected_regions = list() # will contain the regions that we reject

# Experimenting with adding points to gp_model
# The means don't seem to change

# test_point = matrix(data=NA, nrow=2, ncol=2)
# test_point[1, ] <- c(1, 1)
# test_point[2, ] <- c(-1, -1)
# colnames(test_point) <- c("V1", "V2")
# 
# pred <- predict(object = gp_model, newdata = test_point, type = "UK")
# pred$mean
# 
# gp_model@X <- rbind(gp_model@X, acq_func_max$par)
# gp_model@y <- rbind(gp_model@y, test_func(acq_func_max$par))
# gp_model@n <- as.integer(gp_model@n + 1)
# 
# pred <- predict(object = gp_model, newdata = test_point, type = "UK")
# pred$mean

# Now begin the outer optimization loop
# Loop as long as there are promising regions to explore
# (while we're within our total observation budget)

while (length(all_regions) > 0) {
  
  # Calculate how many observations have been taken

  n_obs = min(which(is.na(run_obs))) - 1
  
  # If we reach our observation budget (num_subseq_obs),
  # exit this while loop,
  # else we save our partial progress and continue
  
  if (n_obs >= num_subseq_obs) {
    
    print("Total observation budget reached or exceeded; terminating.")
    break
    
  } else {
    
    print(sprintf("Taken %s observations out of a total budget of %s; continuing.",
                  n_obs, num_subseq_obs))
    print("Saving partial progress.")
    
    write.table(run_obs[1:(n_obs)],
                file = sprintf("%sbo_partition_seed_%s_obs.csv", save_dir, seed_value),
                row.names = FALSE,
                col.names = FALSE
    )
    write.table(best_so_far[1:(n_obs)],
                file = sprintf("%sbo_partition_seed_%s_best_so_far.csv", save_dir, seed_value),
                row.names = FALSE,
                col.names = FALSE
    )
    write.table(ei_vals[1:(n_obs)],
                file = sprintf("%sbo_partition_seed_%s_ei_vals.csv", save_dir, seed_value),
                row.names = FALSE,
                col.names = FALSE
    )
  }
  
  # Set up vector to hold region values
  # and loop over the region list,
  # putting the region values in the vector
  
  # I gather the region bounds just to make
  # it easier to tell in the print-out
  # which region values correspond to which region
  
  region_values = matrix(data = NA, nrow = 1, ncol = length(all_regions))
  
  for (region_index in 1:length(all_regions)){
    
    current_region = all_regions[[region_index]]
    region_values[region_index] <- current_region$region_a_max
    
    if (region_index == 1) {
      
      region_bounds <- current_region$bound_matrix
      
    } else {
      
      region_bounds <- cbind(region_bounds, current_region$bound_matrix)
      
    }
    
  }
  
  print("The region bounds are:")
  print(region_bounds)
  
  print("The corresponding region values (a_maxes) are:")
  print(region_values)
  
  # Select the region to explore
  
  # We also want the second-highest region value
  # to feed to explore_region (for switching)
  # If there is only one promising region,
  # this is moot, so set it to -1,
  # otherwise we extract the
  # second-highest region value
  
  region_indices <- which.maxn(region_values, n = 2)
  index_of_region_to_explore <- region_indices[1]
  
  region_to_explore = all_regions[[index_of_region_to_explore]]
  
  if (length(region_indices) == 1) {
    
    next_highest_a_max <- -1
    
  } else {
    
    index_of_second_best_region <- region_indices[2]
    second_best_region <- all_regions[[index_of_second_best_region]]
    
    next_highest_a_max <- second_best_region$region_a_max
    
  }

  print("Region to explore:")
  print(region_to_explore)
  
  print(sprintf("Next highest a_max value: %s", next_highest_a_max))
  
  # Send the region chosen for exploration to explore_region()
  
  results <- explore_region(region = region_to_explore,
                           next_highest_a_max = next_highest_a_max,
                           best_y_so_far = smallest_y_so_far,
                           where_best_y_so_far = where_smallest_y_so_far,
                           run_obs_vec = run_obs,
                           best_so_far_vec = best_so_far,
                           ei_vals_vec = ei_vals,
                           n_max = n_max_param,
                           num_obs_so_far = n_obs,
                           tol = tol_param)
  
  # Update observation records
  # with output from explore_region()
  
  run_obs <- results$run_obs
  best_so_far <- results$best_so_far
  ei_vals <- results$ei_vals
  smallest_y_so_far <- results$best_y
  where_smallest_y_so_far <- results$where_best_y
  
  # If explore_region() returned because
  # we reached our observation budget,
  # then we need to update the region, and then
  # go directly to the top of the while loop
  
  # Same control flow if explore_region()
  # returned because we're switching regions
  
  if (results$num_obs_exceeded == 1 || results$switch == 1) {
    
    all_regions[[index_of_region_to_explore]] <- results$region
    next

  }
  
  # If explore_region() returned because
  # we either rejected or split the region,
  # in either case we remove the explored region
  # from the list of promising regions

  all_regions <- all_regions[-index_of_region_to_explore]
  
  # If explore_region() returned because
  # we rejected the region,
  # we put the explored region
  # in the list of rejected regions
  
  # If explore_region() returned because
  # we split the region,
  # we put the two new regions
  # in the list of promising regions

  if (results$reject == 1) {
    
    print("Region rejected, split not called.")
    rejected_regions <- c(rejected_regions, list(results$region))
    
  } else if (results$split_called == 1) {
    
    print("Region split.")
    
    new_region_1 <- results$new_region_1
    new_region_2 <- results$new_region_2
    
    print("First new subregion:")
    print(new_region_1)
    print("Second new subregion:")
    print(new_region_2)
    
    all_regions <- c(all_regions, list(new_region_1, new_region_2))
  }
}

# At this point, the Bayesian optimization is done

# If there were still promising regions
# remaining at termination, we print them out

if (length(all_regions) == 0) {
  
  print("List of promising regions empty; terminating.")
  
} else if (length(all_regions) > 0) {
  
  print(sprintf("Number of promising regions remaining at termination: %s",
                length(all_regions)))
  print("Printing each promising region.")
  
  for (region_index in 1:length(all_regions)) {
    
    print(sprintf("Promising region number %s:", region_index))
    print(all_regions[[region_index]])
    
  }
  
}

# If we rejected any regions
# during the while loop,
# we print those out as well

if (length(rejected_regions) == 0) {
  
  print("No regions were rejected during optimization")
  
} else if (length(rejected_regions) > 0) {
  
  print(sprintf("Number of regions rejected during optimization: %s", length(rejected_regions)))
  print("Printing each rejected region")
  
  for (region_index in 1:length(rejected_regions)) {
    
    print(sprintf("Rejected region number %s:", region_index))
    print(rejected_regions[[region_index]])
    
  }
  
}

# Print out the best value we found
# and where we found it

print("Best y observed:")
print(smallest_y_so_far)

print("at location:")
print(where_smallest_y_so_far)

# Now we check how many observations we gathered,
# since we may have quit before reaching num_subseq_obs

n_obs = min(which(is.na(run_obs))) - 1

print(sprintf("Used %s out of a total budget of %s observations.", n_obs, num_subseq_obs))

# Now we save the trimmed record vectors for plotting

write.table(run_obs[1:(n_obs)],
            file = sprintf("%sbo_partition_seed_%s_obs.csv", save_dir, seed_value),
            row.names = FALSE,
            col.names = FALSE
)
write.table(best_so_far[1:(n_obs)],
            file = sprintf("%sbo_partition_seed_%s_best_so_far.csv", save_dir, seed_value),
            row.names = FALSE,
            col.names = FALSE
)
write.table(ei_vals[1:(n_obs)],
            file = sprintf("%sbo_partition_seed_%s_ei_vals.csv", save_dir, seed_value),
            row.names = FALSE,
            col.names = FALSE
)

# Compute the duration of this code and print it out

end <- Sys.time()
duration <- end - start

print("Partition-Bayesian optimization complete in:")
print(duration)

# If we are working locally,
# we terminate the sink file

if (working == "local") {
  sink(file = NULL)
}
sink(file = NULL)