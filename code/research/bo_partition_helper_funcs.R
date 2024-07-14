update_records <- function(region,
                           best_y_so_far,
                           where_best_y_so_far,
                           run_obs_vec,
                           best_so_far_vec,
                           ei_vals_vec,
                           new_x,
                           new_y,
                           new_ei_vals) {
  
  # This function has been written
  # to accept an arbitrary number of new observations
  
  # Update the region$region_x, region$region_y,
  # and region$region_a_max
  
  region$region_x <- rbind(region$region_x, new_x)
  region$region_y <- c(region$region_y, new_y)
  
  last_new_ei_val <- tail(new_ei_vals, n = 1)
  region$region_a_max <- last_new_ei_val
  
  # Find the index in our records
  # at which we will begin adding
  # the new points,
  # and find how many new points we have
  
  first_NA_index <- min(which(is.na(run_obs_vec)))
  num_new_obs <- length(new_y)
  
  # Update run_obs_vec with the new observations
  # and update ei_vals_vec with the new EI values
  
  run_obs_vec[first_NA_index:((first_NA_index + num_new_obs) - 1)] <- new_y
  ei_vals_vec[first_NA_index:((first_NA_index + num_new_obs) - 1)] <- new_ei_vals
  
  # To update best_so_var_vec,
  # we loop over the new observations,
  # each time adding the min so far
  
  for (new_obs_index in 1:num_new_obs) {
    
    best_so_far_vec[first_NA_index + new_obs_index - 1] <- min(run_obs_vec[1:(first_NA_index + new_obs_index - 1)])
    
  }
  
  # Find the best of the new observations,
  # and where it was found
  
  best_new_y <- min(new_y)
  where_best_new_y <- new_x[which.min(new_y)]
  
  # If the best of the new observations
  # is better than the region best,
  # update the region records
  
  if (best_new_y < region$region_min) {
    
    print(sprintf("New region best observed: %s at %s", best_new_y, where_best_new_y))
    
    region$region_min <- best_new_y
    region$region_argmin <- where_best_new_y
    
  }
  
  # If the best of the new observations
  # is better than the overall best,
  # update the overall records
  
  if (best_new_y < best_y_so_far) {
    
    print(sprintf("New overall best observed: %s at %s", best_new_y, where_best_new_y))
    
    best_y_so_far <- best_new_y
    where_best_y_so_far <- where_best_new_y
    
  }
  
  return(list(region = region,
              new_best_y = best_y_so_far,
              where_new_best_y = where_best_y_so_far,
              run_obs = run_obs_vec,
              best_so_far = best_so_far_vec,
              ei_vals = ei_vals_vec
              )
         )
}

explore_region <- function(region,
                           best_y_so_far,
                           where_best_y_so_far,
                           run_obs_vec,
                           best_so_far_vec,
                           ei_vals_vec,
                           n_max = 10 * dim,
                           num_obs_so_far,
                           tol = 0.1) {
  
  # Binding the x and y points
  # and computing how many observations
  # we have in this region
  
  region_x = region$region_x
  region_y = region$region_y
  
  n = nrow(region_x)
  
  # Begin the main explore_region() loop
  # In this loop, we do Bayesian optimization,
  # subject to:
  # the max number of observations we will take in any region, n_max;
  # our overall observation budget, num_subseq_obs;
  # and our tolerance for the EI values, tol
  
  while (n < n_max) {
    
    # Fit the Gaussian process model

    print(sprintf("Fitting Gaussian process model"))
    
    gp_model <- km(
      formula = ~1,
      design = region_x,
      response = region_y,
      covtype = "powexp",
      nugget = 1e-09,
      control = c(dice_ctrl, trace = FALSE),
      optim.method = "gen"
    )
    
    # Feed the Gaussian process model
    # to the acquisition-function maximizer
    
    print(sprintf("Optimizing acquisition function"))
    
    acq_func_max <- max_EI(
      model = gp_model,
      type = "UK",
      lower = region$bound_matrix[, 1],
      upper = region$bound_matrix[, 2],
      control = dice_ctrl
    )
    
    # Bind the new data
    # related to the new acquisition
    
    new_x <- acq_func_max$par
    new_y <- test_func(new_x)
    new_ei_val <- acq_func_max$value
    
    print(paste(c("New observation ", new_y, " at location ", new_x,
                  " with EI value ", new_ei_val)))
    
    # Update our records
    
    update <- update_records(region = region,
                             best_y_so_far = best_y_so_far,
                             where_best_y_so_far = where_best_y_so_far,
                             run_obs_vec = run_obs_vec,
                             best_so_far_vec = best_so_far_vec,
                             ei_vals_vec = ei_vals_vec,
                             new_x = new_x,
                             new_y = new_y,
                             new_ei_vals = new_ei_val
    )
    
    region <- update$region
    
    best_y_so_far <- update$new_best_y
    where_best_y_so_far <- update$where_new_best_y
    
    run_obs_vec <- update$run_obs
    best_so_far_vec <- update$best_so_far
    ei_vals_vec <- update$ei_vals
    
    # Re-bind region_x and region_y
    # for the next Gaussian process model fit
    # in light of the update
    
    region_x <- region$region_x
    region_y <- region$region_y
    
    # Update our observation counters
    
    n <- n + 1
    num_obs_so_far <- num_obs_so_far + 1
    
    print(paste(c("Taken ", n, "observations in region, with maximum ", n_max), collapse = " "))
    print(paste(c("Taken ", num_obs_so_far, "observations overall, with maximum ", num_subseq_obs), collapse = " "))

    # Investigate the stopping conditions for this while loop
    # First we check if we've met our total observation budget, num_subseq_obs
    
    if (num_obs_so_far >= num_subseq_obs) {
      
      print("Total observation budget reached while exploring region, so returning region.")
      
      return(list(region = region,
                  best_y = best_y_so_far,
                  where_best_y = where_best_y_so_far,
                  run_obs = run_obs_vec,
                  best_so_far = best_so_far_vec,
                  ei_vals = ei_vals_vec,
                  num_obs_exceeded = 1)
      )
    }
    
    # Next, we check if the latest EI value
    # is below our tolerance threshold
    
    if (new_ei_val < tol) {
      
      print(sprintf("new_ei_val (%s) is less than tol (%s), so rejecting region.", new_ei_val, tol))
      
      return(list(region = region,
                  best_y = best_y_so_far,
                  where_best_y = where_best_y_so_far,
                  run_obs = run_obs_vec,
                  best_so_far = best_so_far_vec,
                  ei_vals = ei_vals_vec,
                  num_obs_exceeded = 0,
                  split_called = 0)
      )
    }
  
  }
  
  # If the control flow reaches this point,
  # the region we've been exploring is still promising,
  # but we have too many points, so we're going to split the region
  
  print("Maximum number of observations in region (n_max) reached; splitting region")
  
  # We fit one new Gaussian process model
  # that we will pass to split_and_fit()
  
  print(sprintf("Fitting Gaussian process model to pass to split_and_fit()"))
  
  region_model <- km(
    formula = ~1,
    design = region_x,
    response = region_y,
    covtype = "powexp",
    nugget = 1e-09,
    control = c(dice_ctrl, trace = FALSE),
    optim.method = "gen"
  )
  
  # Now we pass everything to split_and_fit()
  # and return to the main while loop
  
  print("Splitting region")
  
  new_subregions = split_and_fit(region = region,
                                 region_model = region_model)
  
  new_subregion_1 = new_subregions$region_1
  new_subregion_2 = new_subregions$region_2
  
  return(list(new_region_1 = new_subregion_1,
              new_region_2 = new_subregion_2,
              best_y = best_y_so_far,
              where_best_y = where_best_y_so_far,
              run_obs = run_obs_vec,
              best_so_far = best_so_far_vec,
              ei_vals = ei_vals_vec,
              num_obs_exceeded = 0,
              split_called = 1)
         )
  
}

split_and_fit <- function(region,
                          region_model) {
  
  # Begin timing how long split_and_fit() takes to run
  
  start <- Sys.time()
  print(sprintf("Beginning split_and_fit"))
  
  # We need to prepare
  # the points at which we will evaluate EI,
  # and then evaluate EI at those points
  
  # First we generate the points
  
  set.seed(1)
  dim <- 2
  
  # ei_points <- randomLHS(5, dim)
  ei_points <- rbind(c(0.25, 0.25), c(0.25, 0.75), c(0.75, 0.25), c(0.75, 0.75))
  scaled_shifted_ei_points <- matrix(data = NA, nrow = 4, ncol = dim)
  
  region_bound_matrix <- rbind(c(-5, 5), c(-5, 5))
  
  # Then we have to scale and shift them to cover our region
  
  scaling_vector <- rep(0, dim)
  shifting_vector <- rep(0, dim)
  
  for (d in 1:dim) {
    
    # The dth coordinate of an LHS point
    # get scaled by the length of the region in dimension d
    
    # region_dimension_d_length <- region$bound_matrix[d, 2] - region$bound_matrix[d, 1]
    region_dimension_d_length <- region_bound_matrix[d, 2] - region_bound_matrix[d, 1]
    scaling_vector[d] <- region_dimension_d_length
    
    scaled_shifted_ei_points[, d] <- ei_points[, d] * scaling_vector[d]
    
    # Next, we find the midpoint of the region 
    
    # region_dimension_d_midpoint_coord <- region$bound_matrix[d, 1] + (scaling_vector[d] / 2)
    region_dimension_d_midpoint_coord <- region_bound_matrix[d, 1] + (scaling_vector[d] / 2)
    shifting_vector[d] <- region_dimension_d_midpoint_coord

  }
  
  # Now that the LHS hypercube has the correct dimensions,
  # we shift it to the origin,
  # and then shift it to overlap the region
  
  scaled_shifted_ei_points <- scaled_shifted_ei_points - (scaling_vector / 2)
  scaled_shifted_ei_points <- scaled_shifted_ei_points + shifting_vector
  
  # Next, we evaluate EI for each of the (scaled, shifted) LHS points
  # and find which indices correspond to the 10 biggest EI values
  
  EI_values <- apply(X = scaled_shifted_ei_points, MARGIN = 1, FUN = EI, region_model)
  biggest_ei_vals_indices <- which.maxn(EI_values, n = 10)
  biggest_ei_vals_coords <- scaled_shifted_ei_points[biggest_ei_vals_indices]
  
  # I need to test all of the above carefully
  
  end <- Sys.time()
  duration <- end - start
  
  print("Region split in:")
  print(duration)

  print(sprintf("Returning region_1 and region_2 from split_and_fit"))

  return(list(region_1 = region_1_return,
              region_2 = region_2_return
              )
         )
}