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
  where_best_new_y <- new_x[which.min(new_y), ]
  
  # If the best of the new observations
  # is better than the region best,
  # update the region records
  
  if (best_new_y < region$region_min) {
    
    # print(sprintf("New region best observed: %s at %s", best_new_y, where_best_new_y))
    print(paste(c("New region best observed: ", best_new_y, " at location: ",
                  where_best_new_y)))
    
    region$region_min <- best_new_y
    region$region_argmin <- where_best_new_y
    
  }
  
  # If the best of the new observations
  # is better than the overall best,
  # update the overall records
  
  if (best_new_y < best_y_so_far) {
    
    # print(sprintf("New overall best observed: %s at %s", best_new_y, where_best_new_y))
    print(paste(c("New overall best observed: ", best_new_y, " at location: ",
                  where_best_new_y)))
    
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

gen_points <- function(region,
                       num_points) {
  
  # This function generates num_points points
  # in dimension dimensions using randomLHS,
  # and then scaling and shifting
  # the points to cover the supplied region
  
  dimension <- ncol(region$region_x)
  
  ### Begin test code ###
  
  # dimension <- 2

  # ei_points <- rbind(c(0.25, 0.25), c(0.25, 0.75), c(0.75, 0.75), c(0.75, 0.25))
  # scaled_shifted_ei_points <- matrix(data = NA, nrow = 4, ncol = dimension)
  
  # region_bound_matrix <- rbind(c(-5, 5), c(-5, 5))
  # region_bound_matrix <- rbind(c(1, 2), c(1, 5))
  # region_bound_matrix <- rbind(c(-6, 2), c(-2, -1))
  
  ### End test code ###
  
  ei_points <- randomLHS(num_points, dimension)
  scaled_shifted_ei_points <- matrix(data = NA, nrow = num_points, ncol = dimension)
  
  # Then we have to scale and shift them to cover our region
  
  scaling_vector <- rep(0, dimension)
  shifting_vector <- rep(0, dimension)
  
  for (d in 1:dimension) {
    
    # The dth coordinate of an LHS point
    # get scaled by the length of the region in dimension d
    
    ### Begin test code ###
    
    # region_dimension_d_length <- region_bound_matrix[d, 2] - region_bound_matrix[d, 1]
    
    ### End test code ###
    
    region_dimension_d_length <- region$bound_matrix[d, 2] - region$bound_matrix[d, 1]
    
    scaling_vector[d] <- region_dimension_d_length
    scaled_shifted_ei_points[, d] <- ei_points[, d] * scaling_vector[d]
    
    # Next, we find the midpoint of the region
    # We will shift the scaled LHS points
    # to the origin, and to the desired region
    
    ### Begin test code ###
    
    # region_dimension_d_midpoint_coord <- region_bound_matrix[d, 1] + (scaling_vector[d] / 2)
    
    ### End test code ###
    
    region_dimension_d_midpoint_coord <- region$bound_matrix[d, 1] + (scaling_vector[d] / 2)
    shifting_vector[d] <- region_dimension_d_midpoint_coord
    
  }
  
  # Now that the LHS hypercube has the correct dimensions,
  # we shift it to the origin,
  # and then shift it to overlap the region
  # (Here we are using the collapse package to subtract/add
  # a vector to each row of a table of vectors)
  
  scaled_shifted_ei_points <- scaled_shifted_ei_points %r-% (scaling_vector / 2)
  scaled_shifted_ei_points <- scaled_shifted_ei_points %r+% shifting_vector
  
  return (scaled_shifted_ei_points)
}

### Begin test code ###

# Must run the code in bo_partition.R
# up to the binding for init_region

# Pick one of the following region bounds

# init_region$bound_matrix[1, ] <- c(-5, 5)
# init_region$bound_matrix[2, ] <- c(-5, 5)

# init_region$bound_matrix[1, ] <- c(1, 2)
# init_region$bound_matrix[2, ] <- c(1, 5)

# init_region$bound_matrix[1, ] <- c(-6, 2)
# init_region$bound_matrix[2, ] <- c(-2, -1)

# Then generate some test points in the region
# and look at the plot

# test_points <- gen_points(init_region, 10000)

# plot(test_points, xlim=c(-10, 10), ylim=c(-10, 10))
# plot(test_points, xlim=c(0, 5), ylim=c(0, 5))
# plot(test_points, xlim=c(-8, 4), ylim=c(-4, 0))

### End test code ###

prep_subregions <- function(region,
                            split_dimension,
                            split_point,
                            min_split_width = 1e-2) {
  
  # Bind the region x and y points
  
  region_x = region$region_x
  region_y = region$region_y
  
  # Split region_x and region_y
  
  region_1_x = region_x[region_x[, split_dimension] < split_point, ]
  region_1_y = region_y[which(region_x[, split_dimension] < split_point)]
  
  region_2_x = region_x[region_x[, split_dimension] >= split_point, ]
  region_2_y = region_y[which(region_x[, split_dimension] >= split_point)]
  
  # Compute the widths of region_1 and region_1
  # on the splitting dimension
  # If either is narrower than min_split_width,
  # we won't split at this point on this dimension
  
  region_1_x_d_range <- frange(region_1_x[, split_dimension])
  region_2_x_d_range <- frange(region_2_x[, split_dimension])
  
  region_1_x_d_width <- region_1_x_d_range[2] - region_1_x_d_range[1]
  region_2_x_d_width <- region_2_x_d_range[2] - region_2_x_d_range[1]
  
  if(region_1_x_d_width <= min_split_width || region_2_x_d_width <= min_split_width) {
    
    print(sprintf("One of region_1_x_d_width (%s) or region_1_x_d_width (%s)
                      is <= min_split_width (%s), when splitting on
                      dimension %d at split_point %s",
                  region_1_x_d_width, region_2_x_d_width, min_split_width,
                  split_dimension, split_point))
    
    print(sprintf("Therefore we are going to not going to consider splitting
                      on dimension %s at split_point %s", split_dimension, split_point))
    
    return(0)

  }
  
  # Now we prepare the two prospective new subregions
  # Note that the EI values (a_max) for each subregion
  # are incorrect, since we have not fit
  # Gaussian process models or optimized EI
  # for these subregions
  
  print(sprintf("Preparing region_1"))
  
  region_1 = region
  
  region_1$bound_matrix[split_dimension, 2] = split_point
  
  region_1$region_x = region_1_x
  region_1$region_y = region_1_y
  
  region_1$region_min = min(region_1_y)
  region_1$region_argmin = region_1_x[which.min(region_1_y), ]
  
  print("Proposed region 1:")
  print(region_1)
  
  print(sprintf("Preparing region_2"))
  
  region_2 = region
  
  region_2$bound_matrix[split_dimension, 1] = split_point
  
  region_2$region_x = region_2_x
  region_2$region_y = region_2_y
  
  region_2$region_min = min(region_2_y)
  region_2$region_argmin = region_2_x[which.min(region_2_y), ]
  
  print("Proposed region 2:")
  print(region_2)
  
  return(list(subregion_1 = region_1,
              subregion_2 = region_2
              )
         )
}

split_and_fit <- function(region,
                          region_model,
                          how_many_EI_points = 1000,
                          top_n_EI_vals = 10
                          ) {
  
  # Begin timing how long split_and_fit() takes to run
  
  start <- Sys.time()
  print(sprintf("Beginning split_and_fit"))
  
  ### Begin test code ###
  
  # region <- init_region
  # region_model <- gp_model
  # 
  # dimension <- ncol(region$region_x)
  # top_n_EI_vals <- 3
  
  ### End test code ###
  
  print("The region we are spliting is:")
  print(region)
  
  # Bind the dimension
  
  dimension <- ncol(region$region_x)
  
  # We need to prepare
  # the points at which we will evaluate EI,
  # and then evaluate EI at those points
  
  # First we generate the points
  
  EI_points <- gen_points(region = region,
                          num_points = how_many_EI_points)
  
  print("The points at which we will evaluate EI are:")
  print(EI_points)
  
  # Next, we evaluate EI for each of the (scaled, shifted) LHS points
  # and find which indices correspond to the biggest EI values
  # and then we isolate those particular EI points
  
  EI_values <- apply(X = EI_points, MARGIN = 1, FUN = EI, region_model)
  
  print("The EI values are:")
  print(EI_values)

  biggest_ei_vals_indices <- which.maxn(EI_values, n = top_n_EI_vals)
  biggest_ei_vals_points <- EI_points[biggest_ei_vals_indices, ]
  
  print(sprintf("The points leading to the %s biggest EI values are:",
                top_n_EI_vals))
  print(biggest_ei_vals_points)
  
  # We need to keep track of how many of the
  # points leading to the biggest EI values
  # are contained in a proposed subregion
  # The variable binding below will serve as our running tally
  # The second binding is to keep track of region volume
  
  most_EI_vals <- 0
  smallest_subregion_vol <- Inf
  
  # Now we begin the splitting process
  
  for (d in 1:dimension) {
    
    ### Begin test code ###
    
    # d <- 1
    
    ### End test code ###
    
    print(sprintf("Splitting on dimension %s", d))
    
    # Determine the 0.25, 0.50, 0.75 percentiles
    # of the region_x points on dimension d
    
    percentiles_vec <- c(0.25, 0.50, 0.75)
    percentiles <- fquantile(region$region_x[, d], percentiles_vec)
    
    print(sprintf("The %s percentiles for the region x points
                   in dimension %s are respectively: %s",
                  percentiles_vec, d, percentiles))
    
    for (p in 1:length(percentiles)) {
      
      ### Begin test code ###
      
      # p <- 1
      
      ### End test code ###
      
      percentile <- percentiles[p]
      
      print(sprintf("Splitting at percentile %s (which takes value %s) in dimension %s",
                    percentiles_vec[p], percentile, d))
      
      # Prepare the subregions
      
      subregions <- prep_subregions(region = region,
                                    split_dimension = d,
                                    split_point = percentile
                                    )
      
      # Check if the split was unsuccessful because
      # a very narrow subregion would have been created
      # If the split was unsuccessful, try the next split
      # (If unsuccessful, prep_subregions returns zero)
      
      if (is.numeric(subregions)) {
        next
      }
      
      region_1 <- subregions[[1]]
      region_2 <- subregions[[2]]
      
      print("Proposed subregion 1:")
      print(region_1)
      print("Proposed subregion 2:")
      print(region_2)
      
      # Next, we must determine how many
      # of the biggest EI points lie in
      # each prospective subregion
      
      is_point_in_region_1 <- matrix(data = NA, nrow = top_n_EI_vals, ncol = 1)
      
      for (EI_point in 1:top_n_EI_vals) {
        is_point_in_region_1[EI_point, 1] <- all(between(biggest_ei_vals_points[EI_point, ],
                                                         region_1$bound_matrix[, 1],
                                                         region_1$bound_matrix[, 2]))
      }
      
      num_points_in_region_1 <- fsum(is_point_in_region_1)
      num_points_in_region_2 <- top_n_EI_vals - num_points_in_region_1
      
      print(sprintf("The number of the biggest EI points in region 1 is:
                     %s, and the number in region 2 is: %s",
                    num_points_in_region_1, num_points_in_region_2))

      # Compute the region volumes

      region_1_dim_lengths <- region_1$bound_matrix[, 2] - region_1$bound_matrix[, 1]
      region_2_dim_lengths <- region_2$bound_matrix[, 2] - region_2$bound_matrix[, 1]

      region_1_volume <- fprod(region_1_dim_lengths)
      region_2_volume <- fprod(region_2_dim_lengths)
      
      print(sprintf("The current record for a subregion containing
                    the highest number of the %s points leading
                    to the highest EI values is: %s",
                    top_n_EI_vals, most_EI_vals))
      
      print(sprintf("The current record for smallest region volume
                    containing %s of the %s points leading to the
                    highest EI values is: %s",
                    most_EI_vals, top_n_EI_vals,
                    smallest_subregion_vol))
      
      # Now we have the record for most EI points
      # in a subregion, the smallest subregion volume,
      # the number of EI points in each subregion,
      # and the volume of each subregion
      
      # First, we check whether at least one
      # of the subregions contains strictly more
      # EI points than the current record
      
      subregions_that_are_better <- which(
        c(num_points_in_region_1, num_points_in_region_2) > most_EI_vals
      )

      print(sprintf("The subregions (if any) that contain
                     more points as the current best are: %s",
                    subregions_that_are_better))

      is_either_subregion_better_than_current_best <- any(
        subregions_that_are_better
      )
      
      # We also check whether at least one
      # of the subregions contains the same
      # number of EI points as the current record,
      # but we only use this information if
      # no subregion contains strictly more
      # EI points than the current record
      
      subregions_that_are_as_good <- which(
        c(num_points_in_region_1, num_points_in_region_2) == most_EI_vals
      )

      print(sprintf("The subregions (if any) that contain
                     as many points as the current best are: %s",
                    subregions_that_are_as_good))

      is_either_subregion_as_good_as_current_best <- any(
        subregions_that_are_as_good
        )
      
      # If neither of these conditions is satisfied,
      # we reject this split and move on to
      # the next one
      
      if (is_either_subregion_better_than_current_best) {
        
        # At least one of the subregions contains
        # strictly more points than the current record,
        # so this is the new best split
        
        dim_to_split <- d
        point_to_split <- percentile
        percentile_to_split <- percentiles_vec[p]
        
        region_1_return <- region_1
        region_2_return <- region_2
        
        # The only issue in this case is
        # how to re-set most_EI_vals
        # and smallest_subregion_vol

        # If region_1 and region_2 are tied,
        # set smallest_subregion_vol
        # to be the minimum of the region volumes,
        # and re-set most_EI_vals
        
        are_the_subregions_tied <- (num_points_in_region_1 == num_points_in_region_2)
        
        if (are_the_subregions_tied) {
          
          min_region_volume <- min(region_1_volume, region_2_volume)

          smallest_subregion_vol <- min_region_volume
          most_EI_vals <- num_points_in_region_1

          
        } else {
          
          # If they are not tied,
          # one subregion contains more points
          # than the other
          
          # If region_1 contains more points
          # than region_2,
          # we re-set the records using region_1
          
          is_region_1_better <- (num_points_in_region_1 > num_points_in_region_2)
          
          if (is_region_1_better) {

            smallest_subregion_vol <- region_1_volume
            most_EI_vals <- num_points_in_region_1

          } else {
            
            # Since region_1 was not better,
            # region_2 must have been better,
            # so we re-set the records using region_2

            smallest_subregion_vol <- region_2_volume
            most_EI_vals <- num_points_in_region_2

          }
          
        }
        
      } else if (is_either_subregion_as_good_as_current_best) {
        
        # Since neither subregion contained strictly
        # more EI points than the current record,
        # we check whether at least one of them
        # contains the same number of EI points
        # as the current record
        
        # If this is true, we don't have to
        # re-set most_EI_vals, but we might
        # have to re-set smallest_subregion_vol
        
        # (If this is false, we discard this split
        # and consider the next possible split)
        
        # If region_1 and region_2 are tied,
        # we only re-set smallest_subregion_vol
        # if the minimum of the region volumes
        # is less than smallest_subregion_vol
        
        are_both_subregions_as_good <- (length(subregions_that_are_as_good) == 2)
        is_only_region_1_as_good <- (subregions_that_are_as_good == 1)
        
        if (are_both_subregions_as_good) {
          
          min_subregion_volume <- min(region_1_volume, region_2_volume)
          
          if (min_subregion_volume < smallest_subregion_vol) {
            
            smallest_subregion_vol <- min_subregion_volume
            
            dim_to_split <- d
            point_to_split <- percentile
            percentile_to_split <- percentiles_vec[p]
            
            region_1_return <- region_1
            region_2_return <- region_2
            
          }
          
        } else if (is_only_region_1_as_good) {
          
          # If only region_1 tied, then
          # we only re-set smallest_subregion_vol
          # if region_1_volume is less than
          # smallest_subregion_vol
          
          if (region_1_volume < smallest_subregion_vol) {
            
            smallest_subregion_vol <- region_1_volume
            
            dim_to_split <- d
            point_to_split <- percentile
            percentile_to_split <- percentiles_vec[p]
            
            region_1_return <- region_1
            region_2_return <- region_2
            
          }
          
        } else {
          
          # Only region_2 tied, in which case
          # we only re-set smallest_subregion_vol
          # if region_2_volume is smaller than
          # smallest_subregion_vol
          
          if (region_2_volume < smallest_subregion_vol) {
            
            smallest_subregion_vol <- region_2_volume
            
            dim_to_split <- d
            point_to_split <- percentile
            percentile_to_split <- percentiles_vec[p]
            
            region_1_return <- region_1
            region_2_return <- region_2
            
          }
          
        }
        
      }

    }

  }
  
  print(sprintf("The chosen split is on dimension %s
                 at percentile %s, which takes value %s",
                dim_to_split, percentile_to_split, point_to_split))
  
  print(sprintf("This split leads to a new subregion
                 containing %s of the top %s
                 EI points, with region volume
                 %s", most_EI_vals, top_n_EI_vals,
                smallest_subregion_vol))
  
  print("The first new subregion is:")
  print(region_1_return)
  
  print("The second new subregion is:")
  print(region_2_return)
  
  # Now we have chosen our new subregions,
  # so we need to fit Gaussian process models
  # for each, and optimize EI for each
    
    # Fit the Gaussian process model
    
    print(sprintf("Fitting Gaussian process model
                   for first new subregion"))
  
  region_1_gp_model <- km(
    formula = ~1,
    design = region_1_return$region_x,
    response = region_1_return$region_y,
    covtype = "powexp",
    nugget = 1e-09,
    control = c(dice_ctrl, trace = FALSE),
    optim.method = "gen"
  )
  
  # Feed the Gaussian process model
  # to the acquisition-function maximizer
  
  print(sprintf("Optimizing acquisition function
                 for first new subregion"))
  
  region_1_acq_func_max <- max_EI(
    model = region_1_gp_model,
    type = "UK",
    lower = region_1_return$bound_matrix[, 1],
    upper = region_1_return$bound_matrix[, 2],
    control = dice_ctrl
  )
  
  # Fit the Gaussian process model
  
  print(sprintf("Fitting Gaussian process model
                 for second new subregion"))
  
  region_2_gp_model <- km(
    formula = ~1,
    design = region_2_return$region_x,
    response = region_2_return$region_y,
    covtype = "powexp",
    nugget = 1e-09,
    control = c(dice_ctrl, trace = FALSE),
    optim.method = "gen"
  )
  
  # Feed the Gaussian process model
  # to the acquisition-function maximizer
  
  print(sprintf("Optimizing acquisition function
                 for second new subregion"))
  
  region_2_acq_func_max <- max_EI(
    model = region_2_gp_model,
    type = "UK",
    lower = region_2_return$bound_matrix[, 1],
    upper = region_2_return$bound_matrix[, 2],
    control = dice_ctrl
  )
  
  # Update the a_max values
  # for the two new subregions
  
  region_1_return$region_a_max <- region_1_acq_func_max$value
  region_2_return$region_a_max <- region_2_acq_func_max$value
  
  end <- Sys.time()
  duration <- end - start
  
  print("Region split in:")
  print(duration)

  print(sprintf("Returning new subregions from split_and_fit"))

  return(list(region_1 = region_1_return,
              region_2 = region_2_return
              )
         )
}

### Begin test code ###

# init_region_split <- split_and_fit(region = init_region,
#                                    region_model = gp_model,
#                                    how_many_EI_points = 10,
#                                    top_n_EI_vals = 3
#                                    )
# sink(file = NULL)

### End test code ###