split_and_fit = function(region,
                         best_y_so_far,
                         where_best_y_so_far,
                         run_obs_vec,
                         best_so_far_vec) {
  
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
  
  region_1_descr <- DescribeX(
    x_names = x_names_arg,
    x_min = region_1_return$bound_matrix[, 1],
    x_max = region_1_return$bound_matrix[, 2],
    support = rep("Continuous", dim)
  )
  
  region_1_init <- Initialize(
    x_design = region_1_return$region_x,
    x_describe = region_1_descr,
    fun = test_func
  )
  
  region_2_descr <- DescribeX(
    x_names = x_names_arg,
    x_min = region_2_return$bound_matrix[, 1],
    x_max = region_2_return$bound_matrix[, 2],
    support = rep("Continuous", dim)
  )
  
  region_2_init <- Initialize(
    x_design = region_2_return$region_x,
    x_describe = region_2_descr,
    fun = test_func
  )
  
  region_1_bo <- EGO(
    fun = test_func,
    reg_model = ~1,
    ego_init = region_1_init,
    x_describe = region_1_descr,
    nsteps = 1,
    control = ctrl
  )
  
  first_NA_index <- min(which(is.na(run_obs_vec)))
  latest_obs <- tail(region_1_bo$y, n = 1)
  run_obs_vec[first_NA_index] <- latest_obs
  best_so_far_vec[first_NA_index] <- min(run_obs_vec[(1:first_NA_index)])
  print(sprintf("New observation in first new subregion: %s", latest_obs))
  print(sprintf("Best so far: %s", best_so_far_vec[first_NA_index]))
  
  region_2_bo <- EGO(
    fun = test_func,
    reg_model = ~1,
    ego_init = region_2_init,
    x_describe = region_2_descr,
    nsteps = 1,
    control = ctrl
  )
  
  first_NA_index <- min(which(is.na(run_obs_vec)))
  latest_obs <- tail(region_2_bo$y, n = 1)
  run_obs_vec[first_NA_index] <- latest_obs
  best_so_far_vec[first_NA_index] <- min(run_obs_vec[(1:first_NA_index)])
  print(sprintf("New observation in second new subregion: %s", latest_obs))
  print(sprintf("Best so far: %s", best_so_far_vec[first_NA_index]))
  
  region_1_return$region_x <- region_1_bo$x
  region_1_return$region_y <- region_1_bo$y
  new_region_1_obs = tail(region_1_bo$y, n = 1)
  if (new_region_1_obs < region_1_return$region_min) {
    region_1_return$region_min = new_region_1_obs
    region_1_return$region_argmin = region_1_bo$x[nrow(region_1_bo$x), ]
    if (new_region_1_obs < best_y_so_far) {
      best_y_so_far = new_region_1_obs
      where_best_y_so_far = region_1_bo$x[nrow(region_1_bo$x), ]
    }
  }
  region_1_return$region_a_max <- region_1_bo$ac_val_track
  
  region_2_return$region_x <- region_2_bo$x
  region_2_return$region_y <- region_2_bo$y
  new_region_2_obs = tail(region_2_bo$y, n = 1)
  if (new_region_2_obs < region_2_return$region_min) {
    region_2_return$region_min = new_region_2_obs
    region_2_return$region_argmin = region_2_bo$x[nrow(region_2_bo$x), ]
    if (new_region_2_obs < best_y_so_far) {
      best_y_so_far = new_region_2_obs
      where_best_y_so_far = region_2_bo$x[nrow(region_2_bo$x), ]
    }
  }
  region_2_return$region_a_max <- region_2_bo$ac_val_track
  
  return(list(region_1 = region_1_return,
              region_2 = region_2_return,
              new_best_y = best_y_so_far,
              where_new_best_y = where_best_y_so_far,
              run_obs = run_obs_vec,
              best_so_far = best_so_far_vec
              )
  )
  # the two regions this function returns
  # need to be added to our list of candidate regions
}

explore_region <- function(region,
                           best_y_so_far,
                           where_best_y_so_far,
                           run_obs_vec,
                           best_so_far_vec,
                           n_max = 10,
                           tol = 1) {
  region_x = region$region_x
  region_y = region$region_y
  n = nrow(region_x)
  descr <- DescribeX(
    x_names = x_names_arg,
    x_min = region$bound_matrix[, 1],
    x_max = region$bound_matrix[, 2],
    support = rep("Continuous", dim)
  )
  while (n < n_max) {
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
    n = n + 1
    
    first_NA_index <- min(which(is.na(run_obs_vec)))
    latest_obs <- tail(bo$y, n = 1)
    run_obs_vec[first_NA_index] <- latest_obs
    best_so_far_vec[first_NA_index] <- min(run_obs_vec[(1:first_NA_index)])
    print(sprintf("New observation: %s", latest_obs))
    print(sprintf("Best so far: %s", best_so_far_vec[first_NA_index]))
    
    new_observed_y = tail(region_y, n = 1)
    if (new_observed_y < region$region_min) {
      region$region_min = new_observed_y
      region$region_argmin = region_x[n, ]
      if (new_observed_y < best_y_so_far) {
        best_y_so_far = new_observed_y
        where_best_y_so_far = region_x[n, ]
      }
    }
    a_max = bo$ac_val_track
    print(sprintf("a_max: %s", a_max))
    
    region$region_x = region_x
    region$region_y = region_y
    region$region_a_max = a_max

    if (a_max < tol) {
      
      # i should prepare the updated region and return it
      # and return the smallest y value, i think
      # and remove this region from my list of promising regions
      # my dice_loop.R might be helpful for the last part
      
      return(list(region = region,
                  best_y = best_y_so_far,
                  where_best_y = where_best_y_so_far,
                  run_obs = run_obs_vec,
                  best_so_far = best_so_far_vec,
                  split_called = 0)
      )
    }
  }
  
  # the while loop completed, so now
  # we need to split the region into 2 subregions
  
  print("n_max limit met; splitting region")
  
  new_subregions = split_and_fit(region = region,
                                 best_y_so_far = best_y_so_far,
                                 where_best_y_so_far = where_best_y_so_far,
                                 run_obs_vec = run_obs_vec,
                                 best_so_far_vec = best_so_far_vec)
  
  # I need to re-bind run_obs_vec and best_so_far_vec
  # and return the updated values below
  
  new_subregion_1 = new_subregions$region_1
  new_subregion_2 = new_subregions$region_2
  new_best_y = new_subregions$new_best_y
  where_new_best_y = new_subregions$where_new_best_y
  run_obs_vec <- new_subregions$run_obs
  best_so_far_vec <- new_subregions$best_so_far
  
  return(list(new_region_1 = new_subregion_1,
              new_region_2 = new_subregion_2,
              best_y = new_best_y,
              where_best_y = where_new_best_y,
              run_obs = run_obs_vec,
              best_so_far = best_so_far_vec,
              split_called = 1)
  )
}