split_and_fit = function(region,
                         best_y_so_far,
                         where_best_y_so_far,
                         run_obs_vec,
                         best_so_far_vec,
                         split_crit,
                         range_tol = 1e-3) {
  print(sprintf("Beginning split_and_fit"))
  region_x = region$region_x
  region_y = region$region_y
  lowest_y_avg_val = 1e+10
  lowest_y_min_minus_a_max_val <- 1e+10
  
  for(d in 1:dim){
    print(sprintf("Splitting on dimension %s", d))
    med = median(region_x[, d])
    
    region_1_x = region_x[region_x[, d] < med, ]
    region_1_y = region_y[which(region_x[, d] < med)]
    region_2_x = region_x[region_x[, d] >= med, ]
    region_2_y = region_y[which(region_x[, d] >= med)]
    
    region_1_x_split_dim_range <- max(region_1_x[, d]) - min(region_1_x[, d])
    region_2_x_split_dim_range <- max(region_2_x[, d]) - min(region_2_x[, d])
    
    if(region_1_x_split_dim_range <= range_tol | region_2_x_split_dim_range <= range_tol) {
      print(sprintf("One of region_1_split_dim_range (%s) or region_2_split_dim_range (%s) is <= range_tol (%s)",
                    region_1_x_split_dim_range, region_2_x_split_dim_range, range_tol))
      print(sprintf("Therefore we are going to not going to consider splitting on dimension %s", d))
      next
    }
    
    if(split_crit == "avg") {
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
    if(split_crit == "y_min_minus_a_max") {
      print(sprintf("Preparing region_1"))
      region_1 = region
      region_1$bound_matrix[d, 2] = med
      region_1$region_x = region_1_x
      region_1$region_y = region_1_y
      region_1$region_min = min(region_1_y)
      region_1$region_argmin = region_1_x[which.min(region_1_y), ]
      print("Proposed region 1:") # The a_max value is inherited from "region", i.e. it hasn't been computed yet for this proposed subregion
      print(region_1)
      
      print(sprintf("Preparing region_2"))
      region_2 = region
      region_2$bound_matrix[d, 1] = med
      region_2$region_x = region_2_x
      region_2$region_y = region_2_y
      region_2$region_min = min(region_2_y)
      region_2$region_argmin = region_2_x[which.min(region_2_y), ]
      print("Proposed region 2:") # See comment above pertaining to propsed region 1
      print(region_2)

      if (r_package == "dice") {
        region_1_km_x <- region_1$region_x
        region_1_km_y <- region_1$region_y
        print(sprintf("Fitting region_1_gp_model"))
        region_1_gp_model <- km(
          formula = ~1,
          design = region_1_km_x,
          response = region_1_km_y,
          covtype = "powexp", # "matern5_2",
          nugget = 1e-09,
          control = c(dice_ctrl, trace = FALSE),
          optim.method = "gen"
          )
        print(sprintf("Optimizing region_1_acq_func_max"))
        region_1_acq_func_max <- max_EI(
          model = region_1_gp_model,
          type = "UK",
          lower = region_1$bound_matrix[, 1],
          upper = region_1$bound_matrix[, 2],
          control = dice_ctrl
          )

        region_2_km_x <- region_2$region_x
        region_2_km_y <- region_2$region_y
        print(sprintf("Fitting region_2_gp_model"))
        region_2_gp_model <- km(
          formula = ~1,
          design = region_2_km_x,
          response = region_2_km_y,
          covtype = "powexp", # "matern5_2",
          nugget = 1e-09,
          control = c(dice_ctrl, trace = FALSE),
          optim.method = "gen"
          )
        print(sprintf("Optimizing region_2_acq_func_max"))
        region_2_acq_func_max <- max_EI(
          model = region_2_gp_model,
          type = "UK",
          lower = region_2$bound_matrix[, 1],
          upper = region_2$bound_matrix[, 2],
          control = dice_ctrl
          )

        region_1_value <- region_1$region_min - region_1_acq_func_max$value
        region_2_value <- region_2$region_min - region_2_acq_func_max$value

      } else if (r_package == "ego") {
        print(sprintf("Preparing region_1_descr"))
        region_1_descr <- DescribeX(
          x_names = x_names_arg,
          x_min = region_1$bound_matrix[, 1],
          x_max = region_1$bound_matrix[, 2],
          support = rep("Continuous", dim)
        )
      
        print(sprintf("Preparing region_1_init"))
        region_1_init <- Initialize(
          x_design = region_1$region_x,
          x_describe = region_1_descr,
          fun = test_func
        )
        
        print(sprintf("Preparing region_2_descr"))
        region_2_descr <- DescribeX(
          x_names = x_names_arg,
          x_min = region_2$bound_matrix[, 1],
          x_max = region_2$bound_matrix[, 2],
          support = rep("Continuous", dim)
        )
        
        print(sprintf("Preparing region_2_init"))
        region_2_init <- Initialize(
          x_design = region_2$region_x,
          x_describe = region_2_descr,
          fun = test_func
        )
      
        print(sprintf("Computing region_1_bo"))
        region_1_bo <- EGO(
          fun = test_func,
          reg_model = ~1,
          ego_init = region_1_init,
          x_describe = region_1_descr,
          nsteps = 1,
          control = ctrl
        )
        
        print(sprintf("Computing region_2_bo"))
        region_2_bo <- EGO(
          fun = test_func,
          reg_model = ~1,
          ego_init = region_2_init,
          x_describe = region_2_descr,
          nsteps = 1,
          control = ctrl
        )
        
        region_1_value <- region_1$region_min - region_1_bo$ac_val_track
        region_2_value <- region_2$region_min - region_2_bo$ac_val_track

      }
     
      print(sprintf("Values of proposed regions 1 and 2, respectively: %s, %s", region_1_value, region_2_value))
      print(sprintf("Current lowest region value: %s", lowest_y_min_minus_a_max_val))
     
      print(sprintf("Comparing region_1_value and region_2_value to lowest_y_min_minus_a_max_val"))
      if(region_1_value < lowest_y_min_minus_a_max_val | region_2_value < lowest_y_min_minus_a_max_val) {
        print(sprintf("New best region value observed: %s; updating best observed", min(region_1_value, region_2_value)))
        lowest_y_min_minus_a_max_val <- min(region_1_value, region_2_value)
        # dim_to_split = d
        # split_point = med
        
        if (r_package == "dice") {
          # I need to figure out what I need to save here
          region_1_a_max <- region_1_acq_func_max$value
          region_2_a_max <- region_2_acq_func_max$value
          region_1_ei_argmax <- region_1_acq_func_max$par
          region_2_ei_argmax <- region_2_acq_func_max$par
        } else if (r_package == "ego") {
          region_1_bo_chosen <- region_1_bo
          region_2_bo_chosen <- region_2_bo
        }
        region_1_return <- region_1
        region_2_return <- region_2
      }
    }
  }
  
  if(split_crit == "avg") {
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
    # print(sprintf("Best so far: %s", best_so_far_vec[first_NA_index]))
    
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
    # print(sprintf("Best so far: %s", best_so_far_vec[first_NA_index]))
    
    region_1_return$region_x <- region_1_bo$x
    region_1_return$region_y <- region_1_bo$y
    new_region_1_obs = tail(region_1_bo$y, n = 1)
    if (new_region_1_obs < region_1_return$region_min) {
      region_1_return$region_min = new_region_1_obs
      region_1_return$region_argmin = region_1_bo$x[nrow(region_1_bo$x), ]
      if (new_region_1_obs < best_y_so_far) {
        best_y_so_far = new_region_1_obs
        where_best_y_so_far = region_1_bo$x[nrow(region_1_bo$x), ]
        print(sprintf("New best: %s", best_y_so_far))
        print(sprintf("Recorded at: %s", where_best_y_so_far))
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
        print(sprintf("New best: %s", best_y_so_far))
        print(sprintf("Recorded at: %s", where_best_y_so_far))
      }
    }
    region_2_return$region_a_max <- region_2_bo$ac_val_track
  }
  
  if(split_crit == "y_min_minus_a_max") {
    print(sprintf("Updating run_obs_vec and best_so_far_vec with region 1 observation"))
    first_NA_index <- min(which(is.na(run_obs_vec)))
    if (r_package == "dice") {
      latest_obs <- test_func(region_1_ei_argmax)
    } else if (r_package == "ego") {
      latest_obs <- tail(region_1_bo_chosen$y, n = 1)
    }
    run_obs_vec[first_NA_index] <- latest_obs
    best_so_far_vec[first_NA_index] <- min(run_obs_vec[(1:first_NA_index)])
    print(sprintf("New observation in first new subregion: %s", latest_obs))
    # print(sprintf("Best so far: %s", best_so_far_vec[first_NA_index]))

    print(sprintf("Updating run_obs_vec and best_so_far_vec with region 2 observation"))
    first_NA_index <- min(which(is.na(run_obs_vec)))
    if (r_package == "dice") {
      latest_obs <- test_func(region_2_ei_argmax)
    } else if (r_package == "ego") {
      latest_obs <- tail(region_2_bo_chosen$y, n = 1)
    }
    run_obs_vec[first_NA_index] <- latest_obs
    best_so_far_vec[first_NA_index] <- min(run_obs_vec[(1:first_NA_index)])
    print(sprintf("New observation in second new subregion: %s", latest_obs))
    # print(sprintf("Best so far: %s", best_so_far_vec[first_NA_index]))
   
    print(sprintf("Updating region_1_return"))
    if (r_package == "dice") {
      region_1_return$region_x <- rbind(region_1_return$region_x, region_1_ei_argmax)
      new_region_1_obs <- test_func(region_1_ei_argmax)
      region_1_return$region_y <- c(region_1_return$region_y, new_region_1_obs)
    } else if (r_package == "ego") {
      region_1_return$region_x <- region_1_bo_chosen$x
      region_1_return$region_y <- region_1_bo_chosen$y
      new_region_1_obs = tail(region_1_bo_chosen$y, n = 1)
    }
    if (new_region_1_obs < region_1_return$region_min) {
      region_1_return$region_min = new_region_1_obs
      if (r_package == "dice") {
        region_1_return$region_argmin <- region_1_ei_argmax
      } else if (r_package == "ego") {
        region_1_return$region_argmin = region_1_bo_chosen$x[nrow(region_1_bo_chosen$x), ]
      }
      if (new_region_1_obs < best_y_so_far) {
        best_y_so_far = new_region_1_obs
        if (r_package == "dice") {
          where_best_y_so_far <- region_1_ei_argmax
        } else if (r_package == "ego") {
          where_best_y_so_far = region_1_bo_chosen$x[nrow(region_1_bo_chosen$x), ]
        }
        print(sprintf("New best: %s", best_y_so_far))
        print(sprintf("Recorded at: %s", where_best_y_so_far))
      }
    }
    if (r_package == "dice") {
      region_1_return$region_a_max <- region_1_a_max
    } else if (r_package == "ego") {
      region_1_return$region_a_max <- region_1_bo_chosen$ac_val_track
    }    
    
    print(sprintf("Updating region_2_return"))
    if (r_package == "dice") {
      region_2_return$region_x <- rbind(region_2_return$region_x, region_2_ei_argmax)
      new_region_2_obs <- test_func(region_2_ei_argmax)
      region_2_return$region_y <- c(region_2_return$region_y, new_region_2_obs)
    } else if (r_package == "ego") {
      region_2_return$region_x <- region_2_bo_chosen$x
      region_2_return$region_y <- region_2_bo_chosen$y
      new_region_2_obs = tail(region_2_bo_chosen$y, n = 1)
    }
    if (new_region_2_obs < region_2_return$region_min) {
      region_2_return$region_min = new_region_2_obs
      if (r_package == "dice") {
        region_2_return$region_argmin <- region_2_ei_argmax
      } else if (r_package == "ego") {
        region_2_return$region_argmin = region_2_bo_chosen$x[nrow(region_2_bo_chosen$x), ]
      }
      if (new_region_2_obs < best_y_so_far) {
        best_y_so_far = new_region_2_obs
        if (r_package == "dice") {
          where_best_y_so_far <- region_2_ei_argmax
        } else if (r_package == "ego") {
          where_best_y_so_far = region_2_bo_chosen$x[nrow(region_2_bo_chosen$x), ]
        }
        print(sprintf("New best: %s", best_y_so_far))
        print(sprintf("Recorded at: %s", where_best_y_so_far))
      }
    }
    if (r_package == "dice") {
      region_2_return$region_a_max <- region_2_a_max
    } else if (r_package == "ego") {
      region_2_return$region_a_max <- region_2_bo_chosen$ac_val_track
    }
  }

  print(sprintf("Returning region_1 and region_2 and other data from split_and_fit"))
  return(list(region_1 = region_1_return,
              region_2 = region_2_return,
              new_best_y = best_y_so_far,
              where_new_best_y = where_best_y_so_far,
              run_obs = run_obs_vec,
              best_so_far = best_so_far_vec
              )
  )
}

explore_region <- function(region,
                           best_y_so_far,
                           where_best_y_so_far,
                           run_obs_vec,
                           best_so_far_vec,
                           n_max = 10 * dim,
                           tol = 0.1,
                           split_crit = "avg") {
  region_x = region$region_x
  region_y = region$region_y
  n = nrow(region_x)
  if (r_package == "ego") {
    descr <- DescribeX(
      x_names = x_names_arg,
      x_min = region$bound_matrix[, 1],
      x_max = region$bound_matrix[, 2],
      support = rep("Continuous", dim)
    )
  }
  while (n < n_max) {
    if (r_package == "dice") {
      km_x <- region_x
      km_y <- region_y
      gp_model <- km(
        formula = ~1,
        design = km_x,
        response = km_y,
        covtype = "powexp", # "matern5_2",
        nugget = 1e-09,
        control = c(dice_ctrl, trace = FALSE),
        optim.method = "gen"
      )
      acq_func_max <- max_EI(
        model = gp_model,
        type = "UK",
        lower = region$bound_matrix[, 1],
        upper = region$bound_matrix[, 2],
        control = dice_ctrl
      )
      latest_obs <- test_func(acq_func_max$par)
      region_x <- rbind(region_x, acq_func_max$par)
      region_y <- c(region_y, latest_obs)
      a_max <- acq_func_max$value
    } else if (r_package == "ego") {
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
      latest_obs <- tail(bo$y, n = 1)
      region_x = bo$x
      region_y = bo$y
      a_max = bo$ac_val_track
    }
    n <- n + 1
    first_NA_index <- min(which(is.na(run_obs_vec)))
    run_obs_vec[first_NA_index] <- latest_obs
    best_so_far_vec[first_NA_index] <- min(run_obs_vec[(1:first_NA_index)])
    print(sprintf("New observation: %s", latest_obs))
    # print(sprintf("Best so far: %s", best_so_far_vec[first_NA_index]))
    
    new_observed_y = latest_obs # tail(region_y, n = 1)
    if (new_observed_y < region$region_min) {
      region$region_min = new_observed_y
      region$region_argmin = region_x[n, ]
      if (new_observed_y < best_y_so_far) {
        best_y_so_far = new_observed_y
        where_best_y_so_far = region_x[n, ]
        print(sprintf("New best: %s", best_y_so_far))
        print(sprintf("Recorded at: %s", where_best_y_so_far))
      }
    }
    print(sprintf("a_max: %s", a_max))
    
    region$region_x = region_x
    region$region_y = region_y
    region$region_a_max = a_max

    if (a_max < tol) {
      print(sprintf("a_max %s is less than tol %s, so rejecting region.", a_max, tol))
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
  
  print("n_max limit met; splitting region.")
  
  new_subregions = split_and_fit(region = region,
                                 best_y_so_far = best_y_so_far,
                                 where_best_y_so_far = where_best_y_so_far,
                                 run_obs_vec = run_obs_vec,
                                 best_so_far_vec = best_so_far_vec,
                                 split_crit = split_crit)
  
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
