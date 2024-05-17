# gen_points_in_region = function(region, num_points){
#   points = matrix(data=NA, nrow=num_points, ncol=dim)
#   for (r in 1:nrow(points)){
#     points[r, 1] = runif(n = 1, min = region[[1]][[1]], max = region[[1]][[2]])
#     points[r, 2] = runif(n = 1, min = region[[2]][[1]], max = region[[2]][[2]])
#   }
#   obs = apply(points, 1, goldprsc)
#   return(list(points, obs))
# }

gen_points_in_region = function(region, num_points){
  points = matrix(data = NA, nrow = num_points, ncol = dim)
  for(r in 1:nrow(points)){
    for(c in 1:ncol(points)){
      points[r, c] = runif(n = 1, min = region[c, 1], max = region[c, 2])
    }
  }
  obs = apply(points, 1, test_func)
  return(list(points, obs))
}

# bounds_for_optim = function(region){
#   lower_bounds = c(region[[1]][1], region[[2]][1])
#   upper_bounds = c(region[[1]][2], region[[2]][2])
#   return(list(lower_bounds, upper_bounds))
# }

bounds_for_optim = function(region){
  lower_bounds = region[, 1]
  upper_bounds = region[, 2]
  return(list(lower_bounds, upper_bounds))
}

# filter_points_region = function(region, x_points, y_vals){
#   indices_of_points_in_region = which(
#       x_points[, 1] >= region[[1]][[1]] &
#       x_points[, 1] <= region[[1]][[2]] &
#       x_points[, 2] >= region[[2]][[1]] &
#       x_points[, 2] <= region[[2]][[2]]
#   )
#   x_points_in_region = x_points[indices_of_points_in_region, ]
#   y_vals_in_region = y_vals[indices_of_points_in_region]
#   return(list(x_points_in_region, y_vals_in_region))
#   # return(indices_of_points_in_region)
# }

filter_points_region = function(region, x_points, y_vals){
  # row = 1
  in_region = matrix(data = 0, nrow = nrow(x_points), ncol = 1) + 1
  for(row in 1:nrow(x_points)){
    for(col in 1:ncol(x_points)){
      if(x_points[row, col] < region[col, 1] | x_points[row, col] > region[col, 2]){
        in_region[row] = 0
      }
    }
    # if(x_points[row, 1] >= region)
    # row = row + 1
  }
  indices_of_points_in_region = as.logical(in_region)
  x_points_in_region = x_points[indices_of_points_in_region, ]
  y_vals_in_region = y_vals[indices_of_points_in_region]
  return(list(x_points_in_region, y_vals_in_region))
  # return(in_region)
}

# new_filter_points_region = function(region, x_points, y_vals){
#   # row = 1
#   in_region = matrix(data = 0, nrow = nrow(x_points), ncol = 1)
#   for(row in nrow(x_points)){
#     for(col in ncol(x_points)){
#       if(x_points[row, col] >= region[col, 1] & x_points[row, col] <= region[col, 2]){
#         in_region[row] = 1
#       }
#     }
#     # if(x_points[row, 1] >= region)
#     # row = row + 1
#   }
#   return(in_region)
# }

method_1 = function(list_of_regions,
                    list_of_models,
                    # list_of_region_bounds,
                    x_points,
                    y_vals){
  method_1_y_vals = matrix(data = NA, nrow = 1, ncol = length(list_of_regions))
  for (reg in 1:length(list_of_regions)){
    region = list_of_regions[[reg]]
    region_model = list_of_models[[reg]]
    region_lower_bounds = region[, 1]
    region_upper_bounds = region[, 2]
    region_ei = max_EI(model = region_model,
                           type = "SK",
                           lower = region_lower_bounds,
                           upper = region_upper_bounds,
                           minimization = TRUE,
                           control = max_ei_control_list
                           )
    new_point = region_ei$par
    new_y_val = apply(new_point, 1, goldprsc)
    x_points = rbind(x_points, new_point)
    y_vals = c(y_vals, new_y_val)
    method_1_y_vals[1, reg] = new_y_val
  }
  return(list(list(x_points, y_vals),
              method_1_y_vals))
}

method_2 = function(list_of_regions,
                    list_of_models,
                    # list_of_region_bounds,
                    best_val_so_far){
  method_2_ei_vals = matrix(data = NA, nrow = 1, ncol = length(list_of_regions))
  for (reg in 1:length(list_of_regions)){
    region = list_of_regions[[reg]]
    region_model = list_of_models[[reg]]
    region_lower_bounds = region[, 1]
    region_upper_bounds = region[, 2]
    region_ei_val = max_EI(model = region_model,
                           plugin = best_val_so_far,
                           type = "SK",
                           lower = region_lower_bounds,
                           upper = region_upper_bounds,
                           minimization = TRUE,
                           control = max_ei_control_list
                           )
    method_2_ei_vals[1, reg] = region_ei_val$value
  }
  return(method_2_ei_vals)
  # return(region_ei_val)
}

split_and_fit = function(region,
                         x_points,
                         y_vals
                         # index_of_region_to_split
                         ) {
  # this function has to take in one region, and return two regions, right?
  # I probably need to check whether a proposed subregion has no points
  # although maybe the step where we add new points
  # to the region chosen to be split will take care of this
  # dim = length(region)

  points_in_region = filter_points_region(region, x_points, y_vals)
  x_points_in_region = points_in_region[[1]]
  y_vals_in_region = points_in_region[[2]]
  
  new_points_in_region = gen_points_in_region(region, num_points)
  
  x_points_in_region = rbind(x_points_in_region, new_points_in_region[[1]])
  y_vals_in_region = c(y_vals_in_region, new_points_in_region[[2]])
  
  # dim_to_split = 0
  # region_1 = region
  # region_2 = region
  # x_points_in_region_1 = x_points_in_region
  # y_vals_in_region_1 = y_vals_in_region
  # x_points_in_region_2 = x_points_in_region
  # y_vals_in_region_2 = y_vals_in_region
  lowest_y_avg_val = 1e+10
  
  for(d in 1:dim){
    first_region = region
    second_region = region
    split_midpoint = mean(region[d, ])
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
  
  # first_split_midpoint = mean(region[[1]])
  # 
  # first_split_region_1 = list(
  #   c(region[[1]][[1]], first_split_midpoint),
  #   region[[2]]
  # )
  # first_split_region_2 = list(
  #   c(first_split_midpoint, region[[1]][[2]]),
  #   region[[2]]
  # )
  # 
  # second_split_midpoint = mean(region[[2]])
  # 
  # second_split_region_1 = list(
  #   region[[1]],
  #   c(region[[2]][[1]], second_split_midpoint)
  # )
  # second_split_region_2 = list(
  #   region[[1]],
  #   c(second_split_midpoint, region[[2]][[2]])
  # )
  # 
  # points_in_first_split_region_1 = filter_points_region(first_split_region_1, x_points_in_region, y_vals_in_region)
  # x_points_in_first_split_region_1 = points_in_first_split_region_1[[1]]
  # y_vals_in_first_split_region_1 = points_in_first_split_region_1[[2]]
  # y_avg_vals_in_first_split_region_1 = mean(y_vals_in_first_split_region_1)
  # 
  # points_in_first_split_region_2 = filter_points_region(first_split_region_2, x_points_in_region, y_vals_in_region)
  # x_points_in_first_split_region_2 = points_in_first_split_region_2[[1]]
  # y_vals_in_first_split_region_2 = points_in_first_split_region_2[[2]]
  # y_avg_vals_in_first_split_region_2 = mean(y_vals_in_first_split_region_2)
  # 
  # points_in_second_split_region_1 = filter_points_region(second_split_region_1, x_points_in_region, y_vals_in_region)
  # x_points_in_second_split_region_1 = points_in_second_split_region_1[[1]]
  # y_vals_in_second_split_region_1 = points_in_second_split_region_1[[2]]
  # y_avg_vals_in_second_split_region_1 = mean(y_vals_in_second_split_region_1)
  # 
  # points_in_second_split_region_2 = filter_points_region(second_split_region_2, x_points_in_region, y_vals_in_region)
  # x_points_in_second_split_region_2 = points_in_second_split_region_2[[1]]
  # y_vals_in_second_split_region_2 = points_in_second_split_region_2[[2]]
  # y_avg_vals_in_second_split_region_2 = mean(y_vals_in_second_split_region_2)
  # 
  # y_avg_vals = c(
  #   y_avg_vals_in_first_split_region_1, 
  #   y_avg_vals_in_first_split_region_2,
  #   y_avg_vals_in_second_split_region_1,
  #   y_avg_vals_in_second_split_region_2
  # )
  # 
  # if (which.min(y_avg_vals) <= 2) {
  #   first_split_region_1_model = km(~1,
  #                                   design = x_points_in_first_split_region_1,
  #                                   response = y_vals_in_first_split_region_1,
  #                                   covtype = "matern5_2",
  #                                   control = km_control_list,
  #                                   optim.method = "gen"
  #                                   )
  #   first_split_region_2_model = km(~1,
  #                                   design = x_points_in_first_split_region_2,
  #                                   response = y_vals_in_first_split_region_2,
  #                                   covtype = "matern5_2",
  #                                   control = km_control_list,
  #                                   optim.method = "gen"
  #                                   )
  #   # first_split_region_1_bounds = bounds_for_optim(first_split_region_1)
  #   # first_split_region_2_bounds = bounds_for_optim(first_split_region_2)
  # 
  #   return(list(list(rbind(all_x, new_points_in_region[[1]]),
  #                    c(all_y, new_points_in_region[[2]])
  #                    ),
  #               list(first_split_region_1,
  #                    first_split_region_2
  #                    ),
  #               # list(first_split_region_1_bounds,
  #                    # first_split_region_2_bounds
  #                    # ),
  #               list(first_split_region_1_model,
  #                    first_split_region_2_model
  #                    )
  #               )
  #         )
  # } else {
  #   second_split_region_1_model = km(~1,
  #                                    design = x_points_in_second_split_region_1,
  #                                    response = y_vals_in_second_split_region_1,
  #                                    covtype = "matern5_2",
  #                                    control = km_control_list,
  #                                    optim.method = "gen"
  #                                    )
  #   second_split_region_2_model = km(~1,
  #                                    design = x_points_in_second_split_region_2,
  #                                    response = y_vals_in_second_split_region_2,
  #                                    covtype = "matern5_2",
  #                                    control = km_control_list,
  #                                    optim.method = "gen"
  #                                    )
  #   # second_split_region_1_bounds = bounds_for_optim(second_split_region_1)
  #   # second_split_region_2_bounds = bounds_for_optim(second_split_region_2)
  # 
  #   return(list(list(rbind(all_x, new_points_in_region[[1]]),
  #                    c(all_y, new_points_in_region[[2]])
  #                    ),
  #               list(second_split_region_1,
  #                    second_split_region_2
  #                    ),
  #               # list(second_split_region_1_bounds,
  #                    # second_split_region_2_bounds
  #                    # ),
  #               list(second_split_region_1_model,
  #                    second_split_region_2_model
  #                    )
  #               )
  #         )
  # }
}

goldpr <- function(xx)
{
  ##########################################################################
  #
  # GOLDSTEIN-PRICE FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  fact1a <- (x1 + x2 + 1)^2
  fact1b <- 19 - 14*x1 + 3*x1^2 - 14*x2 + 6*x1*x2 + 3*x2^2
  fact1 <- 1 + fact1a*fact1b
  
  fact2a <- (2*x1 - 3*x2)^2
  fact2b <- 18 - 32*x1 + 12*x1^2 + 48*x2 - 36*x1*x2 + 27*x2^2
  fact2 <- 30 + fact2a*fact2b
  
  y <- fact1*fact2
  return(y)
}

goldprsc <- function(xx)
{
  ##########################################################################
  #
  # GOLDSTEIN-PRICE FUNCTION, SCALED
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################
  
  x1bar <- 4*xx[1] - 2
  x2bar <- 4*xx[2] - 2
  
  fact1a <- (x1bar + x2bar + 1)^2
  fact1b <- 19 - 14*x1bar + 3*x1bar^2 - 14*x2bar + 6*x1bar*x2bar + 3*x2bar^2
  fact1 <- 1 + fact1a*fact1b
  
  fact2a <- (2*x1bar - 3*x2bar)^2
  fact2b <- 18 - 32*x1bar + 12*x1bar^2 + 48*x2bar - 36*x1bar*x2bar + 27*x2bar^2
  fact2 <- 30 + fact2a*fact2b
  
  prod <- fact1*fact2
  
  y <- (log(prod) - 8.693) / 2.427
  return(y)
}
