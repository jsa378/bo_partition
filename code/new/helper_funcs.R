gen_points_in_region = function(region, num_points){
  points = matrix(data=NA, nrow=num_points, ncol=dim)
  for (r in 1:nrow(points)){
    points[r, 1] = runif(n = 1, min = region[[1]][[1]], max = region[[1]][[2]])
    points[r, 2] = runif(n = 1, min = region[[2]][[1]], max = region[[2]][[2]])
  }
  return(points)
}

filter_points_region_func = function(region, chosen_points, test_func_vals){
  indices_of_points_in_region = which(
      chosen_points[, 1] >= region[[1]][[1]] &
      chosen_points[, 1] <= region[[1]][[2]] &
      chosen_points[, 2] >= region[[2]][[1]] &
      chosen_points[, 2] <= region[[2]][[2]]
  )
  chosen_points_in_region = chosen_points[indices_of_points_in_region, ]
  test_func_vals_in_region = test_func_vals[indices_of_points_in_region]
  return(list(chosen_points_in_region, test_func_vals_in_region))
}

split_func = function(region, chosen_points, test_func_vals) {
  # this function has to take in one region, and return two regions, right?
  # I probably need to check whether a proposed subregion has no points
  # although maybe the step where we add new points
  # to the region chosen to be split will take care of this
  dim = length(region)

  points_in_region = filter_points_region_func(region, chosen_points, test_func_vals)
  chosen_points_in_region = points_in_region[[1]]
  test_func_vals_in_region = points_in_region[[2]]
  first_split_midpoint = mean(region[[1]])
  first_split_region_1 = list(
    c(region[[1]][[1]], first_split_midpoint),
    region[[2]]
  )
  first_split_region_2 = list(
    c(first_split_midpoint, region[[1]][[2]]),
    region[[2]]
  )
  second_split_midpoint = mean(region[[2]])
  second_split_region_1 = list(
    region[[1]],
    c(region[[2]][[1]], second_split_midpoint)
  )
  second_split_region_2 = list(
    region[[1]],
    c(second_split_midpoint, region[[2]][[2]])
  )

  points_in_first_split_region_1 = filter_points_region_func(first_split_region_1, chosen_points_in_region, test_func_vals_in_region)
  chosen_points_in_first_split_region_1 = points_in_first_split_region_1[[1]]
  test_func_vals_in_first_split_region_1 = points_in_first_split_region_1[[2]]
  test_func_avg_vals_in_first_split_region_1 = mean(test_func_vals_in_first_split_region_1)

  points_in_first_split_region_2 = filter_points_region_func(first_split_region_2, chosen_points_in_region, test_func_vals_in_region)
  chosen_points_in_first_split_region_2 = points_in_first_split_region_2[[1]]
  test_func_vals_in_first_split_region_2 = points_in_first_split_region_2[[2]]
  test_func_avg_vals_in_first_split_region_2 = mean(test_func_vals_in_first_split_region_2)

  points_in_second_split_region_1 = filter_points_region_func(second_split_region_1, chosen_points_in_region, test_func_vals_in_region)
  chosen_points_in_second_split_region_1 = points_in_second_split_region_1[[1]]
  test_func_vals_in_second_split_region_1 = points_in_second_split_region_1[[2]]
  test_func_avg_vals_in_second_split_region_1 = mean(test_func_vals_in_second_split_region_1)

  points_in_second_split_region_2 = filter_points_region_func(second_split_region_2, chosen_points_in_region, test_func_vals_in_region)
  chosen_points_in_second_split_region_2 = points_in_second_split_region_2[[1]]
  test_func_vals_in_second_split_region_2 = points_in_second_split_region_2[[2]]
  test_func_avg_vals_in_second_split_region_2 = mean(test_func_vals_in_second_split_region_2)
  test_func_avg_vals = c(
    test_func_avg_vals_in_first_split_region_1, 
    test_func_avg_vals_in_first_split_region_2,
    test_func_avg_vals_in_second_split_region_1,
    test_func_avg_vals_in_second_split_region_2
  )
  if (which.min(test_func_avg_vals) <= 2) {
    return(list(first_split_region_1, first_split_region_2))
  } else {
    return(list(second_split_region_1, second_split_region_2))
  }
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
