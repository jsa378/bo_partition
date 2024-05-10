split_func = function(region, chosen_points, test_func_vals) {
  # this function has to take in one region, and return two regions, right?
  # I probably need to check whether a proposed subregion has no points
  dim = length(region)
  indices_of_points_in_region = which(
    chosen_points[, 1] >= region[[1]][[1]] &
    chosen_points[, 1] <= region[[1]][[2]] &
    chosen_points[, 2] >= region[[2]][[1]] &
    chosen_points[, 2] <= region[[2]][[2]]
  )
  chosen_points_in_region = chosen_points[indices_of_points_in_region, ]
  test_func_vals_in_region = test_func_vals[indices_of_points_in_region]
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
  indices_of_points_in_first_split_region_1 = which(
    chosen_points_in_region[, 1] >= first_split_region_1[[1]][[1]] &
    chosen_points_in_region[, 1] <= first_split_region_1[[1]][[2]] &
    chosen_points_in_region[, 2] >= first_split_region_1[[2]][[1]] &
    chosen_points_in_region[, 2] <= first_split_region_1[[2]][[2]]
  )
  chosen_points_in_first_split_region_1 = chosen_points_in_region[indices_of_points_in_first_split_region_1, ]
  test_func_vals_in_first_split_region_1 = test_func_vals_in_region[indices_of_points_in_first_split_region_1]
  test_func_avg_vals_in_first_split_region_1 = mean(test_func_vals_in_first_split_region_1)
  indices_of_points_in_first_split_region_2 = which(
    chosen_points_in_region[, 1] >= first_split_region_2[[1]][[1]] &
    chosen_points_in_region[, 1] <= first_split_region_2[[1]][[2]] &
    chosen_points_in_region[, 2] >= first_split_region_2[[2]][[1]] &
    chosen_points_in_region[, 2] <= first_split_region_2[[2]][[2]]
  )
  chosen_points_in_first_split_region_2 = chosen_points_in_region[indices_of_points_in_first_split_region_2, ]
  test_func_vals_in_first_split_region_2 = test_func_vals_in_region[indices_of_points_in_first_split_region_2]
  test_func_avg_vals_in_first_split_region_2 = mean(test_func_vals_in_first_split_region_2)
  indices_of_points_in_second_split_region_1 = which(
    chosen_points_in_region[, 1] >= second_split_region_1[[1]][[1]] &
    chosen_points_in_region[, 1] <= second_split_region_1[[1]][[2]] &
    chosen_points_in_region[, 2] >= second_split_region_1[[2]][[1]] &
    chosen_points_in_region[, 2] <= second_split_region_1[[2]][[2]]
  )
  chosen_points_in_second_split_region_1 = chosen_points_in_region[indices_of_points_in_second_split_region_1, ]
  test_func_vals_in_second_split_region_1 = test_func_vals_in_region[indices_of_points_in_second_split_region_1]
  test_func_avg_vals_in_second_split_region_1 = mean(test_func_vals_in_second_split_region_1)
  indices_of_points_in_second_split_region_2 = which(
    chosen_points_in_region[, 1] >= second_split_region_2[[1]][[1]] &
      chosen_points_in_region[, 1] <= second_split_region_2[[1]][[2]] &
      chosen_points_in_region[, 2] >= second_split_region_2[[2]][[1]] &
      chosen_points_in_region[, 2] <= second_split_region_2[[2]][[2]]
  )
  chosen_points_in_second_split_region_2 = chosen_points_in_region[indices_of_points_in_second_split_region_2, ]
  test_func_vals_in_second_split_region_2 = test_func_vals_in_region[indices_of_points_in_second_split_region_2]
  test_func_avg_vals_in_second_split_region_2 = mean(test_func_vals_in_second_split_region_2)
  test_func_avg_vals = c(
    test_func_avg_vals_in_first_split_region_1, 
    test_func_avg_vals_in_first_split_region_2,
    test_func_avg_vals_in_second_split_region_1,
    test_func_avg_vals_in_second_split_region_2
  )
  if (which.max(test_func_avg_vals) <= 2) {
    return(list(first_split_region_1, first_split_region_2))
  } else {
    return(list(second_split_region_1, second_split_region_2))
  }
  # chosen_points_in_region = chosen_points[
  #   chosen_points[, 1] >= region[[1]][[1]] &
  #   chosen_points[, 1] <= region[[1]][[2]] &
  #   chosen_points[, 2] >= region[[2]][[1]] &
  #   chosen_points[, 2] <= region[[2]][[2]]
  #   , 
  # ]
  # test_func_vals_in_region = test_func_vals[
  #   chosen_points[, 1] >= region[[1]][[1]] &
  #   chosen_points[, 1] <= region[[1]][[2]] &
  #   chosen_points[, 2] >= region[[2]][[1]] &
  #   chosen_points[, 2] <= region[[2]][[2]]
  #   , 
  # ]
  # return(chosen_points_in_region)
  # return(test_func_vals_in_region)
  # return(list(first_split_region_1, first_split_region_2))
  # return(list(second_split_region_1, second_split_region_2))
  # return(chosen_points_in_first_split_region_1)
  # return(chosen_points_in_first_split_region_2)
  # return(chosen_points_in_second_split_region_1)
  # return(chosen_points_in_second_split_region_2)
  # return(test_func_vals_in_second_split_region_2)
  # return(test_func_avg_vals_in_second_split_region_2)
}