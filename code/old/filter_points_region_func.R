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