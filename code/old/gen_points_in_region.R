# library(MASS)
# dim = 2
# num_init_points = 10
# n_trials = 10
# 
# goldpr_lower_bound = -2
# goldpr_upper_bound = 2
# 
# x1_ran = c(goldpr_lower_bound, goldpr_upper_bound)
# x2_ran = c(goldpr_lower_bound, goldpr_upper_bound)
# 
# set.seed(1)

gen_points_in_region = function(region, num_points){
  points = matrix(data=NA, nrow=num_points, ncol=dim)
  for (r in 1:nrow(points)){
    points[r, 1] = runif(n = 1, min = region[[1]][[1]], max = region[[1]][[2]])
    points[r, 2] = runif(n = 1, min = region[[2]][[1]], max = region[[2]][[2]])
  }
  return(points)
}

goldpr_points_func = function(num_points, x1_range, x2_range){
  goldpr_points = matrix(data=NA, nrow=num_points, ncol=dim)
  # ranges = c(x1_range, x2_range)
  for (r in 1:nrow(goldpr_points)){
    # for (c in 1:ncol(goldpr_points)){
    #   goldpr_points[r, c] = runif(n = 1, min = ranges[c][c], max = ranges[c][c])
    # }
    goldpr_points[r, 1] = runif(n = 1, min = x1_range[1], max = x1_range[2])
    goldpr_points[r, 2] = runif(n = 1, min = x2_range[1], max = x2_range[2])
  }
  return(goldpr_points)
}

# test = goldpr_points_func(num_points = num_init_points, x1_range = x1_ran, x2_range = x2_ran)
# 
# goldpr_init_points = matrix(data=NA, nrow=num_init_points * n_trials, ncol=dim)
# 
# for (n in 1:nrow(goldpr_init_points)){
#   goldpr_init_points[n, ] = runif(dim, min = goldpr_lower_bound, max = goldpr_upper_bound)
# }
# 
# write.matrix(goldpr_init_points, file="/Users/jesse/Downloads/welch_research/data/goldpr_init_points.csv", sep = ",")
