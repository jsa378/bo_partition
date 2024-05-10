library(MASS)
library(DiceOptim)
source("/Users/jesse/Downloads/welch_research/code/test_funcs.R")
source("/Users/jesse/Downloads/welch_research/code/goldpr_points_func.R")
source("/Users/jesse/Downloads/welch_research/code/split_func.R")
source("/Users/jesse/Downloads/welch_research/code/filter_points_region_func.R")

dim = 2
num_points = 10
n_trials = 10
# goldpr_lower_bound = -2
# goldpr_upper_bound = 2
# x1_ran = c(goldpr_lower_bound, goldpr_upper_bound)
# x2_ran = c(goldpr_lower_bound, goldpr_upper_bound)
goldprsc_lower_bound = 0
goldprsc_upper_bound = 1
x1_ran = c(goldprsc_lower_bound, goldprsc_upper_bound)
x2_ran = c(goldprsc_lower_bound, goldprsc_upper_bound)
X = list(x1_ran, x2_ran)
list_of_regions = list(X)
lower_bound = c(x1_ran[1], x2_ran[1])
upper_bound = c(x1_ran[2], x2_ran[2])
set.seed(1)

# goldpr_init_points = read.csv("/Users/jesse/Downloads/welch_research/data/goldpr_init_points.csv", header = FALSE, sep = ",")
# goldpr_init_points = goldpr_points_func(num_points = num_points * n_trials, x1_range = x1_ran, x2_range = x2_ran)
goldprsc_init_points = goldpr_points_func(num_points = num_points, x1_range = x1_ran, x2_range = x2_ran)

train_x = matrix(data=NA, nrow=num_points, ncol=dim)
# train_x = goldpr_init_points[1:num_init_points, ]
train_x = goldprsc_init_points[1:num_points, ]
# train_obs = apply(train_x, 1, goldpr)
train_obs = apply(train_x, 1, goldprsc)
model <- km(~1, design=train_x, response=train_obs, covtype="matern5_2",
               control=list(pop.size=512, max.generations=50, wait.generations=5, BFGSburnin=5,
                            trace=FALSE), optim.method="gen")
res.nsteps <- EGO.nsteps(model=model,fun=goldprsc, nsteps=1,
                         lower=lower_bound, upper=upper_bound,
                         control=list(pop.size=512, max.generations=50,
                                      wait.generations=5, BFGSburnin=5),
                         kmcontrol=NULL)

train_x = rbind(
  train_x,
  res.nsteps$par,
  goldpr_points_func(num_points = num_points, x1_range = x1_ran, x2_range = x2_ran)
  )
train_obs = apply(train_x, 1, goldprsc)

# test_region = list(c(0, 0.5), c(0.5, 1))
# test_split = split_func(test_region, train_x, train_obs)
regions_1_and_2 = split_func(list_of_regions[[1]], train_x, train_obs)
region_1_points = filter_points_region_func(regions_1_and_2[[1]], train_x, train_obs)
region_2_points = filter_points_region_func(regions_1_and_2[[2]], train_x, train_obs)
region_1_train_x = region_1_points[1][[1]]
region_1_train_obs = region_1_points[2][[1]]
region_2_train_x = region_1_points[1][[1]]
region_2_train_obs = region_1_points[2][[1]]
region_1_lower_bounds = c(regions_1_and_2[[1]][[1]][1], regions_1_and_2[[1]][[2]][1])
region_1_upper_bounds = c(regions_1_and_2[[1]][[1]][2], regions_1_and_2[[1]][[2]][2])
region_2_lower_bounds = c(regions_1_and_2[[2]][[1]][1], regions_1_and_2[[2]][[2]][1])
region_2_upper_bounds = c(regions_1_and_2[[2]][[1]][2], regions_1_and_2[[2]][[2]][2])

region_1_model <- km(~1, design=region_1_train_x, response=region_1_train_obs, covtype="matern5_2",
            control=list(pop.size=512, max.generations=50, wait.generations=5, BFGSburnin=5,
                         trace=FALSE), optim.method="gen")
region_1_res.nsteps <- EGO.nsteps(model=region_1_model,fun=goldprsc, nsteps=1,
                         lower=region_1_lower_bounds, upper=region_1_upper_bounds,
                         control=list(pop.size=512, max.generations=50,
                                      wait.generations=5, BFGSburnin=5),
                         kmcontrol=NULL)

region_2_model <- km(~1, design=region_2_train_x, response=region_2_train_obs, covtype="matern5_2",
                     control=list(pop.size=512, max.generations=50, wait.generations=5, BFGSburnin=5,
                                  trace=FALSE), optim.method="gen")
region_2_res.nsteps <- EGO.nsteps(model=region_2_model,fun=goldprsc, nsteps=1,
                                  lower=region_2_lower_bounds, upper=region_2_upper_bounds,
                                  control=list(pop.size=512, max.generations=50,
                                               wait.generations=5, BFGSburnin=5),
                                  kmcontrol=NULL)

start = Sys.time()



end = Sys.time()
duration = end - start
print(duration)
