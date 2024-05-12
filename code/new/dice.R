library(MASS)
library(DiceOptim)
source("/Users/jesse/Downloads/welch_research/code/new/helper_funcs.R")

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
# goldprsc_init_points = goldpr_points_func(num_points = num_points, x1_range = x1_ran, x2_range = x2_ran)
goldprsc_init_points = gen_points_in_region(region = list_of_regions[[1]], num_points = num_points)

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

# this parts needs modification; I probably shouldn't be calling EGO.nsteps
# I should probably calculate both the method 1 and method 2 metrics
# for the iniial region

train_x = rbind(
  train_x,
  res.nsteps$par,
  # goldpr_points_func(num_points = num_points, x1_range = x1_ran, x2_range = x2_ran)
  gen_points_in_region(region = list_of_regions[[1]], num_points = num_points)
  )
train_obs = apply(train_x, 1, goldprsc)

# test_region = list(c(0, 0.5), c(0.5, 1))
# test_split = split_func(test_region, train_x, train_obs)
regions_1_and_2 = split_func(list_of_regions[[1]], train_x, train_obs)
region_1_points = filter_points_region_func(regions_1_and_2[[1]], train_x, train_obs)
region_2_points = filter_points_region_func(regions_1_and_2[[2]], train_x, train_obs)
region_1_train_x = region_1_points[[1]]
region_1_train_obs = region_1_points[[2]]
region_2_train_x = region_2_points[[1]]
region_2_train_obs = region_2_points[[2]]
region_1_lower_bounds = c(regions_1_and_2[[1]][[1]][1], regions_1_and_2[[1]][[2]][1])
region_1_upper_bounds = c(regions_1_and_2[[1]][[1]][2], regions_1_and_2[[1]][[2]][2])
region_2_lower_bounds = c(regions_1_and_2[[2]][[1]][1], regions_1_and_2[[2]][[2]][1])
region_2_upper_bounds = c(regions_1_and_2[[2]][[1]][2], regions_1_and_2[[2]][[2]][2])

region_1_model <- km(~1, design=region_1_train_x, response=region_1_train_obs, covtype="matern5_2",
            control=list(pop.size=512, max.generations=50, wait.generations=5, BFGSburnin=5,
                         trace=FALSE), optim.method="gen")
# region_1_res.nsteps <- EGO.nsteps(model=region_1_model,fun=goldprsc, nsteps=1,
#                          lower=region_1_lower_bounds, upper=region_1_upper_bounds,
#                          control=list(pop.size=512, max.generations=50,
#                                       wait.generations=5, BFGSburnin=5),
#                          kmcontrol=NULL)

region_2_model <- km(~1, design=region_2_train_x, response=region_2_train_obs, covtype="matern5_2",
                     control=list(pop.size=512, max.generations=50, wait.generations=5, BFGSburnin=5,
                                  trace=FALSE), optim.method="gen")
# region_2_res.nsteps <- EGO.nsteps(model=region_2_model,fun=goldprsc, nsteps=1,
#                                   lower=region_2_lower_bounds, upper=region_2_upper_bounds,
#                                   control=list(pop.size=512, max.generations=50,
#                                                wait.generations=5, BFGSburnin=5),
#                                   kmcontrol=NULL)

list_of_regions = c(list_of_regions, regions_1_and_2)
list_of_regions = list_of_regions[-1]
# list_of_proposed_points = c(region_1_res.nsteps$par, region_2_res.nsteps$par)
# list_of_test_vals_at_proposed_points = c(region_1_res.nsteps$value, region_2_res.nsteps$val)

# Method 1 for deciding which region to split

m1_ei_region_1 = max_EI(
  model = region_1_model,
  # plugin = smallest_test_val_so_far,
  type = "SK",
  lower = region_1_lower_bounds,
  upper = region_1_upper_bounds,
  minimization = TRUE,
  control = list(
    pop.size=512,
    max.generations=50,
    wait.generations=5,
    BFGSburnin=5
  )
)
m1_ei_region_2 = max_EI(
  model = region_2_model,
  # plugin = smallest_test_val_so_far,
  type = "SK",
  lower = region_2_lower_bounds,
  upper = region_2_upper_bounds,
  minimization = TRUE,
  control = list(
    pop.size=512,
    max.generations=50,
    wait.generations=5,
    BFGSburnin=5
  )
)


m1_region_to_split = list_of_regions[
  which.min(
    c(goldprsc(m1_ei_region_1$par), goldprsc(m1_ei_region_2$par))
  )
]

# Method 2 for deciding which region to split

smallest_test_val_so_far = min(train_obs)
m2_ei_region_1 = max_EI(
  model = region_1_model,
  plugin = smallest_test_val_so_far,
  type = "SK",
  lower = region_1_lower_bounds,
  upper = region_1_upper_bounds,
  minimization = TRUE,
  control = list(
    pop.size=512,
    max.generations=50,
    wait.generations=5,
    BFGSburnin=5
  )
)
m2_ei_region_2 = max_EI(
  model = region_2_model,
  plugin = smallest_test_val_so_far,
  type = "SK",
  lower = region_2_lower_bounds,
  upper = region_2_upper_bounds,
  minimization = TRUE,
  control = list(
    pop.size=512,
    max.generations=50,
    wait.generations=5,
    BFGSburnin=5
  )
)
m2_region_to_split = list_of_regions[
  which.max(
    c(m2_ei_region_1$value, m2_ei_region_2$value)
  )
]

start = Sys.time()



end = Sys.time()
duration = end - start
print(duration)
