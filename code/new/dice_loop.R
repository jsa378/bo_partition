library(MASS)
library(DiceOptim)
source("/Users/jesse/Downloads/bo_partition/code/new/helper_funcs.R")
set.seed(1)

num_points = 10
N = 100
goldprsc_x1_range = c(0, 1)
goldprsc_x2_range = c(0, 1)
region_1 = list(goldprsc_x1_range, goldprsc_x2_range)
dim = length(region_1[[1]])
list_of_regions = list(region_1)
region_1_bounds = return_bounds_for_optim(list_of_regions[[1]])
list_of_region_bounds = list(region_1_bounds)
region_1_lower_bounds = region_1_bounds[[1]]
region_1_upper_bounds = region_1_bounds[[2]]

goldprsc_init_points = gen_points_in_region(region = list_of_regions[[1]], num_points = num_points)
# train_x = matrix(data=NA, nrow=num_points, ncol=dim)
# train_x = goldprsc_init_points[1:num_points, ]
train_x = goldprsc_init_points[[1]]
# train_obs = apply(train_x, 1, goldprsc)
train_obs = goldprsc_init_points[[2]]
smallest_test_val_so_far = min(train_obs)
n = length(train_obs)

km_control_list = list(pop.size=512,
                       max.generations=50,
                       wait.generations=5,
                       BFGSburnin=5,
                       trace=FALSE
                       )
max_ei_control_list = list(pop.size=512,
                           max.generations=50,
                           wait.generations=5,
                           BFGSburnin=5
                           )

region_1_model <- km(~1,
                     design=train_x,
                     response=train_obs,
                     covtype="matern5_2",
                     control=km_control_list,
                     optim.method="gen"
                     )

list_of_models = list(region_1_model)

method_2_ei_vals = method_2(list_of_regions = list_of_regions,
                            list_of_models = list_of_models,
                            list_of_region_bounds = list_of_region_bounds,
                            best_val_so_far = smallest_test_val_so_far
                            )

while(n < N){
  index_of_region_to_split = which.min(method_2_ei_vals)
  
  n = length(train_obs)
}