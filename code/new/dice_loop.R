library(MASS)
library(DiceOptim)
source("/Users/jesse/Downloads/bo_partition/code/new/helper_funcs.R")
set.seed(1)

num_points = 10
N = 20
goldprsc_x1_range = c(0, 1)
goldprsc_x2_range = c(0, 1)
region_1 = list(goldprsc_x1_range, goldprsc_x2_range)
dim = length(region_1[[1]])
all_regions = list(region_1)
region_1_bounds = bounds_for_optim(all_regions[[1]])
all_region_bounds = list(region_1_bounds)
region_1_lower_bounds = region_1_bounds[[1]]
region_1_upper_bounds = region_1_bounds[[2]]

goldprsc_init_points = gen_points_in_region(region = all_regions[[1]], num_points = num_points)
all_x = goldprsc_init_points[[1]]
all_y = goldprsc_init_points[[2]]
smallest_y_so_far = min(all_y)
n = length(all_y)

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
                     design=all_x,
                     response=all_y,
                     covtype="matern5_2",
                     control=km_control_list,
                     optim.method="gen"
                     )

all_models = list(region_1_model)

# method_2_ei_vals = method_2(list_of_regions = all_regions,
#                             list_of_models = all_models,
#                             list_of_region_bounds = all_region_bounds,
#                             best_val_so_far = smallest_y_so_far
#                             )
# Should the above call be done inside the while loop below?

while(n < N){
  method_2_ei_vals = method_2(list_of_regions = all_regions,
                              list_of_models = all_models,
                              list_of_region_bounds = all_region_bounds,
                              best_val_so_far = smallest_y_so_far
                              )
  index_of_region_to_split = which.max(method_2_ei_vals)
  region_to_split = all_regions[[index_of_region_to_split]]
  results = split_and_fit(region = region_to_split,
                x_points = all_x,
                y_vals = all_y
                # index_of_region_to_split = index_of_region_to_split
                )
  all_x = results[[1]][[1]]
  all_y = results[[1]][[2]]
  n = length(all_y)
  smallest_y_so_far = min(all_y)

  first_new_region = results[[2]][[1]]
  second_new_region = results[[2]][[2]]
  all_regions = all_regions[-index_of_region_to_split]
  # all_regions = c(all_regions, first_new_region, second_new_region)
  all_regions = c(all_regions, list(first_new_region, second_new_region))
  
  first_new_region_bounds = results[[3]][[1]]
  second_new_region_bounds = results[[3]][[2]]
  all_region_bounds = all_region_bounds[-index_of_region_to_split]
  # all_region_bounds = c(all_region_bounds, first_new_region_bounds, second_new_region_bounds)
  all_region_bounds = c(all_region_bounds, list(first_new_region_bounds, second_new_region_bounds))
  
  first_new_region_model = results[[4]][[1]]
  second_new_region_model = results[[4]][[2]]
  all_models = all_models[-index_of_region_to_split]
  # all_models = c(all_models, first_new_region_model, second_new_region_model)
  all_models = c(all_models, list(first_new_region_model, second_new_region_model))

  # new_regions = split[[1]]
  # new_models = 
  # n = length(all_y)
  # n = N
}