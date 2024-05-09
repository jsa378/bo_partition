library(MASS)
library(DiceOptim)
source("/Users/jesse/Downloads/welch_research/code/test_funcs.R")
source("/Users/jesse/Downloads/welch_research/code/goldpr_points_func.R")

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
region_1 = list(x1_ran, x2_ran)
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


start = Sys.time()



end = Sys.time()
duration = end - start
print(duration)
