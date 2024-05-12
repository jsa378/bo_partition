library(MASS)
library(GaSP)
library(EGO)
source("/Users/jesse/Downloads/welch_research/code/test_funcs.R")
source("/Users/jesse/Downloads/welch_research/code/goldpr_points_func.R")

dim = 2
num_init_points = 10
n_trials = 10
# goldpr_lower_bound = -2
# goldpr_upper_bound = 2
# x1_ran = c(goldpr_lower_bound, goldpr_upper_bound)
# x2_ran = c(goldpr_lower_bound, goldpr_upper_bound)
goldprsc_lower_bound = 0
goldprsc_upper_bound = 1
x1_ran = c(goldprsc_lower_bound, goldprsc_upper_bound)
x2_ran = c(goldprsc_lower_bound, goldprsc_upper_bound)
set.seed(1)

# goldpr_init_points = read.csv("/Users/jesse/Downloads/welch_research/data/goldpr_init_points.csv", header = FALSE, sep = ",")
# goldpr_init_points = goldpr_points_func(num_points = num_init_points * n_trials, x1_range = x1_ran, x2_range = x2_ran)
goldprsc_init_points = goldpr_points_func(num_points = num_init_points * n_trials, x1_range = x1_ran, x2_range = x2_ran)

train_x = matrix(data=NA, nrow=num_init_points, ncol=dim)
# train_x = goldpr_init_points[1:num_init_points, ]
train_x = goldprsc_init_points[1:num_init_points, ]
# train_obs = apply(train_x, 1, goldpr)
train_obs = apply(train_x, 1, goldprsc)
descr = DescribeX(
  x_names = c("x1", "x2"),
  # x_min = c(goldpr_lower_bound, goldpr_lower_bound),
  # x_max = c(goldpr_upper_bound, goldpr_upper_bound),
  x_min = c(x1_ran[1], x2_ran[1]),
  x_max = c(x1_ran[2], x2_ran[2]),
  support = c("Continuous", "Continuous")
)
init = Initialize(
  x_design = train_x,
  y_design = train_obs,
  x_describe = descr,
  # fun = goldpr
  fun = goldprsc
)
ctrl = EGO.control(
  alg = "genoud",
  rel_tol = 0,
  wait_iter = 10,
  acq_control = list(type = "EI"),
  GaSP_control = list(cor_family = "Matern"),
  genoud_control = list(pop.size=512,
                        max.generations=50,
                        wait.generations=5,
                        BFGSburnin=5,
                        trace=FALSE
                        ),
  print_level = 1
)
train_gp = EGO(
  # fun = goldpr,
  fun = goldprsc,
  reg_model = ~1,
  ego_init = init,
  x_describe = descr,
  nsteps = 1,
  control = ctrl
)

start = Sys.time()



end = Sys.time()
duration = end - start
print(duration)
