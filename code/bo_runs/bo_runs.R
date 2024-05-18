library(GaSP)
library(EGO)
source("/Users/jesse/Downloads/bo_partition/code/test_funcs.R")

set.seed(1)

dim = 5
ackley_lbound = rep(ackley_lbound_scalar, dim)
ackley_ubound = rep(ackley_ubound_scalar, dim)
num_init_obs = 20
num_obs = 100
x_names_arg = character(0)
for (d in 1:dim){
  x_names_arg = c(x_names_arg, sprintf("x%s", d))
}

descr = DescribeX(
  # x_names = c("x1", "x2"),
  x_names = x_names_arg,
  x_min = ackley_lbound,
  x_max = ackley_ubound,
  # x_min = c(goldpr_lower_bound, goldpr_lower_bound),
  # x_max = c(goldpr_upper_bound, goldpr_upper_bound),
  # x_min = c(x1_ran[1], x2_ran[1]),
  # x_max = c(x1_ran[2], x2_ran[2]),
  support = rep("Continuous", dim)
)
init = Initialize(
  # x_design = train_x,
  # y_design = train_obs,
  n_design = num_init_obs,
  x_describe = descr,
  # fun = goldpr
  # fun = goldprsc
  fun = ackley,
  n_rep = 0
)
ctrl = EGO.control(
  alg = "genoud",
  rel_tol = 0,
  wait_iter = 10,
  acq_control = list(type = "EI"),
  GaSP_control = list(cor_family = "PowerExponential"),
  genoud_control = list(pop.size=512,
                        max.generations=50,
                        wait.generations=5,
                        BFGSburnin=5,
                        trace=FALSE
  ),
  print_level = 1
)
start = Sys.time()
train_gp = EGO(
  fun = ackley,
  reg_model = ~1,
  ego_init = init,
  x_describe = descr,
  nsteps = num_obs,
  control = ctrl
)

end = Sys.time()
duration = end - start
print(duration)