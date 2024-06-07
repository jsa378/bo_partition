library(GaSP)
library(EGO)

seed_value = 1
test_func_name = "rastr"
dim = 5
num_init_obs = 20
num_obs = 1
num_runs = 10
save_dir = "/Users/jesse/Downloads/cedar_test_output"

source("/Users/jesse/Downloads/bo_partition/code/test_funcs.R")
source("/Users/jesse/Downloads/bo_partition/code/new/arbitrary_dim/helper_funcs.R")

paste(c("Bayesian optimization with seed value:", seed_value), collapse = " ")
paste(c("Test function:", test_func_name), collapse = " ")
paste(c("Dimension:", dim), collapse = " ")
paste(c("Number of initial observations:", num_init_obs), collapse = " ")
paste(c("Number of observations:", num_obs), collapse = " ")
paste(c("Number of runs:", num_runs), collapse = " ")
paste(c("Save directory:", save_dir), collapse = " ")

set.seed(seed_value)

test_func_string = test_func_name
img_width = 2000
img_height = 2000
tot_obs = num_init_obs + num_obs
# reg_tot_obs = num_init_obs + reg_obs
x_names_arg = character(0)
for (d in 1:dim){
  x_names_arg = c(x_names_arg, sprintf("x%s", d))
}
run_obs = matrix(data = NA, nrow = num_runs, ncol = num_obs)
best_so_far = matrix(data = NA, nrow = num_runs, ncol = num_obs)

test_func = test_func_list[[test_func_name]]$func
test_lbound_scalar = test_func_list[[test_func_name]]$lbound_scalar
test_ubound_scalar = test_func_list[[test_func_name]]$ubound_scalar
test_lbound = test_func_list[[test_func_name]]$lbound
test_ubound = test_func_list[[test_func_name]]$ubound
plot_lims = c(test_lbound_scalar, test_ubound_scalar)
test_argmin = test_func_list[[test_func_name]]$argmin

paste(c("Test function lower bound scalar:", test_lbound_scalar), collapse = " ")
paste(c("Test function upper bound scalar:", test_ubound_scalar), collapse = " ")
paste(c("Test function lower bound vector:", test_lbound), collapse = " ")
paste(c("Test function upper bound vector:", test_ubound), collapse = " ")
paste(c("Test function argmin:", test_argmin), collapse = " ")

descr = DescribeX(
  x_names = x_names_arg,
  x_min = test_lbound,
  x_max = test_ubound,
  support = rep("Continuous", dim)
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
  print_level = 2
)

colors = c(rep("green", num_init_obs), rep("blue", num_obs))
start = Sys.time()

init = Initialize(
  n_design = num_init_obs,
  x_describe = descr,
  fun = test_func,
  n_rep = 0
)

bo_1 = EGO(
  fun = test_func,
  reg_model = ~1,
  ego_init = init,
  x_describe = descr,
  nsteps = 5,
  control = ctrl
)

# print(paste("init x pts and y vals:"))
# print(init$x_design)
# print(init$y_design)

for(run in 1:5){
  # print(sprintf("Beginning run %s of %s", run, num_runs))
  
  # print(paste("init x pts and y vals:"))
  # print(init$x_design)
  # print(init$y_design)
  
  bo = EGO(
    fun = test_func,
    reg_model = ~1,
    ego_init = init,
    x_describe = descr,
    nsteps = 1,
    control = ctrl
  )
  # 
  # print(paste("BO visited x and observed y:"))
  # print(bo$x[-(1:(num_init_obs + run - 1)), ])
  # print(bo$y[-(1:(num_init_obs + run - 1))])
  
  x_pts = bo$x
  y_vals = bo$y

  init = Initialize(
    x_design = x_pts,
    y_design = y_vals,
    x_describe = descr,
    fun = test_func
  )

}

identical(bo_1$x[1:20, ], bo$x[1:20, ])
identical(bo_1$y[1:20], bo$y[1:20])
bo_1$x[-(1:20), ]
bo$x[-(1:20), ]
bo_1$y[-(1:20)]
bo$y[-(1:20)]

# print(paste("init x pts and y vals:"))
# print(init$x_design)
# print(init$y_design)

end = Sys.time()
duration = end - start
print(duration)
