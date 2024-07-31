library(GaSP)
library(EGOmod)

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 5){
  stop("Five arguments must be supplied: test function (string), dim (int), num init obs (int), num runs (int) and save dir (no type)", call. = FALSE)
}

test_func_name = args[1]
dim = as.integer(args[2])
num_init_obs = as.integer(args[3])
num_runs = as.integer(args[4])
save_dir = as.character(args[5])

source("/home/jsa378/bo_partition/code/test_funcs.R")

x_names_arg = character(0)
for (d in 1:dim){
  x_names_arg = c(x_names_arg, sprintf("x%s", d))
}

test_func = test_func_list[[test_func_name]]$func
test_lbound = test_func_list[[test_func_name]]$lbound
test_ubound = test_func_list[[test_func_name]]$ubound

descr = DescribeX(
  x_names = x_names_arg,
  x_min = test_lbound,
  x_max = test_ubound,
  support = rep("Continuous", dim)
)

for(run in 1:num_runs){
  init = Initialize(
    n_design = num_init_obs,
    x_describe = descr,
    fun = test_func,
    n_rep = 0
  )
  
  write.table(init$x_design,
              file = sprintf("%srun_%s_init_points.csv", save_dir, run),
              row.names = FALSE,
              col.names = FALSE
  )
}
