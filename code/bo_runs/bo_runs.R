library(GaSP)
library(EGO)
source("/Users/jesse/Downloads/bo_partition/code/test_funcs.R")

set.seed(1)

test_func_string = c("ackley")
num_init_obs = 20
num_obs = 20
num_runs = 2
img_width = 2000
img_height = 2000
tot_obs = num_init_obs + num_obs
x_names_arg = character(0)
for (d in 1:dim){
  x_names_arg = c(x_names_arg, sprintf("x%s", d))
}
run_obs = matrix(data = NA, nrow = num_runs, ncol = num_obs)

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
colors = c(rep("green", num_init_obs), rep("blue", num_obs))
start = Sys.time()

for(run in 1:num_runs){
  print(sprintf("Beginning run %s of %s", run, num_runs))
  init = Initialize(
    n_design = num_init_obs,
    x_describe = descr,
    fun = ackley,
    n_rep = 0
  )
  
  train_gp = EGO(
    fun = ackley,
    reg_model = ~1,
    ego_init = init,
    x_describe = descr,
    nsteps = num_obs,
    control = ctrl
  )
  
  png(filename = sprintf("/Users/jesse/Downloads/bo_partition/plots/bo_runs/%s_ego_plot_run_%s.png", test_func_string, run),
      width = img_width,
      height = img_height)
  EGO.plot(ego_fit = train_gp,
           fun = ackley,
           n.grid = 1000,
           x_describe = descr,
           control = list(limit_min = descr$Min,
                          limit_max = descr$Max,
                          label_order = FALSE)
  )
  # par(cex = 5)
  points(ackley_argmin[1], ackley_argmin[2], pch=4, col="red", cex=5)
  text(train_gp$x, col=colors, label=1:tot_obs, cex = 5)
  dev.off()
  
  plot_title = sprintf(paste(test_func_string, ", run %s", sep=""), run)
  png(filename = sprintf("/Users/jesse/Downloads/bo_partition/plots/bo_runs/%s_numbered_points_run_%s.png", test_func_string, run),
      width = img_width,
      height = img_height)
  par(cex = 5)
  plot(train_gp$x, col=colors, pch=paste(1:tot_obs), type='n', main=plot_title)
  points(ackley_argmin[1], ackley_argmin[2], pch=4, col="red", cex = 5)
  text(train_gp$x, col=colors, label=1:tot_obs)
  dev.off()
  run_obs[run, ] = train_gp$y[-(1:num_init_obs)]
}

write.table(run_obs,
          file = sprintf("/Users/jesse/Downloads/bo_partition/data/bo_runs/%s_obs.csv", test_func_string),
          row.names = FALSE,
          col.names = FALSE
          )

# init = Initialize(
#   # x_design = train_x,
#   # y_design = train_obs,
#   n_design = num_init_obs,
#   x_describe = descr,
#   # fun = goldpr
#   # fun = goldprsc
#   fun = ackley,
#   n_rep = 0
# )
# 
# train_gp = EGO(
#   fun = ackley,
#   reg_model = ~1,
#   ego_init = init,
#   x_describe = descr,
#   nsteps = num_obs,
#   control = ctrl
# )
# 
# 
# 
# EGO.plot(ego_fit = train_gp,
#          fun = ackley,
#          n.grid = 1000,
#          x_describe = descr,
#          control = list(# limit_min = ackley_lbound_scalar,
#                         # limit_max = ackley_ubound_scalar,
#                         limit_min = descr$Min,
#                         limit_max = descr$Max,
#                         label_order = FALSE)
# )
# # text(train_gp$x + 0, symbols)
# 
# # symbols = character(0)
# # colors = character(0)
# # 
# # for (p in 1:(num_init_obs + num_obs)){
# #   symbols = c(symbols, sprintf("%s", p))
# #   if(p <= num_init_obs){
# #     colors = c(colors, "green")
# #   }
# #   else{
# #     colors = c(colors, "blue")
# #   }
# # }
# # plot(train_gp$x, col=colors, pch=as.character(1:40))
# 
# plot_title = sprintf(paste(test_func_string, ", run %s", sep=""), 4)
# plot(train_gp$x, col=colors, pch=paste(1:tot_obs), type='n', main=test_func_string)
# text(train_gp$x, col=colors, label=1:tot_obs)

end = Sys.time()
duration = end - start
print(duration)