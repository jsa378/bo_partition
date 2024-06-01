library(GaSP)
library(EGO)
source("/home/jsa378/bo_partition/code/test_funcs.R")
source("/home/jsa378/bo_partition/code/new/arbitrary_dim/helper_funcs.R")

set.seed(1)

test_func_string = c("rastr")
num_init_obs = 10
# reg_init_obs = num_init_obs / 2
num_obs = 10
# reg_obs = num_obs / 2
# reg_obs = num_obs / 4
num_runs = 2
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
# reg_1_run_obs = matrix(data = NA, nrow = num_runs, ncol = reg_obs)
# reg_1_best_so_far = matrix(data = NA, nrow = num_runs, ncol = reg_obs)
# reg_2_run_obs = matrix(data = NA, nrow = num_runs, ncol = reg_obs)
# reg_2_best_so_far = matrix(data = NA, nrow = num_runs, ncol = reg_obs)

test_func = rastr
test_lbound_scalar = rastr_lbound_scalar
test_ubound_scalar = rastr_ubound_scalar
plot_lims = c(test_lbound_scalar, test_ubound_scalar)
test_lbound = rastr_lbound
test_ubound = rastr_ubound
# test_reg_1_lbound = cit_reg_1_lbound
# test_reg_1_ubound = cit_reg_1_ubound
# test_reg_2_lbound = grie_reg_2_lbound
# test_reg_2_ubound = grie_reg_2_ubound
# test_reg_1 = cit_reg_1
# test_reg_2 = grie_reg_2
test_argmin = rastr_argmin

descr = DescribeX(
  x_names = x_names_arg,
  x_min = test_lbound,
  x_max = test_ubound,
  support = rep("Continuous", dim)
)
# reg_1_descr = DescribeX(
#   x_names = x_names_arg,
#   x_min = test_reg_1_lbound,
#   x_max = test_reg_1_ubound,
#   support = rep("Continuous", dim)
# )
# reg_2_descr = DescribeX(
#   x_names = x_names_arg,
#   x_min = test_reg_2_lbound,
#   x_max = test_reg_2_ubound,
#   support = rep("Continuous", dim)
# )
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
    fun = test_func,
    n_rep = 0
  )
  # reg_1_points = filter_points_region(test_reg_1, init$x_design, init$y_design)
  # reg_1_x = reg_1_points[[1]]
  # reg_1_y = reg_1_points[[2]]
  # reg_1_init = Initialize(
  #   x_design = reg_1_x,
  #   y_design = reg_1_y,
  #   x_describe = reg_1_descr,
  #   fun = test_func,
  #   n_rep = 0
  # )

  # reg_2_points = filter_points_region(test_reg_2, init$x_design, init$y_design)
  # reg_2_x = reg_2_points[[1]]
  # reg_2_y = reg_2_points[[2]]
  # reg_2_init = Initialize(
  #   x_design = reg_2_x,
  #   y_design = reg_2_y,
  #   x_describe = reg_2_descr,
  #   fun = test_func,
  #   n_rep = 0
  # )
  # reg_1_init = Initialize(
  #   n_design = reg_init_obs,
  #   x_describe = reg_1_descr,
  #   fun = test_func,
  #   n_rep = 0
  # )
  # reg_2_init = Initialize(
  #   n_design = reg_init_obs,
  #   x_describe = reg_2_descr,
  #   fun = test_func,
  #   n_rep = 0
  # )
  
  # reg_1_num_init_obs = nrow(reg_1_x)
  # reg_2_num_init_obs = nrow(reg_2_x)
  # reg_1_num_init_obs = reg_init_obs
  # reg_2_num_init_obs = reg_init_obs
  
  # reg_1_tot_obs = reg_1_num_init_obs + reg_obs
  # reg_2_tot_obs = reg_2_num_init_obs + reg_obs
  
  # reg_1_colors = c(rep("green", reg_1_num_init_obs), rep("blue", reg_obs))
  # reg_2_colors = c(rep("green", reg_2_num_init_obs), rep("blue", reg_obs))
  
  bo = EGO(
    fun = test_func,
    reg_model = ~1,
    ego_init = init,
    x_describe = descr,
    nsteps = num_obs,
    control = ctrl
  )
  # reg_1_bo = EGO(
  #   fun = test_func,
  #   reg_model = ~1,
  #   ego_init = reg_1_init,
  #   x_describe = reg_1_descr,
  #   nsteps = reg_obs,
  #   control = ctrl
  # )
  # reg_2_bo = EGO(
  #   fun = test_func,
  #   reg_model = ~1,
  #   ego_init = reg_2_init,
  #   x_describe = reg_2_descr,
  #   nsteps = reg_obs,
  #   control = ctrl
  # )
  
  png(filename = sprintf("/home/jsa378/bo_partition/plots/bo_runs/%s_ego_plot_run_%s.png", test_func_string, run),
      width = img_width,
      height = img_height)
  EGO.plot(ego_fit = bo,
           fun = test_func,
           n.grid = 1000,
           x_describe = descr,
           control = list(limit_min = descr$Min,
                          limit_max = descr$Max,
                          label_order = FALSE)
  )
  points(test_argmin[1], test_argmin[2], pch=4, col="red", cex=5)
  text(bo$x, col=colors, label=1:tot_obs, cex = 5)
  dev.off()
  
  # png(filename = sprintf("/Users/jesse/Downloads/bo_partition/plots/bo_runs/%s_reg_1_ego_plot_run_%s.png", test_func_string, run),
  #     width = img_width,
  #     height = img_height)
  # EGO.plot(ego_fit = reg_1_bo,
  #          fun = test_func,
  #          n.grid = 1000,
  #          x_describe = reg_1_descr,
  #          control = list(limit_min = descr$Min,
  #                         limit_max = descr$Max,
  #                         label_order = FALSE)
  # )
  # points(test_argmin[, 1], test_argmin[, 2], pch=4, col="red", cex=5)
  # text(reg_1_bo$x, col=reg_1_colors, label=1:reg_1_tot_obs, cex = 5)
  # dev.off()
  
  # png(filename = sprintf("/Users/jesse/Downloads/bo_partition/plots/bo_runs/%s_reg_2_ego_plot_run_%s.png", test_func_string, run),
  #     width = img_width,
  #     height = img_height)
  # EGO.plot(ego_fit = reg_2_bo,
  #          fun = test_func,
  #          n.grid = 1000,
  #          x_describe = reg_2_descr,
  #          control = list(limit_min = descr$Min,
  #                         limit_max = descr$Max,
  #                         label_order = FALSE)
  # )
  # points(test_argmin[1], test_argmin[2], pch=4, col="red", cex=5)
  # text(reg_2_bo$x, col=reg_2_colors, label=1:reg_2_tot_obs, cex = 5)
  # dev.off()
  
  plot_title = sprintf(paste(test_func_string, " run %s", sep=""), run)
  png(filename = sprintf("/home/jsa378/bo_partition/plots/bo_runs/%s_numbered_points_run_%s.png", test_func_string, run),
      width = img_width,
      height = img_height)
  par(cex = 5)
  plot(bo$x, col=colors, pch=paste(1:tot_obs), type='n', main=plot_title)
  points(test_argmin[1], test_argmin[2], pch=4, col="red", cex=5)
  text(bo$x, col=colors, label=1:tot_obs)
  dev.off()
  
  # plot_title = sprintf(paste(test_func_string, " reg 1 run %s", sep=""), run)
  # png(filename = sprintf("/Users/jesse/Downloads/bo_partition/plots/bo_runs/%s_reg_1_numbered_points_run_%s.png", test_func_string, run),
  #     width = img_width,
  #     height = img_height)
  # par(cex = 5)
  # plot(reg_1_bo$x, col=reg_1_colors, pch=paste(1:reg_1_tot_obs), type='n', main=plot_title, xlim=plot_lims, ylim=plot_lims)
  # points(test_argmin[, 1], test_argmin[, 2], pch=4, col="red", cex=5)
  # text(reg_1_bo$x, col=reg_1_colors, label=1:reg_1_tot_obs)
  # dev.off()
  
  # plot_title = sprintf(paste(test_func_string, " reg 2 run %s", sep=""), run)
  # png(filename = sprintf("/Users/jesse/Downloads/bo_partition/plots/bo_runs/%s_reg_2_numbered_points_run_%s.png", test_func_string, run),
  #     width = img_width,
  #     height = img_height)
  # par(cex = 5)
  # plot(reg_2_bo$x, col=reg_2_colors, pch=paste(1:reg_2_tot_obs), type='n', main=plot_title, xlim=plot_lims, ylim=plot_lims)
  # points(test_argmin[1], test_argmin[2], pch=4, col="red", cex=5)
  # text(reg_2_bo$x, col=reg_2_colors, label=1:reg_2_tot_obs)
  # dev.off()
  
  run_obs[run, ] = bo$y[-(1:num_init_obs)]
  for(obs in 1:num_obs){
    best_so_far[run, obs] = min(bo$y[-(1:num_init_obs)][(1:obs)])
  }
  # reg_1_run_obs[run, ] = reg_1_bo$y[-(1:reg_1_num_init_obs)]
  # for(obs in 1:reg_obs){
  #   reg_1_best_so_far[run, obs] = min(reg_1_bo$y[-(1:reg_1_num_init_obs)][(1:obs)])
  # }
  # reg_2_run_obs[run, ] = reg_2_bo$y[-(1:reg_2_num_init_obs)]
  # for(obs in 1:reg_obs){
  #   reg_2_best_so_far[run, obs] = min(reg_2_bo$y[-(1:reg_2_num_init_obs)][(1:obs)])
  # }
  
  write.table(run_obs,
              file = sprintf("/home/jsa378/bo_partition/data/bo_runs/%s_obs.csv", test_func_string),
              row.names = FALSE,
              col.names = FALSE
  )
  write.table(best_so_far,
              file = sprintf("/home/jsa378/bo_partition/data/bo_runs/%s_best_so_far.csv", test_func_string),
              row.names = FALSE,
              col.names = FALSE
  )
}



# write.table(reg_1_run_obs,
#             file = sprintf("/Users/jesse/Downloads/bo_partition/data/bo_runs/%s_reg_1_obs.csv", test_func_string),
#             row.names = FALSE,
#             col.names = FALSE
# )
# write.table(reg_1_best_so_far,
#             file = sprintf("/Users/jesse/Downloads/bo_partition/data/bo_runs/%s_reg_1_best_so_far.csv", test_func_string),
#             row.names = FALSE,
#             col.names = FALSE
# )

# write.table(reg_2_run_obs,
#             file = sprintf("/Users/jesse/Downloads/bo_partition/data/bo_runs/%s_reg_2_obs.csv", test_func_string),
#             row.names = FALSE,
#             col.names = FALSE
# )
# write.table(reg_2_best_so_far,
#             file = sprintf("/Users/jesse/Downloads/bo_partition/data/bo_runs/%s_reg_2_best_so_far.csv", test_func_string),
#             row.names = FALSE,
#             col.names = FALSE
# )

end = Sys.time()
duration = end - start
print(duration)
