# library(DiceOptim)
library(GaSP)

start <- Sys.time()

working <- "remote"

if (working == "remote") {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 11) {
    stop("Eleven arguments must be supplied:
    seed value (int),
    test function (string),
    dim (int),
    num init obs (int),
    num_test_pts (int),
    num runs (int),
    num_fits (int),
    save dir (no type),
    slurm job id (int),
    covtype (string),
    nugget (float)", call. = FALSE)
  }
  seed_value <- as.integer(args[1])
  test_func_name <- args[2]
  dim <- as.integer(args[3])
  num_init_obs <- as.integer(args[4])
  num_test_pts <- as.integer(args[5])
  num_runs <- as.integer(args[6])
  num_fits <- as.integer(args[7])
  save_dir <- as.character(args[8])
  slurm_job_id <- as.integer(args[9])
  covtype_param <- args[10]
  nugget_param <- as.numeric(args[11])
  
  source("/home/jsa378/bo_partition/code/test_funcs.R")
  
}

img_width <- 2 * 1e03
img_height <- 2 * 1e03

# A function (and some settings) to dump an .rda file
# in the event of an R error (for debugging purposes)

dump_and_quit <- function() {
  # Save debugging info to file last.dump.rda
  dump.frames(dumpto = sprintf("last.dump%s", slurm_job_id), to.file = TRUE)
  # Quit R with error status
  q(status = 1)
}
options(error = dump_and_quit, CBoundsCheck = TRUE)

# Set the test function and associated information

test_func <- test_func_list[[test_func_name]]$func
test_lbound_scalar <- test_func_list[[test_func_name]]$lbound_scalar
test_ubound_scalar <- test_func_list[[test_func_name]]$ubound_scalar
test_lbound <- test_func_list[[test_func_name]]$lbound
test_ubound <- test_func_list[[test_func_name]]$ubound
test_argmin <- test_func_list[[test_func_name]]$argmin

# Print some summary information concerning the job
# at the beginning of the run

paste(c("Slurm job ID:", slurm_job_id), collapse = " ")
print(sprintf("Job beginning at: %s", Sys.time()))
paste(c("Test function:", test_func_name), collapse = " ")
paste(c("Number of dimensions:", dim), collapse = " ")
paste(c("Number of initial observations:", num_init_obs), collapse = " ")
paste(c("Number of test points:", num_test_pts), collapse = " ")
paste(c("Number of runs:", num_runs), collapse = " ")
paste(c("Save directory:", save_dir), collapse = " ")
paste(c("Kernel type:", covtype_param), collapse = " ")
paste(c("Nugget value", nugget_param), collapse = " ")
paste(c("Test func. lower bound scalar:", test_lbound_scalar), collapse = " ")
paste(c("Test func. upper bound scalar:", test_ubound_scalar), collapse = " ")
paste(c("Test func. lower bound vector:", test_lbound), collapse = " ")
paste(c("Test func. upper bound vector:", test_ubound), collapse = " ")
paste(c("Test func. argmin:", test_argmin), collapse = " ")

set.seed(seed_value)

# range_params <- matrix(data = NA, nrow = num_runs, ncol = dim)
# theta_params <- matrix(data = NA, nrow = num_runs, ncol = dim)

# dice_ctrl <- list(
#   pop.size = 1024,
#   max.generations = 100,
#   wait.generations = 10,
#   BFGSburnin = 5
# )

test_points_loc <- sprintf("/home/jsa378/bo_partition/code/implementation_testing/test_points/%s_%s_dim_%s_test_points/test_points.csv",
                             test_func_name, dim, num_test_pts)
test_points_y_loc <- sprintf("/home/jsa378/bo_partition/code/implementation_testing/test_points/%s_%s_dim_%s_test_points/test_points_y.csv",
                             test_func_name, dim, num_test_pts)

test_points <- read.table(
  file = test_points_loc,
  header = FALSE,
  sep = "",
  dec = "."
)

test_y <- read.table(
  file = test_points_y_loc,
  header = FALSE,
  sep = "",
  dec = "."
)

for (run in 1:num_runs) {
  
  print(sprintf("Beginning run %s of %s", run, num_runs))
  
  init_points_loc <- sprintf("/home/jsa378/bo_partition/code/implementation_testing/init_points/%s_%s_dim_%s_runs_%s_init_points/run_%s_init_points.csv",
                             test_func_name, dim, num_runs, num_init_obs, run)
  
  init_points <- read.table(
    file = init_points_loc,
    header = FALSE,
    sep = "",
    dec = "."
  )
  init_y <- apply(X = init_points, MARGIN = 1, FUN = test_func)
  
  print("The init points are:")
  print(init_points)

  run_fit_mat <- matrix(data = NA, nrow = num_fits, ncol = dim + 3)
  colnames(run_fit_mat) <- c(rep("cor_par", dim), "obj", "cvrmse", "rmse")

  # gp_model <- km(
  #   formula = ~1,
  #   design = init_points,
  #   response = init_y,
  #   covtype = covtype_param,
  #   nugget = nugget_param,
  #   control = c(dice_ctrl, trace = FALSE),
  #   optim.method = "gen"
  # )

  for (fit in 1:num_fits) {

    print(sprintf("Fitting gp_model for fit: %s", fit))

    gp_model <- Fit(
      x = init_points,
      y = init_y,
      reg_model = ~1,
      cor_family = covtype_param,
      random_error = FALSE,
      nugget = nugget_param,
      tries = 1,
      seed = fit,
      fit_objective = "Likelihood",
      alpha_max = 0,
      model_comparison = "Objective"
    )
    
    cor_par <- gp_model$cor_par
    obj <- gp_model$objective
    cvrmse <- gp_model$CVRMSE
    
    print("The estimated correlation parameters are:")
    print(cor_par)
    
    print("The log-likelihood value is:")
    print(obj)
    
    print("The CVRMSE is:")
    print(cvrmse)

    # Next, make predictions using gp_model
    # on the test set

    gp_model_preds <- Predict(gp_model, test_points)
    rmse <- RMSE(gp_model_preds, test_points_y)

    print("The RMSE on the test set is:")
    print(rmse)

    run_fit_mat[fit, ] <- c(cor_par, obj, cvrmse, rmse)


    plot_title = sprintf(paste("qq plot: ", test_func_string, " dim %s", " run %s", sep=""), dim, run)
    png(filename = sprintf("%srun_%s_qq.png", save_dir, run),
        width = img_width,
        height = img_height)
    PlotQQ(gp_model_preds, test_points_y) # Not sure how to save this plot on Cedar
    dev.off()

    plot_title = sprintf(paste("std resid: ", test_func_string, " dim %s", " run %s", sep=""), dim, run)
    png(filename = sprintf("%srun_%s_std_resid.png", save_dir, run),
        width = img_width,
        height = img_height)
    PlotStdResiduals(gp_model_preds, test_points_y, title = "Predict") # This one too
    dev.off()

    write.table(run_fit_mat,
    file = sprintf("%srun_%s_fit_mat.csv", save_dir, run),
    row.names = FALSE,
    col.names = TRUE
    )

  }


  
  print("The range parameters for this set of init points is:")
  # print(gp_model@covariance@range.val)
  print(gp_model$cor_par$Theta)
  # range_params[run, ] <- gp_model@covariance@range.val
  theta_params[run, ] <- gp_model$cor_par$Theta
  
  print("Saving partial progress.")
  
  write.table(theta_params[1:run, ], # range_params[1:run, ],
              file = sprintf("%s%s_init_obs.csv", save_dir, num_init_obs),
              row.names = FALSE,
              col.names = FALSE
  )

  # This break statement is just here
  # to test my code works for a single
  # set of init points
  # (Comment out when code is working)
  break
  
}

print("The range parameter values (unscaled) are:")
# print(range_params)
print(theta_params)

end <- Sys.time()
duration <- end - start

print("Complete in:")
print(duration)