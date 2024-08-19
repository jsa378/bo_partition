# library(DiceOptim)
library(GaSP)

# Record start time (to print duration at end of code)

start <- Sys.time()

working <- "remote"

if (working == "remote") {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 12) {
    stop("Twelve arguments must be supplied:
    img_width (int),
    img_height (int),
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
  img_width <- as.integer(args[1])
  img_height <- as.integer(args[2])
  test_func_name <- args[3]
  dim <- as.integer(args[4])
  num_init_obs <- as.integer(args[5])
  num_test_pts <- as.integer(args[6])
  num_runs <- as.integer(args[7])
  num_fits <- as.integer(args[8])
  save_dir <- as.character(args[9])
  slurm_job_id <- as.integer(args[10])
  covtype_param <- args[11]
  nugget_param <- as.numeric(args[12])

  # Load test function (and region bounds) from test_funcs.R
  
  source("/home/jsa378/bo_partition/code/test_funcs.R")
  
}

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
paste(c("Plot size width and height:", img_width, img_height), collapse = " ")
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

# dice_ctrl <- list(
#   pop.size = 1024,
#   max.generations = 100,
#   wait.generations = 10,
#   BFGSburnin = 5
# )

# Load test points

test_points_loc <- sprintf("/home/jsa378/bo_partition/code/implementation_testing/test_points/%s_%s_dim_%s_test_points/test_points.csv",
                             test_func_name, dim, num_test_pts)

test_points <- read.table(
  file = test_points_loc,
  header = FALSE,
  sep = "",
  dec = "."
)
test_y <- apply(X = test_points, MARGIN = 1, FUN = test_func)


for (run in 1:num_runs) {
  
  print(sprintf("Beginning run %s of %s", run, num_runs))


    # Load initial points
  
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


  # Set up matrix to record thetas, log-likelihood value,
  # CVRMSE, and RMSE

  run_fit_mat <- matrix(data = NA, nrow = num_fits, ncol = dim + 3)


  # Here we fit the Gaussian process model multiple times
  # (using the same set of initial points)

  for (fit in 1:num_fits) {

    print(sprintf("Fitting gp_model for fit: %s", fit))

    # Note that I set a different seed each time in Fit()

    gp_model <- Fit(
      x = init_points,
      y = init_y,
      reg_model = ~1,
      cor_family = covtype_param,
      random_error = FALSE,
      nugget = nugget_param,
      tries = 1,
      seed = as.numeric(fit),
      fit_objective = "Likelihood",
      alpha_max = 0,
      model_comparison = "Objective"
    )
    
    # Select relevant information from the Fit() object

    cor_par <- gp_model$cor_par$Theta
    obj <- gp_model$objective
    cvrmse <- gp_model$CVRMSE
    
    # print("The estimated correlation parameters are:")
    # print(cor_par)
    
    # print("The log-likelihood value is:")
    # print(obj)
    
    # print("The CVRMSE is:")
    # print(cvrmse)

    # Next, make predictions using gp_model
    # on the test set

    predict <- Predict(GaSP_model = gp_model,
                        x_pred = test_points,
                        generate_coefficients = FALSE)

    # Select the relevant output from Predict()

    pred_values <- predict$y_pred$Pred
    plot_pred <- predict$y_pred
    
    # Calculate RMSE

    rmse <- RMSE(y_pred = pred_values,
                 y_true = test_y)

    # print("The RMSE on the test set is:")
    # print(rmse)

    # Put the gathered information in a row of run_fit_mat
    # and then save run_fit_mat

    run_fit_mat[fit, ] <- c(cor_par, obj, cvrmse, rmse)

    write.table(run_fit_mat,
            file = sprintf("%srun_%s_fit_mat.csv", save_dir, run),
            row.names = FALSE,
            col.names = FALSE
    )


    # Print out which run and fit the below information
    # corresponds to
    
    print(sprintf("Showing results for run %s, fit %s", run, fit))
    
    # Print out the row of run_fit_mat
    
    row_print <- matrix(data = NA, nrow = 1, ncol = ncol(run_fit_mat))
    row_print[1, ] <- run_fit_mat[fit, ]
    colnames(row_print) <- c(rep("Theta", dim), "Obj.", "CVRMSE", "RMSE")
    print("The numerical results for this fit are as follows:")
    print(row_print)

    # Next, we make the plots

    png(filename = sprintf("%srun_%s_fit_%s_qq.png", save_dir, run, fit),
        width = img_width,
        height = img_height)
    par(cex = 2)
    PlotQQ(y_pred = plot_pred,
           y = test_y)
    dev.off()

    png(filename = sprintf("%srun_%s_fit_%s_std_resid.png", save_dir, run, fit),
        width = img_width,
        height = img_height)
    par(cex = 2)
    PlotStdResiduals(y_pred = plot_pred,
                     y = test_y,
                     title = "Predict")
    dev.off()

  }

  # Print out the entire matrix of numerical results
  # (with column names)
  
  colnames(run_fit_mat) <- c(rep("Theta", dim), "Obj.", "CVRMSE", "RMSE")  
  print("The run_fit_mat matrix is:")
  print(run_fit_mat)

  if (run == 10) {

    break

  }
  
}

end <- Sys.time()
duration <- end - start

print("Complete in:")
print(duration)
