library(rgenoud)
library(laGP)
library(mvtnorm)
library(DiceOptim) # also loads DiceKriging

# Record start time (to print duration at end of code)

start <- Sys.time()

# Set various parameters

# # img_width <- 1 * 1e03
# # img_height <- 1 * 1e03
# test_func_name <- "rastr"
# dim <- 2
# # num_test_pts <- 10000
# num_runs <- 10
# num_init_obs <- 20
# num_fits <- 10
# covtype_param <- "PowerExponential"
# nugget_param <- 1e-09
# save_dir <- "/Users/jesse/Downloads/test/"

seed_value <- 1
test_func_name <- "rastr"
dim <- 2
num_init_obs <- 20 # 50
num_subseq_obs <- 100
num_runs <- 10
n_0 <- 10 # 10, 15, 19 for 20 init obs; 10, 20, 30, 40, 49 for 50 init obs (both for labo)
# n_max_param <- 25
# tol_param <- 0.1
# how_many_EI_points_param <- 1000
# top_n_EI_vals_param <- 10
# epsilon_param <- 0.01 # 0
save_dir <- "/Users/jesse/Downloads/cedar_test_output/research_testing/"
# slurm_job_id <- seed_value
covtype_param <- "gauss" # powexp"
nugget_param <- 1e-09
# min_consider_reject <- -1

# list_of_models <- vector(mode = "list", length = num_fits)

# Load test function (and region bounds) from test_funcs.R

source("/Users/jesse/Downloads/bo_partition/code/test_funcs.R")

test_func <- test_func_list[[test_func_name]]$func
test_lbound_scalar <- test_func_list[[test_func_name]]$lbound_scalar
test_ubound_scalar <- test_func_list[[test_func_name]]$ubound_scalar
test_lbound <- test_func_list[[test_func_name]]$lbound
test_ubound <- test_func_list[[test_func_name]]$ubound
test_argmin <- test_func_list[[test_func_name]]$argmin

optim_domain <- as.matrix(cbind(test_lbound, test_ubound))

init_points_loc <- sprintf("/Users/jesse/Downloads/bo_partition/code/implementation_testing/init_points/%s_%s_dim_%s_runs_%s_init_points/run_%s_init_points.csv",
                           test_func_name, dim, num_runs, num_init_obs, seed_value)

init_points <- read.table(
  file = init_points_loc,
  header = FALSE,
  sep = "",
  dec = "."
)
init_y <- apply(X = init_points, MARGIN = 1, FUN = test_func)

# Set up the grid of points
# at which to evaluate EI

x_beg <- test_lbound_scalar
x_end <- test_ubound_scalar
y_beg <- test_lbound_scalar
y_end <- test_ubound_scalar

reg_x <- seq(from = x_beg, to = x_end, length.out = 1000)
reg_y <- seq(from = y_beg, to = y_end, length.out = 1000)
grid_pts <- expand.grid(reg_x, reg_y)
colnames(grid_pts) <- c("V1", "V2")


# Define expected improvement function

# ei <- function(gpmodel, x) {
lagp_ei <- function(x, x_pts = init_points, y_pts = init_y, call = "genoud"){ # rewrite so x is the only argument?
  
  # print("The points at which to compute EI are:")
  # print(x)
  
  # if (ncol(x) != ncol(x_pts)) {
  #   x <- t(x)
  # }
  if (call == "genoud") {
    x <- t(x)
  }
  
  local_gp_model <- laGP(
    Xref = x,
    start = 6,
    end = n_0,
    X = x_pts,
    Z = y_pts,
    # d = NULL,
    # g = nugget_param,
    method = "mspe",
    Xi.ret = TRUE,
    # close = 0,
    close = nrow(x_pts),
    lite = TRUE,
    verb = 0 # 3
  )
  
  # f_star <- min(gpmodel$y)
  
  f_star <- min(y_pts)
  
  # print("f_star is:")
  # print(f_star)
  
  # predict_out <- Predict(GaSP_model = gpmodel,
  #                        x_pred = x,
  #                        generate_coefficients = FALSE)
  
  # print("The predictions are:")
  # print(predict_out)
  
  # mean_at_x <- predict_out$y_pred$Pred
  # sd_at_x <- predict_out$y_pred$SE
  
  mean_at_x <- local_gp_model$mean
  scale_at_x <- local_gp_model$s2
  df <- local_gp_model$df
  # sd_at_x <- scale_at_x * (df / (df - 2)) # need sqrt() here?
  sd_at_x <- sqrt(scale_at_x * (df / (df - 2)))
  
  f_star_minus_mu <- f_star - mean_at_x
  f_star_minus_mu_over_sigma <- f_star_minus_mu / sd_at_x
  
  ei_values <- f_star_minus_mu * pnorm(f_star_minus_mu_over_sigma) + sd_at_x * dnorm(f_star_minus_mu_over_sigma)
  
  # print("The EI values at the points are:")
  # print(ei_values)
  
  return(ei_values)
  
}

# Define function to plot EI contours

ei_contour <- function(ei_fn, optim_res) {
  
  if (identical(ei_fn, lagp_ei)) {
    fun_string <- "lagp_ei"
    num_pts <- n_0
    ei_vals <- ei_fn(x = grid_pts, call = "contour")
    file_string <- sprintf("/Users/jesse/Downloads/meetings/12_24oct24/lagp_ei_%s_%s_%s.png", seed_value, n_0, num_init_obs)
  } else {
    fun_string = "dice_ei"
    num_pts <- num_init_obs
    ei_vals <- ei_fn(x = grid_pts) # doesn't have a "call" argument
    file_string <- sprintf("/Users/jesse/Downloads/meetings/12_24oct24/dice_ei_%s_%s.png", seed_value, num_init_obs)
    # ei_vals <- apply(X = grid_pts, MARGIN = 1, FUN = ei_fn)
    # ei_vals <- with(grid_pts, ei_fun)
  }
  
  # ei_vals <- ei(x_pts = init_points, y_pts = init_y, x = grid_pts)
  # ei_vals <- ei_fn(x = grid_pts, call = "contour")
  
  ei_mat <- matrix(data = NA, nrow = 1000, ncol = 1000)
  
  # Need to do some matrix gymnastics
  # to conform to filled.contour()
  
  for (i in 1:1000) {
    ei_mat[, i] <- ei_vals[(((i - 1) * 1000) + 1): (i * 1000)]
  }
  
  point_symbols <- c(rep(1, nrow(init_points)), 19)
  points_to_plot <- rbind(init_points, optim_res$par)
  
  png(file_string, width = 1000, height = 1000, type = "cairo")
  filled.contour(x = reg_x,
                 y = reg_y,
                 z = ei_mat,
                 plot.axes={
                   points(x = points_to_plot, pch = point_symbols)
                   # points(x = init_points),
                   # points(x = gen_res$par, pch = 1)
                   },
                 plot.title = title(main = sprintf("%s, %s points out of %s, seed %s", fun_string, num_pts, num_init_obs, seed_value),
                                    xlab = "x1",
                                    ylab = "x2")
  )
  dev.off()
  # points(x = init_points)
  
}

# Do optimization for laGP/EI

# set.seed(1)
parinit <- test_lbound + runif(dim) * (test_ubound - test_lbound)

genoud_result <- genoud(fn = lagp_ei, nvars=dim, max=TRUE,
                        pop.size=1024, max.generations=100, wait.generations=10,
                        hard.generation.limit=TRUE, starting.values=parinit, MemoryMatrix=TRUE, 
                        Domains=optim_domain, default.domains=10, solution.tolerance=1e-21,
                        gr=NULL, boundary.enforcement=2, lexical=FALSE, gradient.check=FALSE, BFGS=TRUE,
                        data.type.int=FALSE, hessian=FALSE, unif.seed=floor(runif(1,max=10000)), int.seed=floor(runif(1,max=10000)), 
                        print.level=3,  
                        share.type=0, instance.number=0, output.path="stdout", output.append=FALSE, project.path=NULL,
                        P1=50, P2=50, P3=50, P4=50, P5=50, P6=50, P7=50, P8=50, P9=0, P9mix=NULL, 
                        BFGSburnin=2, BFGSfn=NULL, BFGShelp=NULL, control=list("maxit"=96), 
                        cluster=FALSE, balance=FALSE, debug=FALSE
)

# ei_contour()

ei_contour(ei_fn = lagp_ei, optim_res = genoud_result)

#####

##### REGULAR DICE STUFF BELOW #####

#####

dice_ctrl <- list(
  pop.size = 1024,
  max.generations = 100,
  wait.generations = 10,
  BFGSburnin = 2
)

km_gp_model <- km(
  formula = ~1,
  design = init_points,
  response = init_y,
  covtype = covtype_param,
  nugget = nugget_param,
  control = c(dice_ctrl, trace = FALSE),
  optim.method = "gen"
)

# dice_ei_fn <- \(x) EI(
#   x,
#   model = km_gp_model,
#   type = "UK"
# )

dice_ei_fn <- function(x, gpmodel) {
  
  # print("The points at which to compute EI are:")
  # print(x)
  
  f_star <- min(gpmodel@y) # min(gpmodel$y)
  
  # print("f_star is:")
  # print(f_star)
  
  predict_out <- predict(object = gpmodel, newdata = x, type = "UK") # Predict(GaSP_model = gpmodel,
                  #        x_pred = x,
                  #        generate_coefficients = FALSE)
  
  # print("The predictions are:")
  # print(predict_out)
  
  mean_at_x <- predict_out$mean # predict_out$y_pred$Pred
  sd_at_x <- predict_out$sd # predict_out$y_pred$SE
  
  f_star_minus_mu <- f_star - mean_at_x
  f_star_minus_mu_over_sigma <- f_star_minus_mu / sd_at_x
  
  ei_values <- f_star_minus_mu * pnorm(f_star_minus_mu_over_sigma) + sd_at_x * dnorm(f_star_minus_mu_over_sigma)
  
  # print("The EI values at the points are:")
  # print(ei_values)
  
  return(ei_values)
  
}

max_ei_result <- max_EI(
  model = km_gp_model,
  type = "UK",
  lower = test_lbound,
  upper = test_ubound,
  control = dice_ctrl
)

# ei_contour(ei_fn = dice_ei_fn, optim_res = max_ei_result)
ei_contour(ei_fn = \(x) dice_ei_fn(x, gpmodel = km_gp_model), optim_res = max_ei_result)

# test_1 <- matrix(c(-4.12, 2.93), nrow = 1)
# test_2 <- matrix(c(-1, -1), nrow = 1)
# test_3 <- matrix(c(-0.5, -1), nrow = 1)

# ei(test_1, call = "genoud")
# ei(test_1, call = "contour")
# ei(test_2)
# ei(test_3)
# 
# ei(c(-4.116, 2.929))
# ei(c(-4.12, 2.93))



# sink("/Users/jesse/Downloads/labo_test.txt")
# genoud(
#   # fn = \(x) ei(x, x_pts = init_points, y_pts = init_y),
#   fn = ei,
#   nvars = dim,
#   max = TRUE,
#   pop.size = 1024,
#   max.generations = 20,
#   wait.generations = 10,
#   hard.generation.limit = TRUE,
#   starting.values = parinit,
#   MemoryMatrix = TRUE,
#   Domains = optim_domain,
#   default.domains = 10,
#   solution.tolerance = 1e-21,
#   gr = NULL,
#   boundary.enforcement = 2,
#   lexical = FALSE,
#   gradient.check = FALSE,
#   BFGS = TRUE,
#   BFGSburnin = 5
# )

# ,
#             model=model, plugin=plugin, type=type,minimization = minimization, envir=EI.envir
# )

# sink(file = NULL)

# points(x = genoud_result$par, pch = 1)
