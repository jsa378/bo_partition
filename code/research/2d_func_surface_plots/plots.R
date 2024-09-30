library(rgl)

# try to follow the instrutions here
# to produce an html file with the
# surfaces created below

test_func_name <- "rastr"
dim <- 2

test_point <- c(0.5, -2.2)

rastr_mod_1 <- function(xx) # 10 x_i cos(2 pi x_i)
{
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  d <- length(xx)
  
  sum <- sum(xx^2 - 10*xx*cos(2*pi*xx))
  
  y <- 10*d + sum
  return(y)
}

rastr_mod_1(test_point) # approx. 36.88837

rastr_mod_1_check <- function(xx)
{
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  # d <- length(xx)
  
  # sum <- sum(xx^2 - 10*xx*cos(2*pi*xx))
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  sum <- x1^2 + x2^2 - 10 * x1 * cos(2*pi*x1) - 10 * x2 * cos(2*pi*x2)
  
  y <- 10*dim + sum
  return(y)
}

rastr_mod_1_check(test_point)

rastr_mod_2 <- function(xx) # 10 | x_i | cos(2 pi x_i)
{
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  d <- length(xx)
  
  sum <- sum(xx^2 - 10*abs(xx)*cos(2*pi*xx))
  
  y <- 10*d + sum
  return(y)
}

rastr_mod_2(test_point) # approx. 23.29163

rastr_mod_3 <- function(xx) # sum dotted with alpha = (0.1, 0.9)
{
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  d <- length(xx)
  alpha <- c(0.1, 0.9)
  
  # sum <- sum(alpha * (xx^2 - 10*cos(2*pi*xx)))
  sum <- sum(alpha * (xx^2 - 10*cos(1*pi*xx)))
  
  y <- 10*d + sum
  return(y)
}

rastr_mod_3(test_point) # approx. 22.59985

source("/Users/jesse/Downloads/bo_partition/code/test_funcs.R")

test_func <- test_func_list[[test_func_name]]$func
test_lbound_scalar <- test_func_list[[test_func_name]]$lbound_scalar
test_ubound_scalar <- test_func_list[[test_func_name]]$ubound_scalar
test_lbound <- test_func_list[[test_func_name]]$lbound
test_ubound <- test_func_list[[test_func_name]]$ubound
test_argmin <- test_func_list[[test_func_name]]$argmin

# Set up the grid of points
# at which to evaluate EI

x_beg <- test_lbound_scalar
x_end <- test_ubound_scalar
y_beg <- test_lbound_scalar
y_end <- test_ubound_scalar

reg_x <- seq(from = x_beg, to = x_end, length.out = 1000)
reg_y <- seq(from = y_beg, to = y_end, length.out = 1000)
grid_pts <- expand.grid(reg_x, reg_y)
# colnames(grid_pts) <- c("V1", "V2")

# rastr_twoarg <- function(x1, x2) {
#   vec <- c(x1, x2)
#   return(rastr(vec))
# }

test_func_vals <- apply(X = grid_pts, MARGIN = 1, FUN = test_func)
# test_func_vals <- outer(
#   X = reg_x,
#   Y = reg_y,
#   FUN = rastr_twoarg # test_func
# )

test_func_mat <- matrix(data = NA, nrow = 1000, ncol = 1000)
for (i in 1:1000) {
  test_func_mat[, i] <- test_func_vals[(((i - 1) * 1000) + 1): (i * 1000)]
}

# jet.colors <- colorRampPalette(c("blue", "green"))
jet.colors <- colorRampPalette(c("red", "yellow", "blue"))
pal <- jet.colors(100)
col.ind <- cut(test_func_mat, 100)


persp3d(
  x = reg_x,
  y = reg_y,
  z = test_func_mat,
  col = pal[col.ind]
)

# persp(
#   x = reg_x,
#   y = reg_y,
#   z = test_func_mat
# )

test_func <- rastr_mod_1
test_func_vals <- apply(X = grid_pts, MARGIN = 1, FUN = test_func)

test_func_mat <- matrix(data = NA, nrow = 1000, ncol = 1000)
for (i in 1:1000) {
  test_func_mat[, i] <- test_func_vals[(((i - 1) * 1000) + 1): (i * 1000)]
}

jet.colors <- colorRampPalette(c("red", "yellow", "blue"))
pal <- jet.colors(100)
col.ind <- cut(test_func_mat, 100)

persp3d(
  x = reg_x,
  y = reg_y,
  z = test_func_mat,
  col = pal[col.ind]
)

check <- apply(grid_pts, 1, rastr_mod_1_check)
all.equal(check, test_func_vals)
# check - test_func_vals

test_func <- rastr_mod_2
test_func_vals <- apply(X = grid_pts, MARGIN = 1, FUN = test_func)

test_func_mat <- matrix(data = NA, nrow = 1000, ncol = 1000)
for (i in 1:1000) {
  test_func_mat[, i] <- test_func_vals[(((i - 1) * 1000) + 1): (i * 1000)]
}

jet.colors <- colorRampPalette(c("red", "yellow", "blue"))
pal <- jet.colors(100)
col.ind <- cut(test_func_mat, 100)

persp3d(
  x = reg_x,
  y = reg_y,
  z = test_func_mat,
  col = pal[col.ind]
)

test_func <- rastr_mod_3
test_func_vals <- apply(X = grid_pts, MARGIN = 1, FUN = test_func)

test_func_mat <- matrix(data = NA, nrow = 1000, ncol = 1000)
for (i in 1:1000) {
  test_func_mat[, i] <- test_func_vals[(((i - 1) * 1000) + 1): (i * 1000)]
}

jet.colors <- colorRampPalette(c("red", "yellow", "blue"))
pal <- jet.colors(100)
col.ind <- cut(test_func_mat, 100)

persp3d(
  x = reg_x,
  y = reg_y,
  z = test_func_mat,
  col = pal[col.ind]
)

###

# tryCatch(
#   expr = {
#     file <- read.csv("/Users/jesse/Downloads/TA/2024 03 STAT 404/2024-09-11T1014_Grades-STAT_V_404_101_2024W.csv")
#     print(file[3, 1])
#   },
#   error = function(e) {
#     print("Error: couldn't read .csv file!")
#     print(e)
#   }
# )
# 
# file <- read.csv("/Users/jesse/Downloads/TA/2024 03 STAT 404/2024-09-11T1014_Grades-STAT_V_404_101_2024W.csv")
# print(file[3, 1])
# 
# print(2 + 2)

# library(tidyverse)
# library(datateachr)
# 
# test <- vancouver_trees |>
#   filter(
#     diameter > 0,
#     !is.na(latitude),
#     !is.na(longitude)
#   ) |>
#   mutate(
#     EastCentralWest = factor(case_when(longitude > -123.0905 ~ "East",
#                                        longitude > -123.1555 ~ "Central",
#                                        TRUE ~ "West"),
#                              levels = c("East", "Central", "West"))
#   )
# 
# test |>
#   group_by(EastCentralWest) |>
#   summarize(n = n())
# 
# test |>
#   group_by(EastCentralWest) |>
#   summarize(min_long = min(longitude),
#             max_long = max(longitude)
#             ) |>
#   as.data.frame()
