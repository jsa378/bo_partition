library(GaSP)
library(EGOmod)
library(lhs)
library(collapse)

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 4){
  stop("Four arguments must be supplied: test function (string), dim (int), num test pts (int) and save dir (no type)", call. = FALSE)
}

test_func_name = args[1]
dim = as.integer(args[2])
num_test_pts = as.integer(args[3])
save_dir = as.character(args[4])

# The choice of v6 below is arbitrary
# The point is just that I need to load gen_points()

source("/home/jsa378/bo_partition/code/test_funcs.R")
source("/home/jsa378/bo_partition/code/research/v6/bo_partition_helper_funcs.R")

test_func = test_func_list[[test_func_name]]$func
test_lbound = test_func_list[[test_func_name]]$lbound
test_ubound = test_func_list[[test_func_name]]$ubound

# gen_init_points.R is set up differently;
# it uses Initialize() from the EGO package
# to generate init points. Here, we don't
# need multiple runs and we just use randomLHS
# to generate our test points.

# In particular, we use gen_points()
# to generate the test points

# A wrinkle is that gen_points()
# is set up to take in a region
# as part of its input, so I need to
# make a dummy region to give to
# gen_points() when I call that function

# The dummy region doesn't need
# all the attributes that a real region
# would have, just whatever gen_points()
# uses

# Specifically, gen_points() only uses
# the $region_x and $bound_matric
# attributes of a region

dummy_x <- matrix(data = NA, ncol = dim, nrow = 1)
dummy_x[1, ] <- rep(0, dim)

dummy_region = list(bound_matrix = as.matrix(cbind(test_lbound, test_ubound)),
                   region_x = dummy_x
)

test_points <- gen_points(region = dummy_region, num_points = num_test_pts)
test_points_y <- apply(X = test_points, MARGIN = 1, FUN = test_func)

write.table(test_points,
            file = sprintf("%stest_points.csv", save_dir),
            row.names = FALSE,
            col.names = FALSE
)

write.table(test_points_y,
            file = sprintf("%stest_points_y.csv", save_dir),
            row.names = FALSE,
            col.names = FALSE
)
