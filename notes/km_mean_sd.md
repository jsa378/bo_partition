# Making mean and sd plots from `km()` objects

This is only feasible for test functions with two-dimensional domains.

1. In RStudio, run the code that produces the desired region.
2. Bind the region of interest, like say `reg <- rejected_regions[[3]]`.
3. Bind the start- and end-points for the $x$ and $y$ dimensions, like

```
x_beg <- reg$bound_matrix[1, 1]
x_end <- reg$bound_matrix[1, 2]
y_beg <- reg$bound_matrix[2, 1]
y_end <- reg$bound_matrix[2, 2]
```

4. Create the grid of points at which we will evaluate the mean and sd:

```
reg_x <- seq(from = x_beg, to = x_end, length.out = 1000)
reg_y <- seq(from = y_beg, to = y_end, length.out = 1000)
grid_pts <- expand.grid(reg_x, reg_y)
```

5. Fit the `km()` model for the region: `reg_model <- km(formula = ~1, design = reg$region_x, response = reg$region_y, covtype = "powexp", nugget = 1e-09, control = c(dice_ctrl, trace = FALSE), optim.method = "gen")`.
6. Make sure the column names of `grid_pts` match the column names of `reg$region_x`: `colnames(grid_pts) <- c("V1", "V2")`.
7. Make the mean and sd predictions: `preds <- predict(object = reg_model, newdata = grid_pts, type = "UK", se.compute = TRUE)`.
8. The mean and sd predictions in are in vectors in `preds`, and they are a million elements long. We need to convert them to matrices such that (for example), entry $a_{ij}$ in the mean matrix `mean_mat` is the mean at the point corresponding to the $i$th element of `reg_x` and the $j$th element of `reg_y`. To do this, define the two matrices and then put each thousand elements of the `preds` vectors as columns of the respective matrices:

```
mean_mat <- matrix(data = NA, nrow = 1000, ncol = 1000)
sd_mat <- matrix(data = NA, nrow = 1000, ncol = 1000)
for (i in 1:1000) {
    mean_mat[, i] <- preds$mean[(((i - 1) * 1000) + 1): (i * 1000)]
    sd_mat[, i] <- preds$sd[(((i - 1) * 1000) + 1): (i * 1000)]
}
```

9. Now make the contour plots:

```
filled.contour(x = reg_x, y = reg_y, z = mean_mat, plot.title = title(main = "km() mean", xlab = "x1", ylab = "x2"))
filled.contour(x = reg_x, y = reg_y, z = sd_mat, plot.title = title(main = "km() sd", xlab = "x1", ylab = "x2"))
```

and save the pictures. (Before saving, make RStudio full screen and make sure the plots look reasonable in the "Plots" pane. If the RStudio window is small and the Plots pane is really squished, I think the plots will also come out squished.) I found that a horizontal size of 1500 pixels, with a corresponding vertical size to maintain the aspect ratio, looked reasoanable.