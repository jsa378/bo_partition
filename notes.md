# Installation

## xmengju/EGO

To install the xmengju/EGO R package from [https://github.com/xmengju/EGO](https://github.com/xmengju/EGO), I had to do the following:

1. Enter `install.packages('devtools')` in the R Console
2. Log into my GitHub account
3. Go to [https://github.com/settings/tokens](https://github.com/settings/tokens)
4. Generate a token and copy it
5. Enter `devtools::install_github("xmengju/EGO", auth_token =  "token")` in the R Console, where I paste in the token I copied in place of `token`

# Algorithm

The rough idea is to try to adapt the ADAPT algorithm for use in Bayesian Optimization.

1. Generate points in $X = \mathcal R_1$, the domain of $f$
2. Evaluate $f$ at the points generated
3. Train GP on those chosen points and observations
4. Optimize the/an acquisition function on $\mathcal R_1$
5. Generate more new points within $\mathcal R_1$
6. Evaluate $f$ at the new points
7. Now we decide how to split $\mathcal R_1$:
    1. Split the first axis at the midpoint
    2. For each hypothetical sub-region, compute the average of the observed values of $f$
    3. Select the split that leads to the highest average of observed values of $f$ (of the 4 values you will get)
8. Now we have two regions, $\mathcal R_1$ and $\mathcal R_2$
9. Fit a new GP model for each of $\mathcal R_1$ and $\mathcal R_2$
9. Select either Method 1 or Method 2 (below) to decide which of $\mathcal R_1$ or $\mathcal R_2$ to split

## Method 1

1. Optimize an acquisition function in each of $\mathcal R_1$ and $\mathcal R_2$, resulting in proposed new sampling points $x_1^*$ and $x_2^*$
2. Evaluate $f$ at each of $x_1^*$ and $x_2^*$
3. Select the region that saw the highest $f$ value for splitting

I think with this method, if we have three regions $\mathcal R_1, \mathcal R_2$ and $\mathcal R_3$, then if we decided to split $\mathcal R_2$ into $\mathcal R_2$ and $\mathcal R_3$, when deciding which of the three regions to split, we don't have to optimize the acquisition function for $\mathcal R_1$ again, because nothing has changed in that region.

## Method 2

1. Optimize EI in each of $\mathcal R_1$ and $\mathcal R_2$, *using* $\boldsymbol f^*$, *the highest observed value in any region*
2. Choose for splitting the region that returns the highest of the EI values from the previous step

I think with this method, we have to re-optimize EI for every region, if in the last round, we observed a new highest observed max $\boldsymbol f^*$, right?