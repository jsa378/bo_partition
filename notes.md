# Installation

## xmengju/EGO

To install the xmengju/EGO R package from [https://github.com/xmengju/EGO](https://github.com/xmengju/EGO), I had to do the following:

1. Enter `install.packages('devtools')` in the R Console
2. Log into my GitHub account
3. Go to [https://github.com/settings/tokens](https://github.com/settings/tokens)
4. Generate a token and copy it
5. Enter `devtools::install_github("xmengju/EGO", auth_token =  "token")` in the R Console, where I paste in the token I copied in place of `token`

# Algorithm (rough draft)

The rough idea is to try to adapt the ADAPT algorithm for use in Bayesian Optimization.

1. Generate points in $X$, the domain of $f$
2. Evaluate $f$ at the points generated
3. Train GP on those chosen points and observations
4. Optimize the/an acquisition function on $X$
5. Generate more new points within $X$
6. Evaluate $f$ at the new points
7. Now we decide how to split $X$:
    1. Split the first axis at the midpoint
    2. For each hypothetical sub-region, compute the average of the observed values of $f$
    3. Select the split that leads to the *lowest* average of observed values of $f$ (of the 4 values you will get)
8. Now we have two regions, $\mathcal R_1$ and $\mathcal R_2$
9. Fit a new GP model for each of $\mathcal R_1$ and $\mathcal R_2$
9. Select either Method 1 or Method 2 (below) to decide which of $\mathcal R_1$ or $\mathcal R_2$ to split

### Method 1

1. Optimize an acquisition function in each of $\mathcal R_1$ and $\mathcal R_2$, resulting in proposed new sampling points $x_1^*$ and $x_2^*$
2. Evaluate $f$ at each of $x_1^*$ and $x_2^*$
3. Select for splitting the region that saw the lowest $f$ value

I think with this method, if we have three regions $\mathcal R_1, \mathcal R_2$ and $\mathcal R_3$, then if we decided to split $\mathcal R_2$ into $\mathcal R_2$ and $\mathcal R_3$, when deciding which of the three regions to split, we don't have to optimize the acquisition function for $\mathcal R_1$ again, because nothing has changed in that region.

Note: For this method to be honest, I would probably need to count the evaluations $f(x_1^*)$ and $f(x_2^*)$ towards the total budget of $N$ observations.

### Method 2

1. Optimize EI in each of $\mathcal R_1$ and $\mathcal R_2$, *using* $\boldsymbol f^*$, the *lowest* observed value in any region
2. Choose for splitting the region that returns the *highest* of the EI values from the previous step

I think with this method, we have to re-optimize EI for every region, if in the last round, we observed a new lowest observed min $\boldsymbol f^*$, right?

## Algorithm (more polished)

1. Generate points in $X$, the domain of $f$
2. Evaluate $f$ at the points generated
3. Define region $\mathcal R_1 \coloneqq X$ (make a list of regions and put $\mathcal R_1$ in it)
4. Fit GP for $\mathcal R_1$ (using `km`)
5. Apply either the Method 1 or Method 2 criterion to $\mathcal R_1$ (see below) (this is the mechanism for determining which region to split)
6. while $n < N$:
    - for all regions in the list of regions, find the region with the smallest Method 1 criterion, or the largest Method 2 criterion; this is the region we will split
        - maybe i should put this in a function called `which_region_to_split`, which takes in a list of regions and returns the region to split
    - send the region to split to a function `split_and_fit`, which:
        - takes in a region to split
        - generates new points in that region
        - evaluates $f$ at those new points
        - adds both sets of points to the lists of all chosen points and observed values
        - splits the region:
            - split each axis at the midpoint
            - for each pair of sub-regions, record the average of the observed values of $f$ in the sub-regions
            - choose the split that leads to the lowest average of observed values of $f$
        - fit a new GP for each new sub-region
    - remove the region chosen for splitting from the list of regions, and add the two new sub-regions to the list of regions
    - update the total number of observations of $f$
7. finally, return $f^*$, the smallest observed value of $f$

### Method 1 criterion

- for all regions in the list of regions $[\mathcal R_1, \dots, \mathcal R_K]$:
    - maximize EI in each region, resulting in proposed new sampling points $x_i^*$ within each region
    - evaluate $f$ at each of $x_1^*, \dots, x_K^*$
    - add $x_1^*, \dots, x_K^*$ and $f(x_1^*), \dots, f(x_K^*)$ to the lists of all chosen points and observations, respectively
    - choose the region to split with index $\argmin \left\{ f(x_1^*), \dots, f(x_K^*) \right\}$, *i.e.*, smallest $f$ value

Note: for this method, each time through the loop, I think we can either:

1. re-fit GPs for each region and re-optimize EI for each region (because we chose a point and evaluated $f$ at that point in each region, the last time through the loop), OR
2. we can re-fit GPs and optimize EI only for the two sub-regions of the region we chose to split, and leave the rest unchanged. Right?

The second method should be more efficient.

### Method 2 criterion

- identify the smallest observed value of $f$ so far (irrespective of region); call this value $f^*$
- for all regions in the list of regions $[\mathcal R_1, \dots, \mathcal R_K]$:
    - maximize EI in each region, *using $f^*$ as the plugin value for all optimizations*, *i.e.* compute EI in each region with respect to $f^*$
    - choose the region to split with the highest returned EI value

Note: for this method, each time through the loop, I think there are two cases:

1. We have found a new, smallest $f^*$ within the region we chose to split, in which case maybe we need to re-maximize EI in each region with respect to the new, smaller $f^*$
2. The old $f^*$ is still the smallest so far, in which case we just need to re-maximize EI for the two sub-regions of the region that we chose to split.

I think that is right.

### Note regarding Method 1 and 2

Should Method 1 or two actually do a full round of BO? That is, you do whatever EI maximization you were going to do, then observe at the propsed point in each region, then re-fit the model for each region, and then do another EI maximization, and then return the EI values, so you can decide which region to split?

Perhaps this needs to be investigated as well. I guess it might be a tradeoff---the above version is more computationally demanding and used up your budget of $N$ evaluations of $f$ faster. Maybe I should test two versions of Method 1 and 2 each, and regular BO, all with some equal number $N$ of observations, and compare results?