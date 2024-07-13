# Note to myself about the files in this folder:

These are more or less the initial set of files I made for researching my first doctoral project with Professor Welch. Earlier sets of files were focused on testing standard Bayesian optimization using different test functions, libraries and specific parameters (*e.g.*, number of observations). This set of files is concerned with attempting to develop a partition-Bayesian optimization method.

On 12 June, 2024, I moved these files into the folder `v1`, planning to leave them here and make a new set of files to work on. These files in `v1` became too complicated and unwieldy as I tried to support different software libraries (`Dice` and `EGO`) as well as multiple criteria for splitting a region. The code was also repetitive in certain ways and would have benefitted from refactoring, *e.g.* writing functions to reduce repetitive code.

I am making a new set of files that will take what seems useful from these `v1` files. I will modfify certain parts based on the above considerations, and discussions on research direction with Professor Welch today (12 June).