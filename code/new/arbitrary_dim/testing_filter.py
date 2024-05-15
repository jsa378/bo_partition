# I only created this file because I was driving myself nuts with R syntax
# I think I've straightened things out now

import numpy as np

first_interval = np.array([0, 1])
second_interval = np.array([0, 0.5])

test_region = np.vstack((first_interval, second_interval))
print(test_region)
rng = np.random.default_rng(seed=1)

all_x = rng.random(size=(10, 2))
print(all_x)
print(all_x.shape)

def filter(region, x, y):
    in_region = np.ones(x.shape[1])
    for row in range(x.shape[0]):
        for col in range(region.shape[1]):
            if x[row, col] < region[col, 0] or x[row, col] > region[col, 1]:
                in_region[row] = 0
    return in_region

print(filter(test_region, all_x, all_x))