import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
plt.figure(figsize=(8, 6), dpi=224)
import sys

seed_value = int(sys.argv[1])
test_func_name = sys.argv[2]
dim = int(sys.argv[3])
num_init_obs = int(sys.argv[4])
num_obs = int(sys.argv[5])
num_runs = int(sys.argv[6])
save_dir = str(sys.argv[7])
num_array_jobs = int(sys.argv[8])

test_func_dicts = {
    'rastr': {
        'global_min': 0
    }
}

step_size_dict = {
    10: 1,
    100: 10,
    200: 20,
    400: 50
}

all_obs = np.zeros((num_array_jobs, num_obs))
all_best = np.zeros((num_array_jobs, num_obs))

for seed in range(1, num_array_jobs + 1):
   all_obs[seed - 1, ] = pd.read_csv(f'{save_dir}seed_{seed}_obs.csv', header=None, dtype=float, sep='\s+').to_numpy()
   all_best[seed - 1, ] = pd.read_csv(f'{save_dir}seed_{seed}_best_so_far.csv', header=None, dtype=float, sep='\s+').to_numpy()

global_min = test_func_dicts[test_func_name]['global_min']
x_axis_step_size = step_size_dict[num_obs]
x_axis = np.linspace(1, num_obs, num_obs)
xtick_array = np.arange(0, num_obs + x_axis_step_size, step=x_axis_step_size)
fig, (ax1, ax2) = plt.subplots(1, 2)
fig.suptitle(f'{test_func_name}, {num_init_obs} init obs, {num_obs} obs, {dim} dim, {num_array_jobs} runs')
ax1.set_title(f'raw obs')
ax2.set_title(f'best obs so far')

colors = iter(plt.cm.rainbow(np.linspace(0, 1, num_array_jobs)))

ax1.set_xticks(xtick_array)
ax2.set_xticks(xtick_array)
ax1.set_xlabel('observation number')
ax2.set_xlabel('observation number')
ax1.set_ylabel('obj. function value')
ax2.set_ylabel('obj. function value')
ax2.set_ylim(0, 10)
ax1.plot(x_axis, global_min * np.ones(num_obs), label='global_min', color='black')
ax2.plot(x_axis, global_min * np.ones(num_obs), label='global_min', color='black')
plt.legend()

for job in range(num_array_jobs):
    color = next(colors)
    ax1.plot(x_axis, all_obs[job, ], color=color)
    ax2.plot(x_axis, all_best[job, ], color=color)

fig.savefig(f'{save_dir}plots.png')
plt.clf()

print("Plot made and saved successfully.")
