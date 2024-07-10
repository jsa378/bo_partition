import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
plt.figure(figsize=(8, 6), dpi=224)
import sys

plt.rcParams['savefig.dpi'] = 300
plt.rcParams['font.size'] = 18

seed_value = int(sys.argv[1])
test_func_name = sys.argv[2]
r_package = sys.argv[3]
dim = int(sys.argv[4])
num_init_obs = int(sys.argv[5])
num_obs = int(sys.argv[6])
num_runs = int(sys.argv[7])
n_max = int(sys.argv[8])
tol = float(sys.argv[9])
split_crit = str(sys.argv[10])
save_dir = str(sys.argv[11])
num_array_jobs = int(sys.argv[12])

test_func_dicts = {
    'rastr': {
        'global_min': 0
    },
    'schwef': {
        'global_min': 0
    },
}

step_size_dict = {
    10: 1,
    100: 10,
    200: 20,
    400: 50
}

max_obs = 1
for seed in range(1, num_array_jobs + 1):
   number_obs = np.size(pd.read_csv(f'{save_dir}bo_partition_seed_{seed}_obs.csv', header=None, dtype=float, sep='\s+').to_numpy())
   if number_obs > max_obs:
      max_obs = number_obs
all_obs = np.zeros((num_array_jobs, max_obs))
all_best = np.zeros((num_array_jobs, max_obs))
all_ei_vals = np.zeros((num_array_jobs, max_obs))

for seed in range(1, num_array_jobs + 1):
   obs_data = pd.read_csv(f'{save_dir}bo_partition_seed_{seed}_obs.csv', header=None, dtype=float, sep='\s+').to_numpy()
   best_so_far_data = pd.read_csv(f'{save_dir}bo_partition_seed_{seed}_best_so_far.csv', header=None, dtype=float, sep='\s+').to_numpy()
   ei_data = pd.read_csv(f'{save_dir}bo_partition_seed_{seed}_ei_vals.csv', header=None, dtype=float, sep='\s+').to_numpy()
   how_many_obs = np.size(obs_data)
   gap = max_obs - how_many_obs
   aug_obs_data = np.append(obs_data, np.full(gap, None))
   aug_best_so_far_data = np.append(best_so_far_data, np.full(gap, None))
   aug_ei_data = np.append(ei_data, np.full(gap, None))
   all_obs[seed - 1, :] = aug_obs_data
   all_best[seed - 1, :] = aug_best_so_far_data
   all_ei_vals[seed - 1, :] = aug_ei_data

# all_obs = np.zeros((num_array_jobs, num_obs))
# all_best = np.zeros((num_array_jobs, num_obs))
# 
# for seed in range(1, num_array_jobs + 1):
#    all_obs[seed - 1, :] = pd.read_csv(f'{save_dir}bo_partition_seed_{seed}_obs.csv', header=None, dtype=float, sep='\s+').to_numpy()
#    all_best[seed - 1, :] = pd.read_csv(f'{save_dir}bo_partition_seed_{seed}_best_so_far.csv', header=None, dtype=float, sep='\s+').to_numpy()

global_min = test_func_dicts[test_func_name]['global_min']
x_axis_step_size = step_size_dict[num_obs]
# x_axis = np.linspace(1, num_obs, num_obs)
x_axis = np.linspace(1, max_obs, max_obs)
# xtick_array = np.arange(0, num_obs + x_axis_step_size, step=x_axis_step_size)
xtick_array = np.arange(0, max_obs + x_axis_step_size, step=x_axis_step_size)
fig, (ax1, ax2, ax3) = plt.subplots(1, 3, figsize=(60, 30), tight_layout=True)
fig.suptitle(f'bo part, {split_crit}, {r_package}, {test_func_name}, {dim} dim, {num_init_obs} init obs, {num_obs} obs, {n_max} n_max, {tol} tol, {num_array_jobs} runs')
ax1.set_title(f'raw obs')
ax2.set_title(f'best obs so far')
ax3.set_title(f'ei vals')

colors = iter(plt.cm.rainbow(np.linspace(0, 1, num_array_jobs)))

ax1.set_xticks(xtick_array)
ax2.set_xticks(xtick_array)
ax3.set_xticks(xtick_array)
ax1.set_xlabel('observation number')
ax2.set_xlabel('observation number')
ax3.set_xlabel('observation number')
ax1.set_ylabel('obj. function value')
ax2.set_ylabel('obj. function value')
ax3.set_ylabel('ei value')
ax2.set_ylim(0, 100)
# ax1.plot(x_axis, global_min * np.ones(num_obs), label='global_min', color='black')
# ax2.plot(x_axis, global_min * np.ones(num_obs), label='global_min', color='black')
ax1.plot(x_axis, global_min * np.ones(max_obs), label='global_min', color='black')
ax2.plot(x_axis, global_min * np.ones(max_obs), label='global_min', color='black')
ax3.plot(x_axis, tol * np.ones(max_obs), label='tol param', color='black')
plt.legend()

for job in range(num_array_jobs):
    color = next(colors)
    ax1.plot(x_axis, all_obs[job, ], color=color)
    ax2.plot(x_axis, all_best[job, ], color=color)
    ax3.plot(x_axis, all_ei_vals[job, ], color=color)

fig.savefig(f'{save_dir}plots.png')
plt.clf()

print("Plots made and saved successfully.")
