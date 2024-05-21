import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
plt.figure(figsize=(8, 6), dpi=224)

num_obs = 100
reg_obs = num_obs // 2
num_runs = 10
global_mins = {
    'ackley': 0,
    'rastr': 0,
}
test_func_names = ['ackley', 'rastr']

obs = pd.read_csv(f'/Users/jesse/Downloads/bo_partition/data/bo_runs/{test_func_names[1]}_obs.csv', header=None, dtype=float, sep='\s+').to_numpy()
best = pd.read_csv(f'/Users/jesse/Downloads/bo_partition/data/bo_runs/{test_func_names[1]}_best_so_far.csv', header=None, dtype=float, sep='\s+').to_numpy()
x_axis_step_size = 5
x_axis = np.linspace(1, num_obs, num_obs)
plt.xticks(np.arange(0, num_obs + x_axis_step_size, step=x_axis_step_size))
mean_obs = np.mean(obs, axis=0)
sd_obs = np.std(obs, axis=0)
mean_best = np.mean(best, axis=0)
sd_best = np.std(best, axis=0)

reg_x_axis = np.linspace(1, reg_obs, reg_obs)

reg_1_obs = pd.read_csv(f'/Users/jesse/Downloads/bo_partition/data/bo_runs/{test_func_names[1]}_reg_1_obs.csv', header=None, dtype=float, sep='\s+').to_numpy()
reg_1_best = pd.read_csv(f'/Users/jesse/Downloads/bo_partition/data/bo_runs/{test_func_names[1]}_reg_1_best_so_far.csv', header=None, dtype=float, sep='\s+').to_numpy()
reg_1_mean_obs = np.mean(reg_1_obs, axis=0)
reg_1_sd_obs = np.std(reg_1_obs, axis=0)
reg_1_mean_best = np.mean(reg_1_best, axis=0)
reg_1_sd_best = np.std(reg_1_best, axis=0)

reg_2_obs = pd.read_csv(f'/Users/jesse/Downloads/bo_partition/data/bo_runs/{test_func_names[1]}_reg_2_obs.csv', header=None, dtype=float, sep='\s+').to_numpy()
reg_2_best = pd.read_csv(f'/Users/jesse/Downloads/bo_partition/data/bo_runs/{test_func_names[1]}_reg_2_best_so_far.csv', header=None, dtype=float, sep='\s+').to_numpy()
reg_2_mean_obs = np.mean(reg_2_obs, axis=0)
reg_2_sd_obs = np.std(reg_2_obs, axis=0)
reg_2_mean_best = np.mean(reg_2_best, axis=0)
reg_2_sd_best = np.std(reg_2_best, axis=0)

plt.plot(x_axis, global_mins['rastr'] * np.ones(num_obs), label='global min.', color='black')

plt.plot(x_axis, mean_obs, label='value at obs', color='blue')
plt.fill_between(x_axis, mean_obs - sd_obs, mean_obs + sd_obs, alpha=0.2)
plt.plot(x_axis, mean_best, label='best so far', color='red')
plt.fill_between(x_axis, mean_best - sd_best, mean_best + sd_best, alpha=0.2)

plt.plot(reg_x_axis, reg_1_mean_obs, label='(reg 1) value at obs', color='orange')
plt.fill_between(reg_x_axis, reg_1_mean_obs - reg_1_sd_obs, reg_1_mean_obs + reg_1_sd_obs, alpha=0.2)
plt.plot(reg_x_axis, reg_1_mean_best, label='(reg 1) best so far', color='yellow')
plt.fill_between(reg_x_axis, reg_1_mean_best - reg_1_sd_best, reg_1_mean_best + reg_1_sd_best, alpha=0.2)

plt.plot(reg_x_axis, reg_2_mean_obs, label='(reg 2) value at obs', color='purple')
plt.fill_between(reg_x_axis, reg_2_mean_obs - reg_2_sd_obs, reg_2_mean_obs + reg_2_sd_obs, alpha=0.2)
plt.plot(reg_x_axis, reg_2_mean_best, label='(reg 2) best so far', color='maroon')
plt.fill_between(reg_x_axis, reg_2_mean_best - reg_2_sd_best, reg_2_mean_best + reg_2_sd_best, alpha=0.2)

plt.xlabel('observation number')
plt.ylabel('obj. function value')
plt.title(f'{test_func_names[1]} ({num_runs} runs)')
plt.legend()
plt.savefig(f'/Users/jesse/Downloads/bo_partition/plots/bo_runs/{test_func_names[1]}_obs.png')
plt.clf()