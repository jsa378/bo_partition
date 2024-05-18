import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
plt.figure(figsize=(8, 6), dpi=224)

num_obs = 20
num_runs = 2
global_mins = {
    'ackley': 0,
}
test_func_names = {
    'ackley': 'ackley'
}

ackley_obs = pd.read_csv('/Users/jesse/Downloads/bo_partition/data/bo_runs/ackley_obs.csv', header=None, dtype=float, sep='\s+').to_numpy()
x_axis_step_size = 2
x_axis = np.linspace(1, num_obs + x_axis_step_size, num_obs)
plt.xticks(np.arange(0, num_obs, step=x_axis_step_size))
ackley_mean_obs = np.mean(ackley_obs, axis=0)
ackley_sd_obs = np.std(ackley_obs, axis=0)

plt.plot(x_axis, global_mins['ackley'] * np.ones(num_obs), label='global min.', color='black')
plt.plot(x_axis, ackley_mean_obs, label='ackley', color='blue')
plt.fill_between(x_axis, ackley_mean_obs - ackley_sd_obs, ackley_mean_obs + ackley_sd_obs, alpha=0.2)

plt.xlabel('observation number')
plt.ylabel('obj. function value')
plt.title(f'objective function values ({num_runs} runs)')
plt.legend()
plt.savefig(f'/Users/jesse/Downloads/bo_partition/plots/bo_runs/{test_func_names["ackley"]}_obs.png')
plt.clf()