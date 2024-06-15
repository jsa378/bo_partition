import os
import sys
from typing import Optional
import torch
from botorch.test_functions import Rastrigin
from botorch.models import SingleTaskGP
from gpytorch.mlls.marginal_log_likelihood import MarginalLogLikelihood
from botorch.acquisition.objective import GenericMCObjective
from botorch.optim import optimize_acqf
import time
import warnings
from botorch import fit_gpytorch_mll
from botorch.acquisition import LogExpectedImprovement
from botorch.exceptions import BadInitialCandidatesWarning
import numpy as np
import pandas as pd

seed_value = int(sys.argv[1])
test_func_name = sys.argv[2]
dim = int(sys.argv[3])
num_init_obs = int(sys.argv[4])
num_obs = int(sys.argv[5])
num_runs = int(sys.argv[6])
save_dir = str(sys.argv[7])
num_array_jobs = int(sys.argv[8])

warnings.filterwarnings("ignore", category=BadInitialCandidatesWarning)
warnings.filterwarnings("ignore", category=RuntimeWarning)

device = torch.device("cuda:3" if torch.cuda.is_available() else "cpu")
dtype = torch.double
SMOKE_TEST = os.environ.get("SMOKE_TEST")

if test_func_name == "rastr":
    test_func = Rastrigin(dim=dim, negate=True)
    bounds = torch.tensor([[-5.12] * dim, [5.12] * dim], device=device, dtype=dtype)

def obj(X):
    return test_func(X)

init_points = torch.from_numpy(pd.read_csv(f"/home/jsa378/bo_partition/code/implementation_testing/init_points/{test_func_name}_{dim}_dim_{num_runs}_runs_{num_init_obs}_init_points/run_{seed_value}_init_points.csv", header=None, dtype=float, sep='\s+').to_numpy())
init_obs = test_func(init_points).unsqueeze(-1)
train_yvar = torch.full_like(init_obs, 1e-8)
best_observed_value = obj(init_points).max().item()

model_obj = SingleTaskGP(train_X=init_points, train_Y=init_obs, train_Yvar=train_yvar)

# thoughts
# i should maybe try to test this locally before testing it on cedar(?)
# do i need to use anything from botorch.utils.transforms?

start = time.time()