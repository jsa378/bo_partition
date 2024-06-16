import os
import sys
from typing import Optional
import torch
from botorch.test_functions import Rastrigin
from botorch.models import SingleTaskGP
from gpytorch.mlls.marginal_log_likelihood import MarginalLogLikelihood
from botorch.acquisition.objective import GenericMCObjective
from botorch.optim import optimize_acqf
from gpytorch.mlls import ExactMarginalLogLikelihood
from botorch.utils.transforms import standardize, normalize
from botorch.sampling.normal import SobolQMCNormalSampler
import time
import warnings
from botorch import fit_gpytorch_mll
from botorch.acquisition import LogExpectedImprovement, qExpectedImprovement
from botorch.exceptions import BadInitialCandidatesWarning
import numpy as np
import pandas as pd


# seed_value = int(sys.argv[1])
# test_func_name = sys.argv[2]
# dim = int(sys.argv[3])
# num_init_obs = int(sys.argv[4])
# num_obs = int(sys.argv[5])
# num_runs = int(sys.argv[6])
# save_dir = str(sys.argv[7])
# num_array_jobs = int(sys.argv[8])

seed_value = 2
test_func_name = "rastr"
dim = 10
num_init_obs = 40
num_obs = 200
num_runs = 10
save_dir = '/Users/jesse/Downloads/cedar_test_output/'
num_array_jobs = 10

torch.manual_seed(seed_value)

warnings.filterwarnings("ignore", category=BadInitialCandidatesWarning)
warnings.filterwarnings("ignore", category=RuntimeWarning)

device = torch.device("cuda:3" if torch.cuda.is_available() else "cpu")
dtype = torch.double
SMOKE_TEST = os.environ.get("SMOKE_TEST")

BATCH_SIZE = 1
# NUM_RESTARTS = 10 if not SMOKE_TEST else 2
# RAW_SAMPLES = 512 if not SMOKE_TEST else 32
NUM_RESTARTS = 100 if not SMOKE_TEST else 2
RAW_SAMPLES = 1024 if not SMOKE_TEST else 32
MC_SAMPLES = 256 if not SMOKE_TEST else 32

if test_func_name == "rastr":
    test_func = Rastrigin(dim=dim, negate=True)
    bounds = torch.tensor([[-5.12] * dim, [5.12] * dim], device=device, dtype=dtype)

def obj(X):
    return test_func(X)

# init_points = torch.from_numpy(pd.read_csv(f"/home/jsa378/bo_partition/code/implementation_testing/init_points/{test_func_name}_{dim}_dim_{num_runs}_runs_{num_init_obs}_init_points/run_{seed_value}_init_points.csv", header=None, dtype=float, sep='\s+').to_numpy())
# init_points = torch.rand(num_init_obs, dim, device=device, dtype=dtype)
init_points = torch.from_numpy(pd.read_csv(f"/Users/jesse/Downloads/bo_partition/code/implementation_testing/init_points/{test_func_name}_{dim}_dim_{num_runs}_runs_{num_init_obs}_init_points/run_{seed_value}_init_points.csv", header=None, dtype=float, sep=r'\s+').to_numpy())
init_obs = test_func(init_points).unsqueeze(-1)
# train_yvar = torch.full_like(init_obs, 1e-8)
# best_observed_value = obj(init_points).max().item()
best_observed_value = torch.max(init_obs)

def initialize_model(train_x, train_obj, state_dict=None):
    # define models for objective and constraint
    # model_obj = FixedNoiseGP(train_x, train_obj, train_yvar.expand_as(train_obj)).to(
    #     train_x
    # )
    train_yvar = torch.full_like(train_obj, 1e-8)
    standardized_y = standardize(train_obj)
    normalized_x = normalize(train_x, bounds)
    # model_obj = SingleTaskGP(train_X=train_x, train_Y=train_obj, train_Yvar=train_yvar)
    model_obj = SingleTaskGP(train_X=normalized_x, train_Y=standardized_y, train_Yvar=train_yvar)
    # model = ModelListGP(model_obj)
    # mll = SumMarginalLogLikelihood(model.likelihood, model)
    # load state dict if it is passed
    if state_dict is not None:
        # model.load_state_dict(state_dict)
        model_obj.load_state_dict(state_dict)
    # return mll, model
    # return model_obj.likelihood, model_obj
    mll = ExactMarginalLogLikelihood(model_obj.likelihood, model_obj)
    return mll, model_obj

def obj_callable(Z: torch.Tensor, X: Optional[torch.Tensor] = None):
    return Z[..., 0]
unconstrained_obj = GenericMCObjective(objective=obj_callable)

def optimize_acqf_and_get_observation(acq_func):
    """Optimizes the acquisition function, and returns a new candidate and a noisy observation."""
    # optimize
    candidates, _ = optimize_acqf(
        acq_function=acq_func,
        bounds=bounds,
        q=BATCH_SIZE,
        num_restarts=NUM_RESTARTS,
        raw_samples=RAW_SAMPLES,  # used for intialization heuristic
        options={"batch_limit": 5, "maxiter": 200},
    )
    # observe new values
    new_x = candidates.detach()
    # exact_obj = neg_goldsteinprice(new_x).unsqueeze(-1)  # add output dimension
    # new_obj = exact_obj + NOISE_SE * torch.randn_like(exact_obj)
    new_obj = test_func(new_x).unsqueeze(-1)
    return new_x, new_obj

obs, best_so_far = torch.zeros(num_obs), torch.zeros(num_obs)
mll, model = initialize_model(init_points, init_obs)

train_points = init_points
train_obs = init_obs

start = time.time()

for step in range(1, num_obs + 1):
    fit_gpytorch_mll(mll)
    logEI = LogExpectedImprovement(
        model=model,
        best_f=best_observed_value
    )
    new_point, new_obj = optimize_acqf_and_get_observation(logEI)
    # qmc_sampler = SobolQMCNormalSampler(sample_shape=torch.Size([MC_SAMPLES]))
    # qEI = qExpectedImprovement(
    #         model=model,
    #         best_f=best_observed_value,
    #         sampler=qmc_sampler,
    #         objective=unconstrained_obj,
    # )
    # new_point, new_obj = optimize_acqf_and_get_observation(qEI)

    print(f'The {step}th observation is: {new_obj}.')
    obs[step - 1] = new_obj
    best_so_far[step - 1] = torch.max(obs[0:(step - 1) + 1])
    best_observed_value = best_so_far[step - 1]
    train_points = torch.cat([train_points, new_point])
    train_obs = torch.cat([train_obs, new_obj])
    # best_observed_value = obj(train_obs).max().item()
    print(f'The best observed value so far is: {best_observed_value}.')
    mll, model = initialize_model(train_points, train_obs, model.state_dict())

obs, best_so_far = -obs.numpy(), -best_so_far.numpy()

np.savetxt(f'{save_dir}python_seed_{seed_value}_obs.csv', obs, fmt='%1.8f', delimiter=' ')
np.savetxt(f'{save_dir}python_seed_{seed_value}_best_so_var.csv', best_so_far, fmt='%1.8f', delimiter=' ')

end = time.time()
duration = end - start
hours = duration // 3600
minutes = (duration - 3600 * hours) // 60
seconds = (duration - 3600 * hours - 60 * minutes)
print(f"Total execution time is {int(hours)} hours, {int(minutes)} minutes and {int(seconds)} seconds.")