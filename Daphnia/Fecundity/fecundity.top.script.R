##Top level script for Fecundity

## set working directory

setwd("~/GitHub/Species-Traits/Daphnia/Fecundity")

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(shinystan)
## set up- loads packages and functions

#source("../set_up.R")

### read in data and clean data

source("fecundity.data.clean.R")


### fit and graph just to literature data

source("fecundity.lit.only.R")

### fit just to lab data (wide priors)

source("fecundity.wide.R")

## fit just to lab data (NLS)

source("fecundity.nls.R")

### mixed model 

source("fecundity.mixed.R")

## mixed model constrain upper bound

source("fecundity.constrained.upper.R")

## use literature as "hyper" parameter

source("fecundity.hyperparameter.R")

## stitch together graphs

print(grid.arrange(nls_fec_g,stan_lit_g,stan_lit_sat_g,stan_wide_g,stan_hyper_g,stan_con_g, nrow=3))

## stitch together parameters

fecundity_est <- list(
  "mixed_model" = fit_sum_param_mix,
  "wide_prior"=fit_sum_param_wide,
  "constrain_a" = fit_sum_param_cona,
  "nls" = pred_sum_nls,
  "hyper" = pred_sum_hyper,
  "lit" = fit_sum_param_lit
)
