## top level death parameters
setwd("~/GitHub/Species-Traits/Daphnia/Death")


## fit exponential death with stan 
library(tidyverse)
library(gridExtra)
library(rstan)
library(fitdistrplus)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(shinystan)
source("../../transfer_functions.R")
source("../../Graphing_Set_Up.R")


## read and clean data

source("death.clean.data.R")

## fit data with wide priors

source("death.wide.R")

## fit literature only

source("death.lit.only.R")

## fit with literature as the prior

source("death.lit.prior.R")


##combine graphs
grid.arrange(lit_g, wide_g, inf_g)

##combine parameters
death_est <- list(
  "lit only" = pp, ## for now only unweighted...
  "wide_prior"=fit_sum_param_d,
  "informed" = fit_sum_param_inf
)



