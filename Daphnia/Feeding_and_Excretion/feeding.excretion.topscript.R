## feeding and excretion top level script

## 
setwd("~/GitHub/Species-Traits/Daphnia/Feeding_and_Excretion")

source("../../transfer_functions.R")
source("../../chl_adj.R")
source("../../Graphing_Set_Up.R")
library(tidyverse)
library(gridExtra)
library(nlmrt)
library(fitdistrplus)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(shinystan)



## read and clean data

source("feeding.excretion.data.clean.R")


## fit excretion and feeding together, wide priors

source("feeding.excretion.wide.R")

## fit excretion and feeding with mixed slopes for each feeding literature study

source("feeding.excretion.mixed.R")


## fit excretion and feeding with imputed sd for each feeding literature study

source("feeding.excretion.imp.R")

## fit feeding literature only with varying slope

source("feeding.lit.varyslope.R")


## fit feeding literature only with imputed SDs

source("feeding.lit.impute.R")

## compile feeding graphs
frg <- grid.arrange(stan_wide_g, stan_mixed_g, stan_mix_imp, lit_g, lit_g_s)

## excretion graph(s?) 
stan_wideex_g

## compile parameters
feed_exc_est <- list(
  "mixed_model_vs" = fit_sum_mixed,
  "wide_prior"=fit_sum_wide,
  "lit_imp" = fit_sum_lit,
  "lit_vs" = fit_sum_lit_s,
  "mixed_imp" = fit_sum_mimp
)


