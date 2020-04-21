## top level script for fitting juvenile daphnia parameters
setwd("~/GitHub/Species-Traits/Daphnia/Juveniles")

source("../../transfer_functions.R")
source("../../Graphing_Set_Up.R")
library(tidyverse)
library(gridExtra)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(shinystan)



## growth rate
source("growth_rate.R")

## growth rate graphs
grid.arrange(growth_g, grg)

## set up for excretion and feeding
source("set_up_fex.R")

## feeding/excretion rates not constraining juvenile excretion
source("juv_daphnia_feeding_and_excretion_stan.R")


## feeding/excretion rates constraining juvenile excretion > 0
source("juv_daphnia_feeding_and_excretion_stan.R")


## feeding/ excretion graph
grid.arrange(stan_fej_g1, stan_wideexj_g1)

