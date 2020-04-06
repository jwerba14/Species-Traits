
library(tidyverse)
library(nlstools)
library(gridExtra)
library(fitdistrplus)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(shinystan)

## read in needed functions and graphing set up
source("../transfer_functions.R")
source("../chl_adj.R")
source("../Graphing_Set_Up.R")