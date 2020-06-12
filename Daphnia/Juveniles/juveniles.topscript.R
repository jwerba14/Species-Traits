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

## feeding/excretion rates not constraining juvenile excretion # not sure this actually exisits. 
#source("juv_daphnia_feeding_and_excretion_stan.R")


## feeding/excretion rates constraining juvenile excretion > 0
source("juv_daphnia_feeding_and_excretion_0.R")


## feeding/ excretion graph
grid.arrange(stan_fej_g1, stan_wideexj_g1)


## make dataframe of params
## put into day from hour and mg to ug N

juv_p <- data.frame(hj_median_wide =fit_sum_param_fej0[1,6]*24,
hj_lwr_wide =fit_sum_param_fej0[1,4]*24,
hj_upr_wide = fit_sum_param_fej0[1,8]*24,
g_median_wide = fit_sum_paramg[1,6] ,
g_lwr_wide = fit_sum_paramg[1,4],
g_upr_wide = fit_sum_paramg[1,8], 
xj_median_wide = fit_sum_param_fej0[3,6]*1000,
xj_lwr_wide = fit_sum_param_fej0[3,4]*1000,
xj_upr_wide = fit_sum_param_fej0[3,8]*1000)

juv_param <- juv_p %>% 
  pivot_longer(cols= everything() , names_to = "param", values_to = "value") %>%
  separate(col = param,into =c("param","quant", "method") ,sep = "_")

