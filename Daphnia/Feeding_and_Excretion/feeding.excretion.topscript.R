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
  "mixed_model_vs" = fit_sum_param_mixed,
  "wide_prior"=fit_sum_param_wide,
  "lit_imp" = fit_sum_param_lit,
  "lit_vs" = fit_sum_param_lit_s,
  "mixed_imp" = fit_sum_param_mimp
)

## make into usable dataframe

feed_exc <- data.frame(  ha_median_wide  = feed_exc_est$wide_prior[1,6],
                         ha_lwr_wide =feed_exc_est$wide_prior[1,4] ,
                         ha_upr_wide =feed_exc_est$wide_prior[1,8] ,  
                         xa_median_wide  = feed_exc_est$wide_prior[3,6],
                         xa_lwr_wide =feed_exc_est$wide_prior[3,4] ,
                         xa_upr_wide =feed_exc_est$wide_prior[3,8] ,
                         ha_median_mixvs  = feed_exc_est$mixed_model_vs[1,6],  
                         ha_lwr_mixvs =feed_exc_est$mixed_model_vs[1,4] ,
                         ha_upr_mixvs =feed_exc_est$mixed_model_vs[1,8] ,  
                         xa_median_mixvs  = feed_exc_est$mixed_model_vs[4,6],
                         xa_lwr_mixvs =feed_exc_est$mixed_model_vs[4,4] ,
                         xa_upr_mixvs =feed_exc_est$mixed_model_vs[4,8] , 
                         ha_median_litimp  = feed_exc_est$lit_imp[1,6],
                         ha_lwr_litimp =feed_exc_est$lit_imp[1,4] ,
                         ha_upr_litimp =feed_exc_est$lit_imp[1,8] ,  
                         ha_median_litvs =feed_exc_est$lit_vs[1,6] ,
                         ha_upr_litvs =feed_exc_est$lit_vs[1,8],
                         ha_lwr_litvs =feed_exc_est$lit_vs[1,4],
                         ha_median_mmimp =feed_exc_est$mixed_imp[1,6] ,
                         ha_upr_mmimp =feed_exc_est$mixed_imp[1,8],
                         ha_lwr_mmimp =feed_exc_est$mixed_imp[1,4],
                         xa_median_mmimp = feed_exc_est$mixed_imp[4,6],
                         xa_lwr_mmimp =feed_exc_est$mixed_imp[4,4],
                         xa_upr_mmimp =feed_exc_est$mixed_imp[4,8])

feed_exc_a <- feed_exc%>% 
  pivot_longer(cols= everything() , names_to = "param", values_to = "value") %>%
  separate(col = param,into =c("param","quant", "method") ,sep = "_")

