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

print(grid.arrange(nls_fec_g,stan_lit_g1,stan_lit_sat_g2,stan_wide_g,stan_hyper_g,stan_con_g1, nrow=3))

## stitch together parameters

fecundity_est <- list(
  "mixed_model" = fit_sum_param_mix,
  "wide_prior"=fit_sum_param_wide,
  "constrain_a" = fit_sum_param_cona,
  "nls" = pred_sum_nls,
  "hyper" = fit_sum_param_hyper,
  "lit" = fit_sum_param_lit
)
## full data frame

## fecundity param dataframe

fecundity <- data.frame( b1_median_wide  = fecundity_est$wide_prior[1,6],
                         b1_lwr_wide =fecundity_est$wide_prior[1,4] ,
                         b1_upr_wide =fecundity_est$wide_prior[1,8] ,  
                         b2_median_wide  = fecundity_est$wide_prior[2,6],
                         b2_lwr_wide =fecundity_est$wide_prior[2,4] ,
                         b2_upr_wide =fecundity_est$wide_prior[2,8] ,
                         b1_median_lit  = fecundity_est$lit[2,6],  ## check scale... 
                         b1_lwr_lit =fecundity_est$lit[2,4] ,
                         b1_upr_lit =fecundity_est$lit[2,8] ,  
                         b2_median_lit  = fecundity_est$lit[4,6],
                         b2_lwr_lit =fecundity_est$lit[4,4] ,
                         b2_upr_lit =fecundity_est$lit[4,8] , 
                         b1_median_mm  = fecundity_est$mixed_model[2,6],
                         b1_lwr_mm =fecundity_est$mixed_model[2,4] ,
                         b1_upr_mm =fecundity_est$mixed_model[2,8] ,  
                         b2_median_mm  = fecundity_est$mixed_model[4,6],
                         b2_lwr_mm =fecundity_est$mixed_model[4,4] ,
                         b2_upr_mm =fecundity_est$mixed_model[4,8] , 
                         b1_median_hyper =fecundity_est$hyper[1,6] ,
                         b1_upr_hyper =fecundity_est$hyper[1,8],
                         b1_lwr_hyper =fecundity_est$hyper[1,4],
                         b2_median_hyper = fecundity_est$hyper[2,6],
                         b2_lwr_hyper =fecundity_est$hyper[2,4],
                         b2_upr_hyper =fecundity_est$hyper[2,8],  
                         b1_median_conb1 =fecundity_est$constrain_a[2,6] ,
                         b1_upr_conb1 =fecundity_est$constrain_a[2,8],
                         b1_lwr_conb1 =fecundity_est$constrain_a[2,4],
                         b2_median_conb1 = fecundity_est$constrain_a[4,6],
                         b2_lwr_conb1 =fecundity_est$constrain_a[4,4],
                         b2_upr_conb1 =fecundity_est$constrain_a[4,8])

fecund <- fecundity %>% 
  pivot_longer(cols= everything() , names_to = "param", values_to = "value") %>%
  separate(col = param,into =c("param","quant", "method") ,sep = "_")