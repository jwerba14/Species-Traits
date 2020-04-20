### Packages and functions needed for all scripts
##hmmm currently indexing of folders is confusing because if you source and you're not in that folder can't source other files
## but then need to be back in this folder to source anything else...so now im re-setting wd a lot, and re loading things
## which doesn't seem at all streamlined...

source("set_up.R")


###For fecundity parameters
 
source("Fecundity/fecundity.top.script.R")  ## see script to only run pieces


### For death rate
setwd("~/GitHub/Species-Traits/Daphnia")
source("Death/death.topscript.R")



### For feeding and excretion rate
setwd("~/GitHub/Species-Traits/Daphnia")
source("Feeding_and_Excretion/feeding.excretion.topscript.R")

# Juevenile growth rate, feeding and excretion
setwd("~/GitHub/Species-Traits/Daphnia")
source("Juveniles/juveniles.topscript.R")




## parameter table for daphnia



param_daphnia_dat_only <- data.frame(
 b1_median  = fecundity_est$wide_prior[1,6],
 b1_lwr =fecundity_est$wide_prior[1,4] ,
 b1_upr =fecundity_est$wide_prior[1,8] ,  
 b2_median  = fecundity_est$wide_prior[2,6],
 b2_lwr =fecundity_est$wide_prior[2,4] ,
 b2_upr =fecundity_est$wide_prior[2,8] , 
 xa_median = feed_exc_est$wide_prior[3,6],
 xa_lwr = feed_exc_est$wide_prior[3,4],
 xa_upr =feed_exc_est$wide_prior[3,8],
 ha_median = feed_exc_est$wide_prior[1,6],
 ha_upr = feed_exc_est$wide_prior[1,4],
 ha_lwr = feed_exc_est$wide_prior[1,8],
 death2_median = death_est$wide_prior[1,6], 
 death2_lwr =death_est$wide_prior[1,4],
 death2_upr = death_est$wide_prior[1,8],
 hj_median =fit_sum_param_fej0[1,6],
 hj_lwr =fit_sum_param_fej0[1,4],
 hj_upr = fit_sum_param_fej0[1,8],
 g_median = fit_sum_paramg[1,6] ,
 g_lwr = fit_sum_paramg[1,4],
 g_upr = fit_sum_paramg[1,8], 
 xj_median = fit_sum_param_fej0[3,6],
 xj_lwr = fit_sum_param_fej0[3,4],
 xj_upr = fit_sum_param_fej0[3,8]
 
)

param_daphnia_dat_only1 <- param_daphnia_dat_only %>% 
  pivot_longer(cols= everything() , names_to = "param", values_to = "value") %>%
  separate(col = param,into =c("param","quant") ,sep = "_") %>%
  mutate(method = "dat_only")



param_daphnia_lit_only <- data.frame(
  b1_median  = fecundity_est$lit[2,6],  ## check scale... 
  b1_lwr =fecundity_est$lit[2,4] ,
  b1_upr =fecundity_est$lit[2,8] ,  
  b2_median  = fecundity_est$lit[4,6],
  b2_lwr =fecundity_est$lit[4,4] ,
  b2_upr =fecundity_est$lit[4,8] , 
  xa_median = feed_exc_est$wide_prior[3,6],
  xa_lwr = feed_exc_est$wide_prior[3,4],
  xa_upr =feed_exc_est$wide_prior[3,8],
  ha_median = feed_exc_est$lit_imp[1,6],
  ha_lwr = feed_exc_est$lit_imp[1,4],
  ha_upr = feed_exc_est$lit_imp[1,8], 
  death2_median = pp[2], 
  death2_lwr =pp[1],
  death2_upr = pp[3],
  hj_median =fit_sum_param_fej0[1,6],
  hj_lwr =fit_sum_param_fej0[1,4],
  hj_upr = fit_sum_param_fej0[1,8],
  g_median = fit_sum_paramg[1,6] ,
  g_lwr = fit_sum_paramg[1,4],
  g_upr = fit_sum_paramg[1,8], 
  xj_median = fit_sum_param_fej0[3,6],
  xj_lwr = fit_sum_param_fej0[3,4],
  xj_upr = fit_sum_param_fej0[3,8]

)


param_daphnia_lit_only1 <- param_daphnia_lit_only %>% 
  pivot_longer(cols= everything() , names_to = "param", values_to = "value") %>%
  separate(col = param,into =c("param","quant") ,sep = "_") %>%
  mutate(method = "lit_only")

  
param_daphnia_mixed <- data.frame(
  b1_median  = fecundity_est$mixed_model[2,6],
  b1_lwr =fecundity_est$mixed_model[2,4] ,
  b1_upr =fecundity_est$mixed_model[2,8] ,  
  b2_median  = fecundity_est$mixed_model[4,6],
  b2_lwr =fecundity_est$mixed_model[4,4] ,
  b2_upr =fecundity_est$mixed_model[4,8] , 
  xa_median = feed_exc_est$mixed_model_vs[4,6],
  xa_lwr = feed_exc_est$mixed_model_vs[4,4],
  xa_upr =feed_exc_est$mixed_model_vs[4,8],
  ha_median = feed_exc_est$mixed_model_vs[1,6],
  ha_upr = feed_exc_est$mixed_model_vs[1,4],
  ha_lwr = feed_exc_est$mixed_model_vs[1,8],
  death2_median = death_est$informed[1,6], 
  death2_lwr =death_est$informed[1,4],
  death2_upr = death_est$informed[1,8],
  hj_median =fit_sum_param_fej[1,6],
  hj_lwr =fit_sum_param_fej[1,4],
  hj_upr = fit_sum_param_fej[1,8],
  g_median = fit_sum_paramg[1,6] ,
  g_lwr = fit_sum_paramg[1,4],
  g_upr = fit_sum_paramg[1,8], 
  xj_median = fit_sum_param_fej0[3,6],
  xj_lwr = fit_sum_param_fej0[3,4],
  xj_upr = fit_sum_param_fej0[3,8]
)


param_daphnia_mixed1 <- param_daphnia_mixed %>% 
  pivot_longer(cols= everything() , names_to = "param", values_to = "value") %>%
  separate(col = param,into =c("param","quant") ,sep = "_") %>%
  mutate(method = "mixed")


daphnia_param <- rbind(param_daphnia_dat_only1,param_daphnia_lit_only1,param_daphnia_mixed1)

#write.csv(daphnia_param, file = "daphnia_params.csv")
