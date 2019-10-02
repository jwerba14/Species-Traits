## read in all RDS and make dataframe with all parameters for ODE
library(rstan)
library(tidyverse)
library(brms)

growth <- readRDS("growth.rds") ## 853 divergent transitions but mixing looks good ?? 
adult_death <- readRDS("adult_death.rds") ##
adult_feed <- readRDS("adult_feeding.rds") ##
adult_exc <- readRDS("adult_exc_new.RDS") ##
juv_feed <- readRDS("juv_feeding.RDS") ##
juv_exc <- readRDS("juv_exec_update.RDS") ##
fec <- readRDS("fec_stan.rds")  ##
#cerioPop <- readRDS("cerio_pop.rds") ##
#cerio_feed <- readRDS()
#cerio_exc <- readRDS()
amm_loss <- readRDS("ammonium.rds") ##


growth_param <- rstan::extract(growth,permuted = FALSE)  ## saturating
a_death_param <- rstan::extract(adult_death,permuted = FALSE) ## exponential
a_feed_param <- rstan::extract(adult_feed,permuted = FALSE) ## linear
a_exc_param <- rstan::extract(adult_exc,permuted = FALSE)  ## linear
j_feed_param <- rstan::extract(juv_feed,permuted = FALSE)  ## linear
j_exc_param <- rstan::extract(juv_exc,permuted = FALSE)  ## linear
amm_param <- posterior_samples(amm_loss) ## intercept only -- prop loss bc calc with change 
amm_param_b <- amm_param %>% select(b_Intercept)
fec_param <- rstan::extract(fec,permuted = FALSE) ## saturating

param <- data.frame(
  growth_a = exp(c(growth_param[,1,1],growth_param[,2,1], growth_param[,3,1], growth_param[,4,1]))[1:4000],
  growth_b = exp(c(growth_param[,1,2] , growth_param[,2,2], growth_param[,3,2], growth_param[,4,2]))[1:4000],
  death_b = c(a_death_param[,1,1],a_death_param[,2,1],a_death_param[,3,1],a_death_param[,4,1])[1:4000] ,
  a_feed_m = c(a_feed_param[,1,1],a_feed_param[,2,1],a_feed_param[,3,1],a_feed_param[,4,1])[1:4000],
  a_feed_b = c(a_feed_param[,1,2] , a_feed_param[,2,2], a_feed_param[,3,2], a_feed_param[,4,2])[1:4000],
  a_exc_m = c(a_exc_param[,1,1],a_exc_param[,2,1],a_exc_param[,3,1],a_exc_param[,4,1])[1:4000],
  a_exc_b = c(a_exc_param[,1,2] ,a_exc_param[,2,2], a_exc_param[,3,2], a_exc_param[,4,2])[1:4000],
  j_feed_m = c(j_feed_param[,1,1],j_feed_param[,2,1],j_feed_param[,3,1],j_feed_param[,4,1])[1:4000],
  j_feed_b = c(j_feed_param[,1,2] , j_feed_param[,2,2], j_feed_param[,3,2], j_feed_param[,4,2])[1:4000],
  j_exc_m = c(j_exc_param[,1,1],j_exc_param[,2,1],j_exc_param[,3,1],j_exc_param[,4,1])[1:4000],
  j_exc_b = c(j_exc_param[,1,2] ,j_exc_param[,2,2], j_exc_param[,3,2], j_exc_param[,4,2])[1:4000],
 amm_param_b = amm_param_b[,1],
 fec_a = c(fec_param[,1,1],fec_param[,2,1],fec_param[,3,1],fec_param[,4,1])[1:4000],
 fec_b = c(fec_param[,1,2] ,fec_param[,2,2], fec_param[,3,2], fec_param[,4,2])[1:4000]
)

write.csv(param, file = "parameters_no_cerio.csv")
