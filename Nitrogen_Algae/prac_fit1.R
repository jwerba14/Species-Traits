library(tidyverse)
library(shinystan)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


d2 <- read.csv("Data/Algae_Nutrient.csv")


#t_obs <- dat_27 %>% filter(date1 > 1)

## this is to fit just one treatment, one rep

amm_chl_prior <- stan_model(file = "amm_chl_prior.stan", model_name = "amm_chl_prior", verbose = T)
d2$treat1 <- as.numeric(as.factor(d2$treat))

for (i in 6:length(unique(d2$treat1))){
  newdat <- d2 %>% filter(treat1 == 4)
  for (j in 1:length(unique(newdat$rep))){
    dat_paste <- newdat %>% filter(rep == 5)
 
  test_div <- 1
  ode_list <- list(
    N = nrow(dat_paste),
    T = length(seq(1,11)),
    y = dat_paste[, c(8,5)],
    t0 = 0,
    t_obs= dat_paste$date1
  )
  
  while (test_div > 0) {    
  estimates <- sampling(object = amm_chl_prior,
                        data = ode_list, chains = 4,
                        control = list(adapt_delta = 0.99,
                                       max_treedepth = 15))

  test_div1 <- try(sum(attr(estimates@sim$samples[[4]], "sampler_params")$divergent__[1001:2000]), silent = TRUE)
  test_div2 <- try(sum(attr(estimates@sim$samples[[3]], "sampler_params")$divergent__[1001:2000]), silent = TRUE)
  test_div3 <- try(sum(attr(estimates@sim$samples[[2]], "sampler_params")$divergent__[1001:2000]), silent = TRUE)
  test_div4 <- try(sum(attr(estimates@sim$samples[[1]], "sampler_params")$divergent__[1001:2000]), silent = TRUE) 
          
  test_div <- sum(
    ifelse(is.numeric(test_div1),test_div1,0)
  , ifelse(is.numeric(test_div2),test_div2,0)
  , ifelse(is.numeric(test_div3),test_div3,0)
  , ifelse(is.numeric(test_div4),test_div4,0))
  
  }
  
  tempname <- paste(paste("nitalg.prior",i,j, sep = "_"),".RDS", sep = "")
  
  saveRDS(estimates, file = tempname)
  print(i)
  print(j)
  Sys.sleep(2400)
  }
}

checktime <- system.time({
  estimates <- sampling(object = ode_apr10,
                        data = ode_list, chains = 4,
                        control = list(adapt_delta = 0.99,
                                       max_treedepth = 15))
  
})


 #launch_shinystan(estimates)

