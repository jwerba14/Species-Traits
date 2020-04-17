library(tidyverse)
library(shinystan)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

d1 <- read.csv("Data/Nh4_Air.csv")
d2 <- read.csv("Data/Algae_Nutrient.csv")

ammonium <- d1 %>% drop_na()

#same early morning machine problem
ammonium <- ammonium %>%
  filter(Rep != 1 & Rep != 15 & Rep != 52) 


dat_27 <- d2 %>%
  filter(treat == 27) %>%
  filter(rep == 2)

dat_27a <- d2 %>%
  filter(treat == 27) %>%
  filter(rep == 3)

#t_obs <- dat_27 %>% filter(date1 > 1)

## this is to fit just one treatment, one rep

ode_apr10 <- stan_model(file = "nitalg.ode.apr10.stan", model_name = "ode_apr10", verbose = T)
d2$treat1 <- as.numeric(as.factor(d2$treat))

for (i in 2:length(unique(d2$treat1))){
  newdat <- d2 %>% filter(treat1 == i)
  for (j in 1:length(unique(newdat$rep))){
    dat_paste <- newdat %>% filter(rep == j)
 
  test_div <- 1
  ode_list <- list(
    N = nrow(dat_paste),
    T = length(seq(1,11)),
    y = dat_paste[, c(8,5)],
    t0 = 0,
    t_obs= dat_paste$date1
  )
  
  while (test_div > 0) {    
  estimates <- sampling(object = ode_apr10,
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
  
  tempname <- paste(paste("nitalg",i,j, sep = "_"),".RDS", sep = "")
  
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


 launch_shinystan(estimates)

## now to fit all reps at once 
ode_multi <- stan_model(file = "fit_w_multi_rep.stan", model_name = "fit_w_multi_rep", verbose = T)
 
two_rep <- data.frame(
  y1 = dat_27$nh4,
  y2 = dat_27$chl,
  y3 = dat_27a$nh4,
  y4 = dat_27a$chl
)

ode_list1 <- list(
  N = nrow(dat_27),
  T = length(seq(1,11)),
  y = two_rep,
  t0 = 0,
  t_obs= dat_27$date1
)

estimates2 <- sampling(object = ode_multi,
                      data = ode_list1, chains = 4,
                      control = list(adapt_delta = 0.95,
                                     max_treedepth = 15))
 
                  