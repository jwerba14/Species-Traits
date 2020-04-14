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

ode_list <- list(
  N = nrow(dat_27),
  T = length(seq(1,11)),
  y = dat_27[, c(8,5)],
  t0 = 0,
  t_obs= dat_27$date1
)



ode_apr10 <- stan_model(file = "nitalg.ode.apr10.stan", model_name = "ode_apr10", verbose = T)

checktime <- system.time({
  estimates <- sampling(object = ode_apr10,
                        data = ode_list, chains = 4,
                        control = list(adapt_delta = 0.99,
                                       max_treedepth = 15))
  
})


## 2 divergent transitions in all 4 chains bc not final model at not at adapt_delta = 0.95 not going to worry about
## it for now
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
 
                  