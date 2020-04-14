devtools::install_github("parksw3/boms")
library(rstan)
library(tidyr)
library(dplyr)
library(boms)

dat_nit_27 <- read.csv("JoData.csv")

dat_nit_27_mod <- dat %>%
  filter(treat == 27) %>%
  dplyr::select(date1, rep, chl, nh4) %>%
  gather(key, value, -date1, -rep) %>%
  mutate(
    delta=as.numeric(factor(key))-1
  )

jo_model <- make_model(
  grad = list(
    pred_nh4 ~ -pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) + gamma*(death1*pred_chl)-0.04085*pred_nh4,
    pred_chl ~ beta * pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) - death1*pred_chl
  ),
  observation = 
    value ~ (1-delta) * pred_chl + delta * pred_nh4,
  initial = list(
    pred_nh4 ~ 40,
    pred_chl ~ 15
  ),
  par= c("alpha", "beta", "omega","gamma","death1", "delta"),
  data=dat_nit_27_mod,
  tcol="date1",
  family="gaussian"
) 

jo_boms_code <- make_stancode_boms(
  jo_model,
  effect=list(
    alpha ~ 1,
    beta ~ 1,
    omega ~ 1,
    gamma ~ 1, 
    death1 ~ 1,
    sigma ~ -1 + key
  ),
  prior=list(
    alpha ~ exponential(0.1),
    beta ~ exponential(0.1),
    omega ~ exponential(0.1),
    gamma ~ exponential(0.1),
    death1 ~ exponential(0.1)
  )
)

jo_boms_data <- make_standata_boms(
  jo_model,
  effect=list(
    alpha ~ 1,
    beta ~ 1,
    omega ~ 1,
    gamma ~ 1,
    death1 ~ 1,
    sigma ~ -1 + key
  ),
  prior=list(
    alpha ~ exponential(0.1),
    beta ~ exponential(0.1),
    omega ~ exponential(0.1),
    gamma ~ exponential(0.1),
    death1 ~ exponential(0.1)
  )
)
jo_boms_data[[1]] <- NULL




sm <- rstan::stan_model(file = "jo_boms_inprior.stan")

samp <- rstan::sampling(sm, data = jo_boms_data, chains = 1, iter = 2000, refresh = 1)



library(bayesplot)
plot(samp)
saveRDS(samp, file = "samp1.RDS")
samp <- readRDS("samp1.RDS")

traceplot(samp, pars = c("b_alpha", "b_beta", "b_omega", "b_gamma"))

draws <- as.array(samp)
mcmc_trace(draws)
rt <- rstan::stanc(model_code=jo_boms_code)
sm <- rstan::stan_model(stanc_ret=rt)




oo <- rstan::optimizing(sm, data=jo_boms_data, seed=101, as_vector=FALSE,
                        iter=4000)

rstan::sampling(sm,iter = 2000, chains = 2)
