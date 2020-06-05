## simulation for ode
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

sim_mod <- stan_model(file = "sim_mod.stan", model_name = "sim_mod", verbose = T)


sim_out <- sampling(sim_mod, seed = 2, data = list(N = nrow(dat_27),
                                                 y = dat_27[c(8,5)],
                                                 t0 = 0,
                                                 t_obs= seq(1,11),
                                                 run_estimation = 0),chains=1,iter=4000,
                    control = list(adapt_delta = 0.99, max_treedepth =18))

y_sim <- rstan::extract(sim_out, pars = "y_hat_n")
y_sim <- data.frame(y_sim)
true_param <- rstan::extract(sim_out)
fit_sum_sim<- summary(sim_out)
fsss<-data.frame(fit_sum_sim$summary[c(1:9),])
fsss1 <- fsss %>% mutate(param = rownames(fsss)) %>% gather(-c(n_eff, Rhat, param), key = "quantile", value = "value")
fsss1 <- fsss1 %>% filter(quantile != "mean" & quantile != "se_mean" & quantile != "sd") %>% mutate(trial = "sim")

fsss_med <- fsss1 %>% filter(quantile == "X50.")
fsss_upr <- fsss1 %>% filter(quantile == "X75.")
fsss_lwr <- fsss1 %>% filter(quantile == "X25.")

simdat <- apply(y_sim$y_hat_n, 2:3,median)

dgp_recapture_fit <- sampling(sim_mod, data = list(N = nrow(simdat),
                                                   y = simdat,
                                                   t0 = 0,
                                                   t_obs= seq(1,11),
                                                   run_estimation = 1),chains=1,
                              control = list(adapt_delta = 0.99, max_treedepth =15))

fit_sum_check<- summary(dgp_recapture_fit)
fsso<-data.frame(fit_sum_check$summary[c(1:9),])
fsso1 <- fsso %>% mutate(param = rownames(fsso)) %>% gather(-c(n_eff, Rhat, param), key = "quantile", value = "value")
fsso1 <- fsso1 %>% filter(quantile != "mean" & quantile != "se_mean" & quantile != "sd") %>% mutate(trial = "recap")


ndat <- rbind(fsso1,fsss1)
ndat <- ndat %>% filter(param != "y0[1]" & param != "y0[2]" & param != "sigma[1]" & param != "sigma[2]")

ggplot(ndat, aes(value, param)) + geom_point(aes(color=quantile, shape = trial)) 
