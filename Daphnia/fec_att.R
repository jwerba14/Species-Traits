library(tidyverse)

daph <- read.csv("daphnia_lifetime.csv")
daph <- daph %>%
  filter(adult_only=="N")

## to get fecundity parameter fit saturating curve (params z and w in full ode)
## need to make per day so need to divide total fecundity by # of days that individual was an adult
daph_fec <- daph %>% group_by(rep, treatment) %>%
  filter(size_class == "A") %>%
  summarize(
    time_adult = n()
    , life_fec   = sum(clutch_size, na.rm = TRUE)
    , chl        = mean(chl_avg)
    , chl_sd_rep = sd(chl_avg)) %>%
  mutate(daily_fec = life_fec / time_adult)


library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

daph_fec_r <- daph_fec %>% select(rep, chl, daily_fec)
daph_fec_r$rep <- seq(1,70)
daph_fec_r <- data.frame(daph_fec_r)

daph_fec_list <- list(
   "N" = 70,
   "chl" = daph_fec_r$chl,
   "daily_fec" = daph_fec_r$daily_fec
)

fit <- stan(file = "fec_stan.stan", 
            data = daph_fec_list,
            control = list(adapt_delta = 0.95))


t <- rstan::extract(fit,permuted = FALSE)
fit_sum <- summary(fit)
print(names(fit_sum))
print(fit_sum$summary)
fit_sum_param <- fit_sum$summary[c(1:4),]

tidy_pred <- tidybayes::add_fitted_draws(fit, newdata = data.frame(chl = seq(1,75)))

## right now only 1 chain want all 4 eventually 
a_pred <- rbind(t[,1,1],t[,2,1]) ## all rows, first chain, alpha
b_pred <- rbind(t[,1,2], t[,2,2])

newdat <- data.frame(chl = seq(0,75))

pred_out <- apply(newdat,1,sat_fun,a=a_pred,b=b_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

with(daph_fec_r, plot(chl, daily_fec))
lines(seq(0,75), pred_sum[1,])
lines(seq(0,75), pred_sum[2,])
lines(seq(0,75), pred_sum[3,])

hist(t[,1,4], breaks = 200)

