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


fit_sum <- summary(fit)
print(names(fit_sum))
print(fit_sum$summary)
fit_sum_param <- fit_sum$summary[c(1:4),]

tidy_pred <- tidybayes::add_fitted_draws(fit, newdata = data.frame(chl = seq(1,75)))

## manual and a bit wrong
a_pred <- rnorm(1000, fit_sum_param[1,1], fit_sum_param[1,3])
b_pred <- rnorm(1000, fit_sum_param[2,1], fit_sum_param[2,3])

newdat <- data.frame(chl = seq(0,75))

pred_out <- apply(newdat,1,sat_fun,a=a_pred,b=b_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

with(daph_fec_r, plot(chl, daily_fec))
lines(seq(0,75), pred_sum[1,])
lines(seq(0,75), pred_sum[2,])
lines(seq(0,75), pred_sum[3,])

ggplot(daph_fec_r, aes(chl, daily_fec)) + geom_point() + geom_line(data = newdat, aes(chl, daily_fec), color = "blue")



launch_shinystan(fit)

ext_pred <- extract(fit)

library(bayesplot)
library(tidybayes)
fit %>%
  spread_draws(Y_mean) %>%
  head(10)



out_mat <- matrix(NA, nrow = dim(ext_pred$Y_pred)[2],
                  ncol = dim(ext_pred$Y_pred)[3])
for(i in 1:dim(ext_pred$y_test)[2]) {
  for(j in 1:dim(ext_pred$y_test)[3]) {
    out_mat[i, j] <- mean(ext_pred$y_test[, i, j])
  }
}

plot(ext_pred$Y_pred)
library(bayesplot)
posterior <- as.matrix(fit)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("alpha", "beta"),
           prob = 0.8) + plot_title


Y_mean <- extract(fit, "Y_mean")
Y_mean_cred <- apply(Y_mean$Y_mean, 2, quantile, c(0.05, 0.95))
Y_mean_mean <- apply(Y_mean$Y_mean, 2, mean)
Y_mean_dat <- data.frame(
  chl = rep(daph_fec_r$chl , 3)
, fec = Y_mean_mean
, fec_low  = Y_mean_cred[1, ]
, fec_high = Y_mean_cred[2, ]
)
Y_mean_dat <- Y_mean_dat %>% group_by(treat) %>% summarize()


Y_pred <- extract(fit, "Y_pred")
Y_pred_cred <- apply(Y_pred$Y_pred, 2, quantile, c(0.05, 0.95))
Y_pred_mean <- apply(Y_pred$Y_pred, 2, mean)

plot(daph_fec_r$daily_fec ~ daph_fec_r$chl, xlab="chl", ylab="fec", main="Non-linear fecundity")



lines(daph_fec_r$chl, Y_mean_mean)

newdat <- data.frame(
  daily_fec = Y_pred_mean,
  chl = daph_fec_r$chl
)
ggplot(newdat, aes(chl, daily_fec)) + geom_point()

ggplot(daph_fec_r, aes(chl,daily_fec)) + geom_point()  + geom_point(newdat,aes(chl,daily_fec,color = "blue"))
library(tidybayes)

 points(daph_fec_r$chl, Y_pred_mean, pch=19)
lines(daph_fec_r$chl, Y_mean_cred[1,], col=4)
lines(daph_fec_r$chl, Y_mean_cred[2,], col=4)
lines(daph_fec_r$chl, Y_pred_cred[1,], col=2)
lines(daph_fec_r$chl, Y_pred_cred[2,], col=2)
legend(x="bottomright", bty="n", lwd=2, lty=c(NA, NA, 1, 1,1),
       legend=c("observation", "prediction", "mean prediction",
                "90% mean cred. interval", "90% pred. cred. interval"),
       col=c(1,1,1,4,2),  pch=c(1, 19, NA, NA, NA))


