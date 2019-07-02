## growth rate

library(tidyverse)
source("../transfer_functions.R")
daph <- read.csv("daphnia_lifetime.csv")
daph <- daph %>%
  filter(adult_only=="N")
###growth to adult
daph_growth <- daph %>%
  group_by(rep,treatment) %>%
  filter(sum(size_class == "A") != 0)

daph_growth_j <- daph_growth %>%
  filter(size_class=="J") %>%
  group_by(treatment, rep) %>%
  summarize(days_to_adult = n(), chl = mean(chl_avg), chl_sd = sd(chl_avg) )

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



daph_grow_list <- list(
  "N" = 64,
  "chl" = daph_growth_j$chl,
  "days_to_adult" = (daph_growth_j$days_to_adult)
)

##fit with sigmoidal to force through zero
fit <- stan(file = "growth.stan", 
            data = daph_grow_list,
            control = list(adapt_delta = 0.99, max_treedepth = 17),
            iter = 5000)
library(shinystan)
launch_shinystan(fit)


fit_sum <- summary(fit)
print(names(fit_sum))
print(fit_sum$summary)
fit_sum_param <- fit_sum$summary[c(1:4),]
plot(t[,4,1])
t <- rstan::extract(fit,permuted = FALSE)
a_pred <- exp(rbind(t[,1,1],t[,2,1], t[,3,1], t[,4,1])) ## all rows, first chain, alpha
b_pred <- (rbind(t[,1,2] , t[,2,2], t[,3,2], t[,4,2]))


newdat <- data.frame(chl = seq(0,64))

pred_out <- apply(newdat,1,sat_fun,a=a_pred,b=b_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975), na.rm = T))

with(daph_growth_j, plot(chl, 1/days_to_adult))
lines(seq(0,64), 1/pred_sum[1,])
lines(seq(0,64), 1/pred_sum[2,])
lines(seq(0,64), 1/pred_sum[3,])



