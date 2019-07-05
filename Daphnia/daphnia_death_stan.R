## fit exponential death with stan 
library(tidyverse)
source("../transfer_functions.R")
daph <- read.csv("daphnia_lifetime.csv")
# filter out individuals that were NOT born in the conditions (original daphnia)
daph <- daph %>%
  filter(adult_only=="N")

dA <-  daph %>%
  filter(size_class == "A")

daph_adult_death <-  dA %>%
  group_by(rep,treatment) %>%
  summarize(days_adult = n(),
            chl = mean(chl_avg),
            chl_sd_rep = sd(chl_avg)) 

survcurve <- function(x) {
  x <- c(0,sort(x))
  tibble(day=x,frac_surv=seq(1,0,length.out=length(x)))
}

daph_surv_curves <- daph_adult_death %>%
  group_by(treatment) %>%
  do(survcurve(.$days_adult))

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(shinystan)
daph_death_list <- list(
  "N" = 74,
  "days" = daph_surv_curves$day,
  "survival" = daph_surv_curves$frac_surv
)


fit <- stan(file = "adult_death.stan", 
            data = daph_death_list,
            control = list(adapt_delta = 0.95,
                           max_treedepth = 12))  

launch_shinystan(fit)
## for ODE 1/beta is the rate- beta is the number of days until 1/e are lost
fit_sum <- summary(fit)
print(names(fit_sum))
print(fit_sum$summary)
fit_sum_param <- fit_sum$summary[c(1:4),]


t <- rstan::extract(fit,permuted = FALSE)
b_pred <- rbind(t[,1,1],t[,2,1],t[,3,1],t[,4,1]) 

newdat <- data.frame(days = seq(0,60))

pred_out <- apply(newdat,1,expon,b=b_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

with(daph_surv_curves, plot(day, frac_surv))
lines(seq(0,60), pred_sum[1,])
lines(seq(0,60), pred_sum[2,])
lines(seq(0,60), pred_sum[3,])
 


