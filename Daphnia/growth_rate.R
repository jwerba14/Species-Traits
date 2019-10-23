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

##
fit <- stan(file = "growth.stan", 
            data = daph_grow_list,
            control = list(adapt_delta = 0.99, max_treedepth = 17),
            iter = 5000)  # mix looks good but lots of divergent transitions-- 
  library(shinystan)
launch_shinystan(fit)

saveRDS(fit, file = "growth.rds")

## instead lets do exponential bc does look pretty even across food groups and will be easier to interpret

survcurve <- function(x) {
  x <- c(0,sort(x))
  tibble(day=x,frac_surv=seq(1,0,length.out=length(x)))
}

daph_growth_curves <- daph_growth_j %>%
  group_by(treatment) %>%
  do(survcurve(.$days_to_adult))


daph_grow_list1 <- list(
  "N" = 68,
  "days" = daph_growth_curves$day,
  "survival" = (daph_growth_curves$frac_surv)
)

fit <- stan(file = "adult_death.stan", 
            data = daph_grow_list1,
            control = list(adapt_delta = 0.99, max_treedepth = 17),
            iter = 5000)

library(shinystan)
launch_shinystan(fit)


fit_sum <- summary(fit)
print(names(fit_sum))

t <- rstan::extract



fit_sum_param <- fit_sum$summary[c(1:4),]

t <- rstan::extract(fit,permuted = FALSE)
a_pred <- exp(rbind(t[,1,1],t[,2,1], t[,3,1], t[,4,1])) ## all rows, first chain, alpha
b_pred <- exp(rbind(t[,1,2] , t[,2,2], t[,3,2], t[,4,2]))


newdat <- data.frame(chl = seq(0,90, by = 0.1))

pred_out <- apply(newdat,1,sat_fun,a=a_pred,b=b_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975), na.rm = T))

with(daph_growth_j, plot(chl, 1/days_to_adult))
lines(seq(0,90,by=0.1), 1/pred_sum[1,])
lines(seq(0,90, by= 0.1), 1/pred_sum[2,])
lines(seq(0,90, by = 0.1), 1/pred_sum[3,])



