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
saveRDS(fit, file = "adult_death.rds")

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
dev.off()
with(daph_surv_curves, plot(day, frac_surv))
lines(seq(0,60), pred_sum[1,])
lines(seq(0,60), pred_sum[2,])
lines(seq(0,60), pred_sum[3,])

 
lower <- data.frame(day = seq(0,60), frac_surv = pred_sum[1,])
upper <- data.frame(day = seq(0,60), frac_surv = pred_sum[3,])
med <- data.frame(day = seq(0,60), frac_surv= pred_sum[2,])

ggplot(daph_surv_curves, aes(day, frac_surv)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower, linetype = "dotdash", lwd = 1.25) + geom_line(data = upper, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med, linetype = "solid", lwd =1.25) + xlab("Day") + ylab("Proportion Surviving")+
  theme_bw() + theme(axis.text.x = element_text(size = 30),
                     axis.text.y = element_text(size = 32),
                     axis.title.x = element_text(size = 30),
                     axis.title.y = element_text(size = 32),
                     strip.text = element_text(size = 0),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     strip.background = element_blank()) 

