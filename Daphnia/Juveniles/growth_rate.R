## growth rate

library(tidyverse)
source("../transfer_functions.R")
source("../Graphing_Set_Up.R")
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
library(shinystan)


#daph_grow_list <- list(
 # "N" = 64,
  #"chl" = daph_growth_j$chl,
  #"days_to_adult" = (daph_growth_j$days_to_adult)
#)

##
#fit <- stan(file = "growth.stan", 
 #           data = daph_grow_list,
  #          control = list(adapt_delta = 0.99, max_treedepth = 17),
   #         iter = 5000)  # mix looks good but lots of divergent transitions-- 
  
#launch_shinystan(fit)

#saveRDS(fit, file = "growth.rds")

## instead lets do exponential bc does look pretty even across food groups and will be easier to interpret

survcurve <- function(x) {
  x <- c(0,sort(x))
  tibble(day=x,frac_surv=seq(1,0,length.out=length(x)))
}

daph_growth_curves <- daph_growth_j %>%
  group_by(treatment) %>%
  do(survcurve(.$days_to_adult)) %>%
  mutate(adults = 1-frac_surv)



daph_grow_list1 <- list(
  "N" = nrow(daph_growth_curves),
  "days" = daph_growth_curves$day,
  "survival" = (daph_growth_curves$frac_surv)
)

fit <- stan(file = "adult_death.stan", 
            data = daph_grow_list1) 

launch_shinystan(fit)


fit_sum <- summary(fit)
print(names(fit_sum))

t <- rstan::extract
fit_sum_param <- fit_sum$summary[c(1:4),]

t <- rstan::extract(fit,permuted = FALSE)
b_pred <- (rbind(t[,1,1],t[,2,1], t[,3,1], t[,4,1])) ##


newdat <- data.frame(day = seq(0,7, by = 0.1))

pred_out <- apply(newdat,1,expon,b=b_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))


lower <- data.frame(day = seq(0,7, by = 0.1), adults = 1- pred_sum[1,])
upper <- data.frame(day = seq(0,7, by = 0.1), adults = 1- pred_sum[3,])
med <- data.frame(day = seq(0,7, by = 0.1), adults =1- pred_sum[2,])

ggplot(daph_growth_curves, aes(day, adults)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower, linetype = "dotdash", lwd = 1.25) + 
  geom_line(data = upper, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med, linetype = "solid", lwd =1.25) + xlab("Day") + 
  ylab("Proportion Adult")+
  theme_bw() + theme(axis.text.x = element_text(size = 30),
                     axis.text.y = element_text(size = 32),
                     axis.title.x = element_text(size = 30),
                     axis.title.y = element_text(size = 32),
                     strip.text = element_text(size = 0),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     strip.background = element_blank()) 



