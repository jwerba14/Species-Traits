## fit exponential death with stan 
library(tidyverse)
library(gridExtra)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(shinystan)
source("../transfer_functions.R")
daph <- read.csv("daphnia_lifetime.csv")
dat <- read.csv("survival_literature.csv")

# filter out individuals that were NOT born in the conditions (original daphnia)
daph <- daph %>%
  filter(adult_only=="N")

dA <-  daph %>%
  filter(size_class == "A")

daph_adult_death <-  dA %>%
  group_by(rep, treatment) %>%
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
nrow(daph_surv_curves)
## add in chl
ll <- daph_adult_death %>% dplyr::select(rep, chl)
fdat <- left_join(daph_surv_curves,ll)
nrow(fdat)



daph_death_list <- list(
  "N" = 74,
  "days" = daph_surv_curves$day,
  "survival" = daph_surv_curves$frac_surv
)

if(!file.exists("RDS_Files/fit.death.wide.rds")) {
fit <- stan(file = "adult_death.stan", 
            data = daph_death_list) #,
              
saveRDS(fit, file = "RDS_Files/fit.death.wide.rds")
} else {
  fit <- readRDS("RDS_Files/fit.death.wide.rds")
}

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


 
lower <- data.frame(day = seq(0,60), frac_surv = pred_sum[1,])
upper <- data.frame(day = seq(0,60), frac_surv = pred_sum[3,])
med <- data.frame(day = seq(0,60), frac_surv= pred_sum[2,])

wide_g <- ggplot(daph_surv_curves, aes(day, frac_surv)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med, linetype = "solid", lwd =1.25) + 
  xlab("Day") + ylab("Proportion Surviving")+ ggtitle("Wide Priors")+
  theme_bw() + theme(axis.text.x = element_text(size = 30),
                     axis.text.y = element_text(size = 32),
                     axis.title.x = element_text(size = 30),
                     axis.title.y = element_text(size = 32),
                     strip.text = element_text(size = 0),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     strip.background = element_blank()) 


print(wide_g)


##it only -- weighted average-- because literature values already parameter of interest
dat_lit <- dat %>% filter(sd != "NA")
b1 <- mean(dat$daphnia_survival)
b2 <- weighted.mean(dat_lit$daphnia_survival, dat_lit$sd)
mean(dat_lit$daphnia_survival)
b3 <- weighted.mean(dat$daphnia_survival, dat$Replicates)

predb1 <-apply(newdat,1,expon,b=b1)
predb2 <-apply(newdat,1,expon,b=b2)
predb3 <-apply(newdat,1,expon,b=b3)

dd <- data.frame(average = predb1,
                 weighted_sd = predb2,
                 weighted_replicate = predb3,
                 day = newdat$days)

dd1 <- dd %>% pivot_longer(c(average,weighted_sd,weighted_replicate), names_to = "type", values_to = "frac_surv")

lit_g <- ggplot(daph_surv_curves, aes(day, frac_surv)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = dd1, aes(color=type, linetype=type), size = 3)+
  xlab("Day") + ylab("Proportion Surviving")+ ggtitle("Literature Only") +
  theme_bw() + theme(axis.text.x = element_text(size = 30),
                     axis.text.y = element_text(size = 32),
                     axis.title.x = element_text(size = 30),
                     axis.title.y = element_text(size = 32),
                     strip.text = element_text(size = 0),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     strip.background = element_blank()) 

print(lit_g)



## wide prior

library(fitdistrplus)
d <- fitdistr(dat$daphnia_survival, "normal" )

set.seed(100)
h <- hist(dat$daphnia_survival) 
xfit <- seq(0,100)
yfit <- rnorm(xfit,d$estimate[1], d$estimate[2] )

ggplot(data = dat, aes(x=daphnia_survival)) + geom_histogram(aes(y=..density..)) +
  geom_density(data = data.frame(daphnia_survival=yfit))


daph_death_list <- list(
  "N" = 74,
  "days" = daph_surv_curves$day,
  "survival" = daph_surv_curves$frac_surv
)

if(!file.exists("RDS_Files/death.fit.inform.RDS")){
  fit_inf <- stan(file = "death_informedprior.stan", 
                  data = daph_death_list)
  saveRDS(fit_inf, file = "RDS_Files/death.fit.inform.RDS")
}else {
  fit_inf <- readRDS("RDS_Files/death.fit.inform.RDS")
}



launch_shinystan(fit_inf)

fit_sum_inf <- summary(fit_inf)
fit_sum_param_inf <- fit_sum_inf$summary[c(1:4),]


t_inf <- rstan::extract(fit_inf,permuted = FALSE)
b_pred_inf <- rbind(t_inf[,1,1],t_inf[,2,1],t_inf[,3,1],t_inf[,4,1]) 

newdat <- data.frame(days = seq(0,60))

pred_out_inf <- apply(newdat,1,expon,b=b_pred_inf)
pred_sum_inf <- apply(pred_out_inf, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))



lower_inf <- data.frame(day = seq(0,60), frac_surv = pred_sum_inf[1,])
upper_inf <- data.frame(day = seq(0,60), frac_surv = pred_sum_inf[3,])
med_inf <- data.frame(day = seq(0,60), frac_surv= pred_sum_inf[2,])









inf_g <- ggplot(daph_surv_curves, aes(day, frac_surv)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_inf, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_inf, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_inf, linetype = "solid", lwd =1.25) + 
  xlab("Day") + ylab("Proportion Surviving")+ ggtitle("Informed Prior") +
  theme_bw() + theme(axis.text.x = element_text(size = 30),
                     axis.text.y = element_text(size = 32),
                     axis.title.x = element_text(size = 30),
                     axis.title.y = element_text(size = 32),
                     strip.text = element_text(size = 0),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     strip.background = element_blank()) 

print(inf_g)



grid.arrange(lit_g, wide_g, inf_g)
