## adult daphnia feeding and excretion

source("../transfer_functions.R")
library(tidyverse)

rdat <- read.csv("Daphnia_large_Feeding_Nov11.csv")

## get average change in controls by treatment


cont <- rdat %>% 
  mutate(Chl_Time_Diff_day = (Chl_Time_Diff/60)/24,Nh4_Time_Diff_day = (Nh4_Time_Diff/60)/24) %>%
  filter(control == 2) %>% 
  mutate(chl_diff =(chl1-chl2)/Chl_Time_Diff_day, nh4_diff= (nh42-nh41)/Nh4_Time_Diff_day) %>% # subtract 1 from 2 for change over time
  group_by(Treatment) %>%
  summarize(mean_chl = mean(chl_diff, na.rm = T), mean_nh4 = mean(nh4_diff,na.rm = T)) ## onr row in treatment 3 is all NAs..??


## add mean control avg back to main dataframe
dat <- left_join(rdat,cont)

## account for controls

dat <- dat %>%
  filter(control == 1) %>%
  mutate(Chl_Time_Diff_day = (Chl_Time_Diff/60)/24,Nh4_Time_Diff_day = (Nh4_Time_Diff/60)/24) %>%
  mutate(chl_diff = (chl1-chl2)/Chl_Time_Diff_day, nh4_diff = (nh41-nh42)/Nh4_Time_Diff_day) %>%
  mutate(chl_diff_cc = (chl_diff-mean_chl)/Num_Daphnia, nh4_diff_cc = (nh4_diff-mean_nh4)/Num_Daphnia)

dat1 <- dat %>% filter(control == 1) %>% filter(!is.na(chl_diff_cc)) %>% ## 1 entry is NA
  select(Rep, Treatment,chl1,nh41, chl_diff_cc,nh4_diff_cc, Num_Daphnia)

ggplot(dat1, aes(chl1,chl_diff_cc)) + geom_point()


mod <- nls(data = dat1, chl_diff_cc ~ (chl1*h)/(1+chl1*h*r), start = list(h=1,r=1))

library(nlmrt)
mod_sig <- nlxb(data=dat1, chl_diff_cc ~ (a*chl1^b)/(c+chl1^b), start = list(a=1,b=0.1,c=1))
mod_sat <- nlxb(data = dat1, chl_diff_cc ~ (chl1*a)/(chl1+b), start = list(a=1,b=1))
mod_lm <- lm(data = dat1, chl_diff_cc ~ chl1)

newpred <- sig_fun(k= seq(1,100,1), a = 0.154319 , b = 0.861941, c = 83.8371)
plot(seq(1,100,1), newpred)
points(dat1$chl1,dat1$chl_diff_cc)

newdata = data.frame(chl1 = seq(1,100,1))

newpred <- sat_fun(k= seq(1,100,1), a=960892 ,b =1339121459)
newpred1 <- predict(mod_lm, newdata = newdata )
plot(seq(1,100,1), newpred)
points(seq(1,100,1),newpred1)
points(dat1$chl1,dat1$chl_diff_cc)
## linear line exactly the same as saturating, looks like good fit, range of chl1 close to final experiment
## so keep linear ie type I

#######  fit in stan
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(shinystan)


daph_grow_list <- list(
  "N" = nrow(dat1),
  "chl" = dat1$chl1,
  "diff" = dat1$chl_diff_cc
)


fit <- stan(file = "adult_feeding.stan", 
            data = daph_grow_list, control = list(max_treedepth = 12))

launch_shinystan(fit)

fit_sum <- summary(fit)
(fit_sum_param <- fit_sum$summary[c(1:4),])
saveRDS(fit, file = "adult_feeding.RDS")

t <- rstan::extract(fit,permuted = FALSE)
m_pred <- rbind(t[,1,1],t[,2,1],t[,3,1],t[,4,1]) 
b_pred <- rbind(t[,1,2],t[,2,2],t[,3,2],t[,4,2]) 

newdat <- data.frame(chl1 = seq(0,100))

pred_out <- apply(newdat,1,lin,m= m_pred, b=b_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

with(dat1, plot(chl1, chl_diff_cc))
lines(seq(0,100), pred_sum[1,])
lines(seq(0,100), pred_sum[2,])
lines(seq(0,100), pred_sum[3,])


### ammonium excretion 
ggplot(dat1, aes(nh41,nh4_diff_cc)) + geom_point()+geom_smooth(method = "lm")
ggplot(dat1, aes(chl_diff_cc,nh4_diff_cc)) + geom_point()+ geom_smooth(method = "lm")

lm(data = dat1, nh4_diff_cc ~chl_diff_cc)



mod_nh4 <- lm(data = dat1, nh4_diff_cc ~ nh41)
mod_int_only <- lm(data = dat1, nh4_diff_cc ~1)
mod_sat <- nlxb(data = dat1, nh4_diff_cc ~ (nh41*a)/(nh41+b), start = list(a=1,b=1))

newpred <- sat_fun(k= seq(5,22,.1), a=1667 ,b =9905303)


newdata = data.frame(nh41 = seq(5,22,0.1))
newpred2 <- predict(mod_nh4, newdata = newdata)
newpred3 <- predict(mod_int_only, newdata = newdata)
plot(seq(5,22,0.1), newpred2)
points(dat1$nh41,dat1$nh4_diff_cc)
points(seq(5,22,0.1), newpred3)
points(seq(5,22,0.1), newpred)
### fit in stan

daph_excretion_list <- list(
  "N" = nrow(dat1),
  "nh4" = dat1$nh41,
  "diff" = dat1$nh4_diff_cc
)


fit <- stan(file = "adult_excretion.stan", 
            data = daph_excretion_list)

launch_shinystan(fit)

fit_sum <- summary(fit)
(fit_sum_param <- fit_sum$summary[c(1:4),])

t <- rstan::extract(fit,permuted = FALSE)
m_pred <- rbind(t[,1,1],t[,2,1],t[,3,1],t[,4,1]) 
b_pred <- rbind(t[,1,2],t[,2,2],t[,3,2],t[,4,2]) 

newdat <- data.frame(nh41 = seq(0,20))

pred_out <- apply(newdat,1,lin,m= m_pred, b=b_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

with(dat1, plot(nh41, nh4_diff_cc))
lines(seq(0,20), pred_sum[1,])
lines(seq(0,20), pred_sum[2,])
lines(seq(0,20), pred_sum[3,])

saveRDS(fit, file = "adult_exc.RDS")
fit2 <- readRDS("adult_exc.RDS")


daph_excretion_list <- list(
  "N" = nrow(dat1),
  "chl" = dat1$chl_diff_cc,
  "diff" = dat1$nh4_diff_cc
)


fit <- stan(file = "adult_excretion_update.stan", 
            data = daph_excretion_list)

launch_shinystan(fit)

fit_sum <- summary(fit)
(fit_sum_param <- fit_sum$summary[c(1:4),])

t <- rstan::extract(fit,permuted = FALSE)
m_pred <- rbind(t[,1,1],t[,2,1],t[,3,1],t[,4,1]) 
b_pred <- rbind(t[,1,2],t[,2,2],t[,3,2],t[,4,2]) 

newdat <- data.frame(chl1 = seq(0,100))

pred_out <- apply(newdat,1,lin,m= m_pred, b=b_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

with(dat1, plot(chl_diff_cc, nh4_diff_cc))
lines(seq(0,20), pred_sum[1,])
lines(seq(0,20), pred_sum[2,])
lines(seq(0,20), pred_sum[3,])

saveRDS(fit, file = "adult_exc_new.RDS")
fit2 <- readRDS("adult_exc_new.RDS")



