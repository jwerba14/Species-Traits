### ammonium excretion 
source("../transfer_functions.R")
source("../chl_adj.R")
source("../Graphing_Set_Up.R")
library(tidyverse)
library(nlmrt)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(shinystan)

lit <- read.csv("excretion_lit.csv")
##  == mg N/ Daphnia *day

rdat <- read.csv("Daphnia_large_Feeding_Nov11.csv")
dim(rdat)
## get average change in controls by treatment

## per hour
cont <- rdat %>% 
  mutate(Chl_Time_Diff_day = (Chl_Time_Diff/60),Nh4_Time_Diff_day = (Nh4_Time_Diff/60)) %>%
  filter(control == 2) %>% 
  mutate(chl_diff =(chl1-chl2)/Chl_Time_Diff_day, nh4_diff= (nh42-nh41)/Nh4_Time_Diff_day) %>% # subtract 1 from 2 for change over time
  group_by(Treatment) %>%
  summarize(mean_chl = mean(chl_diff, na.rm = T), mean_nh4 = mean(nh4_diff,na.rm = T)) ## onr row in treatment 3 is all NAs..??

dim(cont)
## add mean control avg back to main dataframe
dat <- left_join(rdat,cont)
dim(dat)
## account for controls

dat <- dat %>%
  filter(control == 1) %>%
  mutate(Chl_Time_Diff_day = (Chl_Time_Diff/60),Nh4_Time_Diff_day = (Nh4_Time_Diff/60)) %>%
  mutate(chl_diff = (chl1-chl2)/Chl_Time_Diff_day, nh4_diff = (nh41-nh42)/Nh4_Time_Diff_day) %>%
  mutate(chl_diff_cc = (chl_diff-mean_chl)/Num_Daphnia, nh4_diff_cc = (nh4_diff-mean_nh4)/Num_Daphnia)
dim(dat)

dat1 <- dat %>% filter(control == 1) %>% filter(!is.na(chl_diff_cc)) %>% ## 1 entry is NA
  dplyr::select(Rep, Treatment,chl1,nh41, chl_diff_cc,nh4_diff_cc, Num_Daphnia)
dim(dat1)


ggplot(dat1, aes(nh41,nh4_diff_cc)) + geom_point()+geom_smooth(method = "lm")
ggplot(dat1, aes(chl_diff_cc,nh4_diff_cc)) + geom_point()+ geom_smooth(method = "lm")


lm(data = dat1, nh4_diff_cc ~-1+chl_diff_cc)
## hmm but how would this work in the ODE? Thats confusing...


mod_nh4 <- lm(data = dat1, nh4_diff_cc ~ nh41)
mod_int_only <- lm(data = dat1, nh4_diff_cc ~1)
mod_sat <- nlxb(data = dat1, nh4_diff_cc ~ (nh41*a)/(nh41+b), start = list(a=1,b=1))
mod_lm <- lm(data = dat1, nh4_diff_cc ~-1+chl_diff_cc)

newpred <- sat_fun(k= seq(5,22,.1), a=1667 ,b =9905303)

newdata = data.frame(nh41 = seq(5,22,0.1))
newpred2 <- predict(mod_nh4, newdata = newdata)
newpred3 <- predict(mod_int_only, newdata = newdata)
plot(seq(5,22,0.1), newpred2)
points(dat1$nh41,dat1$nh4_diff_cc)
points(seq(5,22,0.1), newpred3)
points(seq(5,22,0.1), newpred)

newdata1 = data.frame(chl_diff_cc = seq(-0.02,0.75,0.001))
newpred5 <- data.frame(predict(mod_lm, newdata = newdata1, interval = "confidence"))
newdata1$nh4_diff_cc <- newpred5$fit
newdata1$upr <- newpred5$upr
newdata1$lwr <- newpred5$lwr
ls_g <- ggplot(dat1, aes(chl_diff_cc, nh4_diff_cc)) + geom_point() + geom_line(data = newdata1) +
  geom_ribbon(data = newdata1, aes(ymin=lwr, ymax=upr),alpha = 0.3) + ggtitle("LS") +
  xlab("Chl (ug/L) uptake") + ylab("Excretion N (mg/N)")
print(ls_g)


### fit in stan

daph_excretion_list <- list(
  "N" = nrow(dat1),
  "chl" = dat1$chl_diff_cc,
  "diff" = dat1$nh4_diff_cc
)
if(!file.exists("RDS_Files/exc.fit.wide.RDS")){
  
  fit <- stan(file = "adult_excretion.stan", 
              data = daph_excretion_list) 
  saveRDS(fit, file = "RDS_Files/exc.fit.wide.RDS")
} else {
  fit <- readRDS("RDS_Files/exc.fit.wide.RDS")
}



launch_shinystan(fit)

fit_sum <- summary(fit)
(fit_sum_param <- fit_sum$summary[c(1:4),])
t <- rstan::extract(fit,permuted = FALSE)
m_pred <- rbind(t[,1,1],t[,2,1],t[,3,1],t[,4,1]) 


newdat <- data.frame(chl_diff_cc = seq(0,0.75,0.01))

pred_out_wide <- apply(newdat,1,lin2,m=m_pred)
pred_sum_wide <- apply(pred_out_wide, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_wide <- data.frame(chl_diff_cc = seq(0,0.75,0.01), nh4_diff_cc = pred_sum_wide[1,])
upper_wide <- data.frame(chl_diff_cc = seq(0,0.75,0.01), nh4_diff_cc = pred_sum_wide[3,])
med_wide <- data.frame(chl_diff_cc = seq(0,0.75,0.01), nh4_diff_cc = pred_sum_wide[2,])

stan_wide_ex <- ggplot(dat1, aes(chl_diff_cc, nh4_diff_cc)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_wide, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_wide, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_wide, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
  ylab("Excretion N (mg/N) ") + ggtitle("Stan: Wide Priors")

print(stan_wide_ex)




## Need to go back through units and check which model is correct before moving onto mixed model 
#mg N/ daphnia*day
## and lit only model



## literature




## mixed model



## total models: ls,wideprior, lit only, mixed model, hyperparam


