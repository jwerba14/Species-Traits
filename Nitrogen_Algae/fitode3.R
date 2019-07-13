## trying again to get diff eqs correct

library(tidyverse)
library(fitode)
library(corrplot)
source("../transfer_functions.R")
dat <- read.csv("Algae_Nutrient.csv") 

## look at a single treatment for Nh4 ## patterns are more obvious when just looking at single treatment 
##but doesn't seem to help with fit

dat_nit_27 <- dat %>%
  filter(treat == 27)

dat_nit_9 <- dat %>%
  filter(treat == 9)

dat_nit_3 <- dat %>%
  filter(treat == 3)

dat_nit_108 <- dat %>%
  filter(treat == 108)

dat_nit_54 <- dat %>%
  filter(treat == 54)

dat_nit_0.5 <- dat %>%
  filter(treat == 0.5)

cammonium = (1-9.4235e-01) # proportional ammonium lost to env-- calc in nutrient_air.R

chl_nh4_mod <- new("model.ode",
                   name = "algal_nit",
                   model = list(
                     pred_nh4 ~ -pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) + gamma *(death1*pred_chl + death2*(pred_chl^2))-cammonium*pred_nh4 ,
                     pred_chl ~ beta * pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) - death1*pred_chl - death2*(pred_chl^2)
                   ),
                   ## consider using bbmle::dnorm_n ?
                   observation = list(
                     nh4 ~ dnorm2(mean = pred_nh4),
                     chl ~ dnorm2(mean = pred_chl)
                   ),
                   initial = list(pred_nh4 ~ pred_nh40 , pred_chl ~ pred_chl0),
                   par=c("alpha", "beta", "omega", "death1","death2", "pred_nh40", "pred_chl0", "gamma")
)

## maybe figure out initial values
start <- c(alpha = 0.03, 
           beta = 15,
           omega=2.3,
           death1=0.006,
           death2=0.001,
           pred_nh40 = 15 ,
           pred_chl0 = 40, 
           gamma=0.01
           )

ss <- ode.solve(chl_nh4_mod, 1:11, start,
                solver.opts=list(method="rk4", hini=0.1))

plot(dat_nit_27$date1, dat_nit_27$nh4, ylim=c(0, 30))
lines(ss@solution$pred_nh4)

plot(dat_nit_27$date1, dat_nit_27$chl)
lines(ss@solution$pred_chl)



## fit with a bunch of starting parameter values
## but this is way too many but does encompass whole range
#start_dat <- expand.grid(
  #alpha =  seq(1e^-5,1, 3e^-5),
  #beta = seq(1,300000,500),
  #omega = seq(0.001, 700, .1),
  #death1 = seq(0,0.3,0.01),
  #death2 = seq(0,0.002,0.0001),
  #pred_nh40 = 13,
  #pred_chl0 = 43,
  #gamma = seq(0, 1.5, 0.0008),
  #sd1 =  seq(0,8,0.5) ,    
  #sd2 =  seq(20,80,5) 
#)




chl_fit_27_dd <- fitode(
  chl_nh4_mod,
  data = dat_nit_27, 
  start=start,
  tcol = "date1",
  solver.opts=list(method="rk4", hini=0.1)
)

plot(chl_fit_27_dd, level=0.95)
coef(chl_fit_27_dd)

library("psych")

v = vcov(chl_fit_27_dd)
cc1 = (cov2cor(v))
cc2 = cor.smooth(cc1)


## alpha - NH4 consumption per algae per time: 0.05
## beta - algae per NH4: 13
## K - half max: 6.5
## d - death rate per time: 0.03
## gamma - nh4 release per chl: 0.1
## pred_chl_d - scale for density dependent death: 162

start2 <- coef(chl_fit_27_dd)
start2[["pred_nh40"]] <- 6
start2[["pred_chl0"]] <- 50

## 
chl_fit_9_dd <- fitode(
  chl_nh4_mod,
  data = dat_nit_9,
  start=start2,
  tcol = "date1",
  solver.opts=list(method="rk4", hini=0.1)
  #method="Nelder-Mead"
)
plot(chl_fit_9_dd, level=0.95)

coef(chl_fit_27_dd)
coef(chl_fit_9_dd)

v= vcov(chl_fit_9_dd,"fitted")
cm = cov2cor(v)
cm1 = cor.smooth(cm)
corrplot(cm1)
#start3 <- coef(chl_fit_27_dd)
#start3[["pred_nh40"]] <- 3
#start3[["pred_chl0"]] <- 40

##not bad

start3 <- c(alpha = 0.003, 
            beta = 15,
            omega=400,
            death1=0.001,
            death2=0.00003,
            pred_nh40 = 3.5 ,
            pred_chl0 = 40, 
            gamma=1,
            sd1 = .2,
            sd2 = 30)


ss <- ode.solve(chl_nh4_mod, 1:11, start3,
                solver.opts=list(method="rk4", hini=0.1))

plot(dat_nit_3$date1, dat_nit_3$nh4, ylim=c(0, 7))
lines(ss@solution$pred_nh4)

plot(dat_nit_3$date1, dat_nit_3$chl, ylim=c(0, 350))
lines(ss@solution$pred_chl)


chl_fit_3_dd <- fitode(
  chl_nh4_mod,
  data = dat_nit_3,
  start=start3,
  tcol = "date1",
  solver.opts=list(method="rk4", hini=0.1)
  #method="Nelder-Mead"
)
plot(chl_fit_3_dd, level=0.95)

coef(chl_fit_9_dd)
coef(chl_fit_3_dd)
v3 = vcov(chl_fit_3_dd)
cc3 = cov2cor(v3)
cc3a = cor.smooth(cc3)

start4 <- coef(chl_fit_27_dd)
start4[["pred_nh40"]] <- 22
start4[["pred_chl0"]] <- 40

chl_fit_54 <- fitode(
  chl_nh4_mod,
  data = dat_nit_54,
  start=start4,
  tcol = "date1" ,
  solver.opts=list(method="rk4", hini=0.1)
  #method="Nelder-Mead"
)
plot(chl_fit_54, level=0.95)

coef(chl_fit_27_dd)
coef(chl_fit_54)

start5 <- coef(chl_fit_54)
start5[["pred_nh40"]] <- 40
start5[["pred_chl0"]] <- 40

## warning messages...

chl_fit_108 <- fitode(
  chl_nh4_mod,
  data = dat_nit_108,
  start=start5,
  tcol = "date1",
  solver.opts=list(method="rk4", hini=0.1))
#method="Nelder-Mead"

plot(chl_fit_108,level = 0.95)

start6 <- coef(chl_fit_27_dd)
start6[["pred_nh40"]] <- 2.5
start6[["pred_chl0"]] <- 40

#### this isn't great-- will also fit with 54 starting starting values but that has worse fit 
chl_fit_0.5 <- fitode(
  chl_nh4_mod,
  data = dat_nit_0.5,
  start=start6,
  tcol = "date1",
  solver.opts=list(method="rk4", hini=0.1)
  
)
plot(chl_fit_0.5, level = 0.95)

## remove all data points whose nh4 >4 bc I think those were first of day measurements and YSI doesn't work-- need TO CHECK!!!

chl_fit_0.5a <- fitode(
  chl_nh4_mod,
  data = dat_nit_0.5[dat_nit_0.5$nh4 < 4,],
  start=start6,
  tcol = "date1" ,
  solver.opts=list(method="rk4", hini=0.1)
)

plot(chl_fit_0.5a, level = 0.95)

## make a dataframe of all parameters

treat0.5 <- data.frame(confint(chl_fit_0.5a))
treat3 <- data.frame(confint(chl_fit_3_dd))
treat9 <- data.frame(confint(chl_fit_9_dd))
treat27 <- data.frame(confint(chl_fit_27_dd))
treat54 <- data.frame(confint(chl_fit_54))
treat108 <- data.frame(confint(chl_fit_108))

all_param <- data.frame(
  model =  rep(c("chl_fit_0.5", "chl_fit_3","chl_fit_9","chl_fit_27","chl_fit_54","chl_fit_108"), each=8),
  parameter = rep(names(start),6),
  estimate = c(treat0.5$estimate,treat3$estimate,treat9$estimate,treat27$estimate,treat54$estimate,treat108$estimate),
  lowcon = c(treat0.5$X2.5..,treat3$X2.5..,treat9$X2.5..,treat27$X2.5..,treat54$X2.5..,treat108$X2.5..),
  uppcon =  c(treat0.5$X97.5..,treat3$X97.5..,treat9$X97.5..,treat27$X97.5..,treat54$X97.5..,treat108$X97.5..)
  
)
all_param$model <- factor(all_param$model, levels = c("chl_fit_0.5", "chl_fit_3","chl_fit_9","chl_fit_27","chl_fit_54","chl_fit_108"))

filter_param <- all_param %>%
  filter(!(parameter %in% c("pred_nh40", "pred_chl0", "sd1", "sd2"))) 

ggplot(filter_param, aes(model,estimate)) +
  geom_point() + 
  #geom_errorbar(aes(model, ymin=lowcon, ymax=uppcon)) +
  #scale_y_log10() +
  facet_wrap(~parameter, scale="free_y")

par_high <- filter_param %>% filter( model != "chl_fit_0.5")
ggplot(par_high, aes(model,estimate)) +
  geom_point() + 
  #geom_errorbar(aes(model, ymin=lowcon, ymax=uppcon)) +
  scale_y_log10() +
  facet_wrap(~parameter, scale="free_y")



## remove really high estimates
par2 <- filter_param %>% filter(estimate < 1000)

ggplot(par2, aes(model,estimate)) +
  geom_point() + 
  #geom_errorbar(aes(model, ymin=lowcon, ymax=uppcon)) +
  #scale_y_log10() +
  facet_wrap(~parameter, scale="free_y")




param_avg <- all_param %>% group_by(parameter) %>% summarise(med = median(estimate), avg = mean(estimate))


##### 
start_new <- param_avg %>% select(parameter,med)
newstart <- setNames(start_new$med, as.character(start_new$parameter))
#start <- coef(chl_fit_108)
ss <- ode.solve(chl_nh4_mod, 1:11, newstart,
                solver.opts=list(method="rk4", hini=0.1))

plot(dat_nit_27$date1, dat_nit_27$nh4, ylim=c(0, 30))
lines(ss@solution$pred_nh4)

plot(dat$date1, dat$chl, ylim=c(0, 600))
lines(ss@solution$pred_chl)
plot(dat$date1, dat$nh4, ylim=c(0, 100))
lines(ss@solution$pred_nh4)




## what if try to fit all at once
##probbly not a good move

all_mod_fit <- fitode(
  chl_nh4_mod,
  data = dat,
  start=newstart,
  tcol = "date1",
  solver.opts=list(method="rk4", hini=0.1)
  #method="Nelder-Mead"
)
plot(all_mod_fit, level = 0.95)
