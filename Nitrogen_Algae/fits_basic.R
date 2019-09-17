library(tidyverse)
library(fitode)
library(corrplot)

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

chl_nh4_mod3 <- odemodel(
  name = "algal_nit",
  model = list(
    pred_nh4 ~ -a*pred_chl*(pred_nh4/(k+pred_nh4))+r*death*pred_chl, #-cammonium,
    pred_chl ~ a*pred_chl*(pred_nh4/(k+pred_nh4)) - death*pred_chl  
  ),
  observation = list(
    nh4 ~ dlnorm(meanlog = log(pred_nh4), sdlog = 0.05),
    chl ~ dlnorm(meanlog = log(pred_chl), sdlog = 0.01)
  ),
  initial = list(pred_nh4 ~ pred_nh40 , pred_chl ~ pred_chl0),
  par=c("a","k", "r","death", "pred_nh40", "pred_chl0")
)



## fits

start <- c(a = 0.03, 
           k = .03,
           r = 1,
           death = 0.02,
           pred_nh40= 15,
           pred_chl0 = 40
)

chl_fit_27_dd <- fitode(
  chl_nh4_mod3,
  data = dat_nit_27, 
  start=start,
  tcol = "date1",
  solver.opts=list(method="rk4", hini=0.1)
)
plot(chl_fit_27_dd, level=0.95)
coef(chl_fit_27_dd)

newdat <- simulate(chl_nh4_mod3,nsim = 5, parms= coef(chl_fit_27_dd), times = seq(1,11),             
                   solver.opts=list(method="rk4", hini=0.1))

cc <- coef(chl_fit_27_dd)
cc["pred_nh40"] <- 15

nd1 <-simulate(chl_nh4_mod3,nsim = 5, parms= cc, times = seq(1,11),             
              solver.opts=list(method="rk4", hini=0.1))

nd1$date1 <- nd1$times

ggplot(dat_nit_27, aes(date1,chl)) + geom_point()+ geom_line(data = nd1)

ggplot(dat_nit_27, aes(date1,nh4)) + geom_point()+ geom_line(data = nd1)

newdat$date1 <- newdat$times



ggplot(dat_nit_27, aes(date1,chl)) + geom_point()+ geom_line(data = newdat)

ggplot(dat_nit_27, aes(date1,nh4)) + geom_point()+ geom_line(data = newdat)




## 9
chl_fit_9_dd <- fitode(
  chl_nh4_mod3,
  data = dat_nit_9, 
  start=start,
  tcol = "date1",
  solver.opts=list(method="rk4", hini=0.1)
)
coef(chl_fit_9_dd)

newdat9 <- simulate(chl_nh4_mod3,nsim = 5, parms= coef(chl_fit_9_dd), times = seq(1,11),             
                   solver.opts=list(method="rk4", hini=0.1))
newdat9$date1 <- newdat$times

ggplot(dat_nit_9, aes(date1,chl)) + geom_point()+ geom_line(data = newdat9)

ggplot(dat_nit_9, aes(date1,nh4)) + geom_point()+ geom_line(data = newdat9)



## 3
chl_fit_3_dd <- fitode(
  chl_nh4_mod3,
  data = dat_nit_3, 
  start=start,
  tcol = "date1",
  solver.opts=list(method="rk4", hini=0.1)
)
coef(chl_fit_3_dd)

newdat3 <- simulate(chl_nh4_mod3,nsim = 5, parms= coef(chl_fit_3_dd), times = seq(1,11),             
                    solver.opts=list(method="rk4", hini=0.1))
newdat3$date1 <- newdat$times

ggplot(dat_nit_3, aes(date1,chl)) + geom_point()+ geom_line(data = newdat3)

ggplot(dat_nit_3, aes(date1,nh4)) + geom_point()+ geom_line(data = newdat3)


## 54
chl_fit_54_dd <- fitode(
  chl_nh4_mod3,
  data = dat_nit_54, 
  start=start,
  tcol = "date1",
  solver.opts=list(method="rk4", hini=0.1)
)
coef(chl_fit_54_dd)

newdat54 <- simulate(chl_nh4_mod3,nsim = 5, parms= coef(chl_fit_54_dd), times = seq(1,11),             
                    solver.opts=list(method="rk4", hini=0.1))
newdat54$date1 <- newdat$times

ggplot(dat_nit_54, aes(date1,chl)) + geom_point()+ geom_line(data = newdat54)

ggplot(dat_nit_54, aes(date1,nh4)) + geom_point()+ geom_line(data = newdat54)


## 108
chl_fit_108_dd <- fitode(
  chl_nh4_mod3,
  data = dat_nit_108, 
  start=start,
  tcol = "date1",
  solver.opts=list(method="rk4", hini=0.1)
)
coef(chl_fit_108_dd)

newdat108 <- simulate(chl_nh4_mod3,nsim = 5, parms= coef(chl_fit_108_dd), times = seq(1,11),             
                     solver.opts=list(method="rk4", hini=0.1))
newdat108$date1 <- newdat$times

ggplot(dat_nit_108, aes(date1,chl)) + geom_point()+ geom_line(data = newdat108)

ggplot(dat_nit_108, aes(date1,nh4)) + geom_point()+ geom_line(data = newdat108)


## 0.5
chl_fit_0.5_dd <- fitode(
  chl_nh4_mod3,
  data = dat_nit_0.5, 
  start=start,
  tcol = "date1",
  solver.opts=list(method="rk4", hini=0.1)
)
coef(chl_fit_0.5_dd)

newdat0.5 <- simulate(chl_nh4_mod3,nsim = 5, parms= coef(chl_fit_0.5_dd), times = seq(1,11),             
                      solver.opts=list(method="rk4", hini=0.1))
newdat0.5$date1 <- newdat$times

ggplot(dat_nit_0.5, aes(date1,chl)) + geom_point()+ geom_line(data = newdat0.5)

ggplot(dat_nit_0.5, aes(date1,nh4)) + geom_point()+ geom_line(data = newdat0.5)

### calculate inverse variance weighted means

coef_all <- data.frame(
  coefficients = rep(names(coef(chl_fit_0.5_dd)),6),
  treatment = rep(c("0.5","3","9", "27","54","108"), each = 6),
  values = (c(coef(chl_fit_0.5_dd), coef(chl_fit_3_dd), coef(chl_fit_9_dd), coef(chl_fit_27_dd), 
               coef(chl_fit_54_dd), coef(chl_fit_108_dd)))
)






