library(tidyverse)
library(fitode)
source("transfer_functions.R")
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


### correct Nh4 for pH based on communication with YSI

# first need temp in kelvin
tempK <- dat$temp + 273.15 
nh3 <- dat$nh4 * (10^(dat$ph-((2726.3/tempK)+0.0963)))

cnitrate = .000001 # nitrate lost to env-- calc in nutrient_air.R
cammonium = .0001 # ammonium lost to env-- calc in nutrient_air.R



chl_nh4_mod <- new("model.ode",
                   name = "algal_nit",
                   model = list(
                       pred_nh4 ~ -pred_chl*((v)/(pred_nh4+s))-cnitrate -cammonium,
                     
                       ## chl is gained through uptake of nh4 and lost through density dependent death
                       ## death is not directly measured-- for evidence of dd death see nls feeding
                       pred_chl ~ pred_chl*((v)/(pred_nh4+s))*g-pred_chl*((death*pred_chl)/(d1+pred_chl))
                     
                   ),
                   ## consider using bbmle::dnorm_n ?
                   observation = list(
                     nh4 ~ dnorm(mean = pred_nh4, sd=sd1),
                     chl ~ dnorm(mean = pred_chl, sd=sd2)
                     
                   ),
                   initial = list(pred_nh4 ~ pred_nh40 , pred_chl ~ pred_chl0),
                   par=c("v","s","g","pred_nh40","pred_chl0", "sd1","sd2", "death","d1")
                   )


 options(error=recover)  ## stop/browse when error occurs
chl_fit_27_dd <- fitode(
  chl_nh4_mod,
  data = dat_nit_27, 
  start=c(v = 5, 
          s = 1000,
          g=10,
          pred_nh40 = 15 ,
          pred_chl0 = 15, 
          sd1 = 1,
          sd2 = 1,
          death=0.01,
          d1=100),
  tcol = "date1" #,
  #method="Nelder-Mead"
)
plot(chl_fit_27_dd)  ## I think to change the shape of the nh4 curve it needs its own parameters like before



chl_fit_27 <- fitode(
  chl_nh4_mod,
  data = dat_nit_27, 
  start=c(v = .1, 
          s = 10,
          j = .28,
          h = 250,
          pred_nh40 = 15 ,
          pred_chl0 = 15, 
          sd1 = 0.1 ,
          sd2 = 0.1,
          death=0.1),
  tcol = "date1" #,
  #method="Nelder-Mead"
)
plot(chl_fit_27)



## close but not great
chl_fit_9_dd <- fitode(
  chl_nh4_mod,
  data = dat_nit_9,
  start=c(v = 600, 
          s = 54000,
          j = .9,
          h = 2,
          pred_nh40 = 4 ,
          pred_chl0 = 15, 
          sd1 = 0.8 ,
          sd2 = 3,
          death=0.3,
          d1 = 1),
  tcol = "date1" #,
  #method="Nelder-Mead"
)
plot(chl_fit_9_dd)




chl_fit_9 <- fitode(
  chl_nh4_mod,
  data = dat_nit_9,
  start=c(v = 1, 
          s = 4.5,
          j = .1,
          h = 100,
          pred_nh40 = 6 ,
          pred_chl0 = 15, 
          sd1 = 0.1 ,
          sd2 = 0.1,
          death=0.1),
  tcol = "date1" #,
  #method="Nelder-Mead"
)
plot(chl_fit_9)


##not bad
chl_fit_3_dd <- fitode(
  chl_nh4_mod,
  data = dat_nit_3,
  start=c(v = 3, 
          s = 3000,
          j = 70,
          h = 600,
          pred_nh40 = 3 ,
          pred_chl0 = 15, 
          sd1 = 0.3 ,
          sd2 = 20,
          death=2.2e-16,
          d1 = 1),
  tcol = "date1" #,
  #method="Nelder-Mead"
)
plot(chl_fit_3_dd)


chl_fit_3 <- fitode(
  chl_nh4_mod,
  data = dat_nit_3,
  start=c(v = 1, 
          s = 10,
          j = .1,
          h = 100,
          pred_nh40 = 3 ,
          pred_chl0 = 15, 
          sd1 = 0.1 ,
          sd2 = 0.1,
          death=0.1),
  tcol = "date1" #,
  #method="Nelder-Mead"
)
plot(chl_fit_3)

## looks good
chl_fit_54 <- fitode(
  chl_nh4_mod,
  data = dat_nit_54,
  start=c(v = 8, 
          s = .5,
          j = .8,
          h = 150,
          pred_nh40 = 25 ,
          pred_chl0 = 15, 
          sd1 = 0.1 ,
          sd2 = 0.1,
          death=0.1),
  tcol = "date1" #,
  #method="Nelder-Mead"
)
plot(chl_fit_54)



chl_fit_108 <- fitode(
  chl_nh4_mod,
  data = dat_nit_108,
  start=c(v = 8, 
          s = .5,
          j = .8,
          h = 150,
          pred_nh40 = 25 ,
          pred_chl0 = 15, 
          sd1 = 0.1 ,
          sd2 = 0.1,
          death=0.1),
  tcol = "date1" #,
  #method="Nelder-Mead"
)
plot(chl_fit_108)
## what does warning mean:In .local(object, ...) :
#### At least one entries in diag(vcov) is negative. Confidence interval will be accurate.


##
chl_fit_0.5 <- fitode(
  chl_nh4_mod,
  data = dat_nit_0.5,
  start=c(v = 1, 
          s = 1,
          j = .25,
          h = 200,
          pred_nh40 = 3 ,
          pred_chl0 = 15, 
          sd1 = 0.1 ,
          sd2 = 0.1,
          death=0.1),
  tcol = "date1" #,
  #method="Nelder-Mead"
)
plot(chl_fit_0.5)


## make a dataframe of all parameters

treat0.5 <- data.frame(confint(chl_fit_0.5))
treat3 <- data.frame(confint(chl_fit_3))
treat9 <- data.frame(confint(chl_fit_9))
treat27 <- data.frame(confint(chl_fit_27))
treat54 <- data.frame(confint(chl_fit_54))
treat108 <- data.frame(confint(chl_fit_108))

all_param <- data.frame(
  model =  rep(c("chl_fit_0.5", "chl_fit_3","chl_fit_9","chl_fit_27","chl_fit_54","chl_fit_108"), each= 9),
  parameter = rep(c("v","s", "j", "h", "pred_nh40", "pred_chl0","sd1","sd2","death"), 6),
  estimate = c(treat0.5$estimate,treat3$estimate,treat9$estimate,treat27$estimate,treat54$estimate,treat108$estimate),
  lowcon = c(treat0.5$X2.5..,treat3$X2.5..,treat9$X2.5..,treat27$X2.5..,treat54$X2.5..,treat108$X2.5..),
  uppcon =  c(treat0.5$X97.5..,treat3$X97.5..,treat9$X97.5..,treat27$X97.5..,treat54$X97.5..,treat108$X97.5..)
  
)

ggplot(data = all_param, aes(model,estimate)) + geom_point() + facet_wrap(~parameter)
ggplot(data = all_param, aes(parameter,estimate)) + geom_point() + facet_wrap(~model)

ggplot(data = all_param[all_param$estimate < 250, ], aes(model,estimate)) + geom_point() + facet_wrap(~parameter)
