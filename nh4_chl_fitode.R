library(fitode)
source("transfer_functions.R")

## exp_model <- new("model.ode",
##                  name="exponential",
##                  model=list(
##                    A ~ - m * A
##                  ),
##                  observation=list(
##                    nitrogen ~ dnorm(mean=A, sd=sd.nitrogen)
##                  ),
##                  initial=list(
##                    A ~ A0
##                  ),
##                  par=c("m", "A0", "sd.nitrogen")
## )

## ff1 <- fitode(
##   exp_model,
##   data=data,
##   start=c(m=0.4, A0=4500, sd.nitrogen=100), ## naming has to be consistent with parameter names
##   tcol="day")
  

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
                       pred_nh4 ~ -pred_chl*((v*pred_nh4)/(pred_nh4+s))-cnitrate -cammonium,
                     
                       ## chl is gained through uptake of nh4 and lost through density dependent death
                       ## death is not directly measured
                       pred_chl ~ pred_chl*((j*pred_nh4)/(pred_nh4+h))-pred_chl*death
                     
                   ),
                   ## consider using bbmle::dnorm_n ?
                   observation = list(
                     nh4 ~ dnorm(mean = pred_nh4, sd=sd1),
                     chl ~ dnorm(mean = pred_chl, sd=sd2)
                     
                   ),
                   initial = list(pred_nh4 ~ pred_nh40 , pred_chl ~ pred_chl0),
                   par=c("v","s","j","h","pred_nh40","pred_chl0", "sd1","sd2", "death")
                   )


options(error=recover)  ## stop/browse when error occurs
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


##not terrible
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


chl_fit_54 <- fitode(
  chl_nh4_mod,
  data = dat_nit_54,
  start=c(v = .1, 
          s = 5,
          j = .28,
          h = 125,
          pred_nh40 = 25 ,
          pred_chl0 = 15, 
          sd1 = 0.1 ,
          sd2 = 0.1,
          death=0.1),
  tcol = "date1" #,
  #method="Nelder-Mead"
)
plot(chl_fit_54)


