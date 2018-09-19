# nh4 excretion
source("transfer_functions.R")
library(tidyverse)
dat <- read.csv("Daphnia_large_Feeding_Nov11.csv")


dat$rate_nh4 <- with(dat, (nh42-nh41)/Nh4_Time_Diff)
dat$rate_nh4_ind <- dat$rate_nh4/dat$Num_Daphnia

dat$rate_chl <- with(dat, (chl1-chl2)/Chl_Time_Diff)
dat$rate_chl_ind <- dat$rate_chl/dat$Num_Daphnia

dat1 <- dat %>%
  filter(control == 1)

dat2 <- dat %>%
  filter(control == 2)

with(dat, plot(rate_chl1_ind, rate_nh4_ind))

df <- data.frame(hollings2(a= dat$chl1, h = 0.0000939, r = 78.05))
df<- df %>% drop_na()


# need to incorporate algae uptake for this to make sense
(b <- nls(rate_nh4_ind ~ 
           b*(chl1*0.0000939)/(1+chl1*0.0000939*78.05),
          start = list(b=0.001), data = dat1))

m1 <- nls(rate_nh4_ind~(chl1*h)/(1+chl1*h*r), start = list(h=0.002,r=130), data = dat1 )

newpred <- sat_fun(a= seq(0.1,24,0.1), h = 0.0000939, r = 78.05) 

newpred <- hollings2(a= seq(0.1,24,0.1), h = -8.228e-06, r = -4.871e+03) 



rdat2 <- read.csv("Ceriodaphnia_Feeding_Nov07_2017.csv")

rdat3 <- filter(rdat2, Control.Y.N == "N", date == "11_7" | date == "11_16" | date == "12_31")
rdat3$rate_chl <- with(rdat3, (Chl.1-chl2)/Chl_Time_Diff)
rdat3$rate_chl_ind <- with(rdat3, rate_chl/X..of.ceriodaphnia)

m2 <- nls(rate_chl_ind~(Chl.1*h)/(1+Chl.1*h*r), start = list(h=0.00001,r=100), data = rdat3 )
