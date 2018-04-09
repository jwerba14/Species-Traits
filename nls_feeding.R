dat = read.csv("Feeding_lit_extraction.csv")



m <- nls(feeding.rate.mean~(Chl_conc*h)/(1+Chl_conc*h*r), start = list(h=1,r=1), data = dat )

newpred <- hollings2(a= seq(0.1,24,0.1), h = 0.264, r = 0.5218)
plot(seq(0.1,24,0.1), newpred)
points(dat$Chl_conc,dat$feeding.rate.mean)

library(tidyverse)
#currently doesn't account for controls and doesn't have high conc feeding 

rdat <- read.csv("Daphnia_large_Feeding_Nov11.csv")

rdat$rate_chl <- with(rdat, (chl1-chl2)/Chl_Time_Diff)
rdat$rate_chl_ind <- rdat$rate_chl/rdat$Num_Daphnia

rdat1 <- rdat %>%
  filter(control == 1)

m1 <- nls(rate_chl_ind~(chl1*h)/(1+chl1*h*r), start = list(h=0.2,r=0.5), data = rdat1 )

newpred <- hollings2(a= seq(0.1,24,0.1), h = 0.000122, r = 137.7)
plot(seq(0.1,24,0.1), newpred)
points(rdat1$chl1,rdat1$rate_chl_ind)

rdat2 <- read.csv("Ceriodaphnia_Feeding_Nov07_2017.csv")

rdat3 <- filter(rdat2, Control.Y.N == "N", date == "11_7" | date == "11_16" | date == "12_31")
rdat3$rate_chl <- with(rdat3, (Chl.1-chl2)/Chl_Time_Diff)
rdat3$rate_chl_ind <- with(rdat3, rate_chl/X..of.ceriodaphnia)

m2 <- nls(rate_chl_ind~(Chl.1*h)/(1+Chl.1*h*r), start = list(h=0.00001,r=100), data = rdat3 )
