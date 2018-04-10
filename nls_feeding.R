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

#lit cerio
#ldat <- read.csv("Feeding_lit_cerio.csv")
# looks pretty linear...and with higher conc less filtration-- bc they are looking at amnt of water...
#cm <- nls(ml.animal.hr_filtered~(concentration_algae.ug.ml.)/(1+concentration_algae.ug.ml.*h*r), start = list(h=.00001,r=-500), data = ldat )


rdat2 <- read.csv("Ceriodaphnia_Feeding_Nov07_2017.csv")

rdat3 <- filter(rdat2, Control.Y.N == "N", date == "11_7" | date == "11_16" | date == "12_31")
rdat3$rate_chl <- with(rdat3, (Chl.1-chl2)/Chl_Time_Diff)
rdat3$rate_chl_ind <- with(rdat3, rate_chl/X..of.ceriodaphnia)

m2 <- nls(rate_chl_ind~(Chl.1*h)/(1+Chl.1*h*r), start = list(h=0.00001,r=100), data = rdat3 )


rdatj <- read.csv("Small_Daph_Feeding.csv")
rdatj1 <- rdatj %>%
  filter(Control.Y.N == "N", date == "10_4_2017"| date == "1_8_2017" | date == "2_12_2018")
rdatj1$rate_chl <- with(rdatj1, (Chl.1-Chl.2)/Chl_Time_Diff)
rdatj1$rate_chl_ind <- with(rdatj1, rate_chl/X..of.Daphnia)

m3 <- nls(rate_chl_ind~(Chl.1*h)/(1+Chl.1*h*r), start = list(h=0.0001,r=130), data = rdatj1 )
# h= .000026 p= 0.02 std er .000014; r = -448 se = 798 p = .57

newpred <- hollings2(a= seq(0.1,24,0.1), h = 0.000026, r = -448)
plot(seq(0.1,24,0.1), newpred)
points(rdatj1$Chl.1,rdatj1$rate_chl_ind)
