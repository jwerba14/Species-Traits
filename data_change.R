library(tidyverse)
dat <- read.csv("Algae_Nutrient.csv")

dat$uni <- dat$treat + dat$rep

dat$change_nh4 <- 0
dat$change_chl <- 0

for (i in 1:length(unique(dat$uni))) {
  dat[dat$uni==unique(dat$uni)[i],]$change_nh4 <- 
   c(diff(dat[dat$uni==unique(dat$uni)[i],]$nh4),0)
  dat[dat$uni==unique(dat$uni)[i],]$change_chl <- 
    c(diff(dat[dat$uni==unique(dat$uni)[i],]$chl),0)
}
