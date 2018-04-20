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

#remove last two days because see population decline
dat <- dat %>%
  filter(date1 != 10 | date1 != 11)

dat$change_nh4 <- dat$change_nh4*(-1)
# fit mm first

#plot start vs ending (log transform?? )
mm <- nls(change_nh4 ~ nh4*a/(k+nh4)*chl, start = list(a=3,k=20),data = dat)

          
m <- nls(change_chl ~ nh4*b*a/(k+nh4)*chl , start = list(b=1), data = dat)

newpred <- predict(m)

plot(seq(1, 330,1), newpred)
points(dat$change_chl,dat$change_nh4)

newdat <- dat[dat$date1==2,]

newdat1 <-dat[dat$date1==9,]

newdat2 <- cbind(newdat$treat, newdat$nh4-newdat1$nh4)
plot(newdat2,col= newdat$treat)
newdat2[,2]
