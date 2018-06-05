library(tidyverse)
dat <- read.csv("Algae_Nutrient.csv")

#create column that is unique for every replicate
dat$uni <- dat$treat + dat$rep

#create columns for change in nh4 and change in chl
dat$change_nh4 <- 0
dat$change_chl <- 0

#populate columns with chnages in nh4 and chl
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

##pseudo-algorithm

LL <- function(a, b,m) {
  df <- odefun(a,b,m)
  
  ll <- sum(dnorm(data$nh4, mean=df[,1], sd=sigma1, log=TRUE)) +
  sum(dnorm(data$chl, mean=df[,2], sd=sigma1, log=TRUE))
    
  -ll
}

do(m=mle2(LL, start=list(a=1, b=2), data=.),
   fixed(m=0.076432))

ggplot(dat, aes(date,nh4))+geom_point(aes(color=as.factor(treat)))
ggplot(dat, aes(date,chl))+geom_point(aes(color=as.factor(treat)))

# fit mm first

#plot start vs ending (log transform?? )
mm <- nls((nh4+change_nh4) ~ ((nh4*a)/(k+nh4)*chl)*-1.0169, start = list(a=1,k=100),data = dat)

          
m <- nls(chl ~ nh4*b*a/(k+nh4)*chl , start = list(a=10,b=1,k=100), data = dat)

newpred <- predict(m)

plot(seq(1, 330,1), newpred)
points(dat$change_chl,dat$change_nh4)

newdat <- dat[dat$date1==2,]

newdat1 <-dat[dat$date1==9,]

newdat2 <- cbind(newdat$treat, newdat$nh4-newdat1$nh4)
plot(newdat2,col= newdat$treat)
newdat2[,2]
