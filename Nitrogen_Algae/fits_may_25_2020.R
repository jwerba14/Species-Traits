library(pomp)
library(tidyverse)
library(viridisLite)
library(deSolve)
d2 <- read.csv("Data/Algae_Nutrient.csv")

d2$treat1 <- as.numeric(as.factor(d2$treat))

## select treatment 
d2 <- d2 %>% filter(treat1 == 4) %>% filter(rep == 1) 

#parameters  // p[1]=a;  p[2] = k p[3] = l p[4]=death1  p[5] = g 

nit_ODE <-function(times, state, parameters) {
  with(as.list(c(state, parameters)),{
    dammonium <- -(a*ammonium)/(k+ammonium)*algae + l*death1*algae  #- 0.028*ammonium  ## see if works better without non-algal losses
    dalgae <- (a*ammonium)/(k+ammonium)*algae*g - death1*algae
    # return the rate of change
    list(c(dammonium, dalgae))
  }) 
}


prediction <- function (params, times) {
  out <- ode(
    func=nit_ODE,
    y=c(ammonium = 13000, algae = 40), 
    times=c(0, times),
    parms=params
  )
  return(out[-1, ])
}

d2$ammonium <- d2$nh4*1000  ## put nh4 in ug so that it is one the same scale as chl
d2$algae <- d2$chl  

## calc SD based on data
## confusing/unfortunate that we have mean as an argument and a function,
##  but oh well ...
dnorm2 <- function(x,mean,log=FALSE) {
  sd <- sqrt(mean((x-mean)^2))
  dnorm(x,mean,sd,log=log)
}
loglik <- function (params, data) {
  times <- data$date1
  pred <- prediction(params, times)
  sum(dnorm2(x=data$algae,mean=pred[,3],log=TRUE))+
    sum(dnorm2(x=data$ammonium, mean=pred[,2], log=TRUE))
}  


## params based on a set that gave a reasonable fit by eye (a = 7, g= 0.215)
params <- c(a=9.10701,k=5528,l=NA,death1=0.08105952, g= NA, sigma=10) # is sigma = 1 weird? how to pick a sigma? Because im on such a large scale

tt <- sobolDesign(lower = c(a =1, k= 1000, l = 0.01, death1 = 0.05, g = 0.01),
                  upper = c(a =50, k= 9000, l = 1, death1 = 2, g = 1),
                  500000)


gg <- data.frame(a = numeric(length = nrow(tt)),
                 k = numeric(length = nrow(tt)),
                 l =numeric(length = nrow(tt)),
                 death1 = numeric(length = nrow(tt)),
                 g = numeric(length = nrow(tt)),
                 loglik = numeric(length = nrow(tt)))
system.time(
for (i in 1:nrow(tt)){
  params <- unlist(tt[i,])
  gg[i, 1:5] <- params
  gg[i,6] <- loglik(params,d2)
  
}


)

w <- which(gg$loglik == max(gg$loglik))


param = c(a=gg[which(gg$loglik == max(gg$loglik)),1][1],k=gg[which(gg$loglik == max(gg$loglik)),2][1],
          l=gg[which(gg$loglik == max(gg$loglik)),3][1],death1=gg[which(gg$loglik == max(gg$loglik)),4][1], 
          g= gg[which(gg$loglik == max(gg$loglik)),5][1]) 
state = c(ammonium = 13000, algae = 40)
out <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param))
names(out) <- c("date1", "ammonium","chl")
ggplot(out, aes(date1, chl)) + geom_point(data = d2) + geom_line(data = out)
ggplot(out, aes(date1, ammonium)) + geom_point(data=d2) + geom_line(data = out)

write.csv(gg, file = "Fits_4_1.csv")

gg %>% filter(loglik > -170) %>% pairs() ## trade off between a and g quite strong, aloso g and death1

