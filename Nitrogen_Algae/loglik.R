library(tidyverse)
library(deSolve)
d2 <- read.csv("Data/Algae_Nutrient.csv ")

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
    out[-1, ] 
  }
  

d2$ammonium <- d2$nh4*1000  ## put nh4 in ug so that it is one the same scale as chl
d2$algae <- d2$chl  

loglik <- function (params, data) {
    times <- data$date1
    pred <- prediction(params, times)
    sum(dnorm(x=data$algae,mean=pred[,3],sd=params["sigma"],log=TRUE))+
      sum(dnorm(x=data$ammonium, mean=pred[,2], sd=params["sigma"],log=TRUE))
  }  



## params based on a set that gave a reasonable fit by eye (a = 7, g= 0.215)
params <- c(a=NA,k=1500,l=0.1,death1=1.05, g= NA, sigma=10) # is sigma = 1 weird? how to pick a sigma? Because im on such a large scale

f <- function (eg) {
  par <- params
  par["a"] <- eg$a
  par["g"] <- eg$g
  loglik(par,d2)
}


eg <- expand.grid(
  a = seq(from=1,to=10,by=.1),
  g = seq(from=0.01, to=5, by=0.02),
  ll = 0
)


for(i in 1:nrow(eg)){
  eg$ll[i] <- f(eg[i,])
}

hist(eg$ll) 

ggplot(eg, aes(a,g)) + geom_contour(aes(z=ll), bins = 100)

which(eg$ll == max(eg$ll))



## graph with data 
param = c(a=eg[which(eg$ll == max(eg$ll)),1][1],k=1500,l=0.1,death1=1.05, g= eg[which(eg$ll == max(eg$ll)),2 ][1]) ## unfortunately multiple equivalent likelihoods...
state = c(ammonium = 13000, algae = 40)
out <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param))
names(out) <- c("date1", "ammonium","chl")
ggplot(out, aes(date1, chl)) + geom_point(data = d2) + geom_line(data = out)
ggplot(out, aes(date1, ammonium)) + geom_point(data=d2) + geom_line(data = out)

library(viridisLite)
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
params <- c(a=NA,k=1500,l=0.1,death1=1.05, g= NA, sigma=10) # is sigma = 1 weird? how to pick a sigma? Because im on such a large scale

f <- function (eg) {
  par <- params
  par["a"] <- eg$a
  par["g"] <- eg$g
  loglik(par,d2)
}


eg <- expand.grid(
    ## a = seq(from=1,to=10,by=.1),
    a = seq(from=2.5,to=10, length.out=71),
    ## g = seq(from=0.01, to=1.5, by=0.02),
    g = seq(from=0.01, to=0.75, length.out=71),
    ll = 0
)

res <- plyr::alply(eg,1,f,.progress="text")
eg$ll <- unlist(res)
## for(i in 1:nrow(eg)){
##  eg$ll[i] <- f(eg[i,])
##}

hist(eg$ll) 

w <- which(eg$ll == max(eg$ll))
if (length(w)>1) warning("multiple max values")
(ggplot(eg, aes(a,g))
    + geom_raster(aes(fill=max(ll)-ll))
    + scale_fill_viridis_c(trans=scales::log10_trans())
    + geom_contour(aes(z=max(ll)-ll),breaks=c(15,20,25),colour="red",lty=2)
    + geom_point(data=eg[w,],colour="red")
)
## comments:
## * after fixing sigma, I zoomed in a bunch (and changed scales etc.)
## * log-likelihood differences of 15, 20, 25 (dashed lines) are very
##   large; e.g. 99.9% likelihood region for a 2-parameter space is
##   qchisq(0.999,2)/2 = 6.9 log-likelihood units
##


## this is what the log-likelihood surface looks along the max slice!
eg_slice <- (eg
    %>% group_by(a)
    %>% summarise(g=g[which.max(ll)],
                  ll=max(ll))
)
## yikes! I had hoped this would be smooth
ggplot(eg_slice,aes(a,ll))+geom_line()+geom_point()
## maybe try a few of these values to see what the plots look like?

## select best ll

eg1 <- eg %>% filter(ll > -170) ##46

## graph with data 
param = c(a=eg1[46,1][1],k=1500,l=0.1,death1=1.05, g= eg1[46,2 ][1]) ## unfortunately multiple equivalent likelihoods...
state = c(ammonium = 13000, algae = 40)
out <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param))
names(out) <- c("date1", "ammonium","chl")
ggplot(out, aes(date1, chl)) + geom_point(data = d2) + geom_line(data = out)
ggplot(out, aes(date1, ammonium)) + geom_point(data=d2) + geom_line(data = out)







