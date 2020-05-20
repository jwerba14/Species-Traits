#sigma ~ normal(0, 1);
#p[1] ~ lognormal(2.9, 1);
#p[2] ~ lognormal(0.3, 1);
#p[3] ~ lognormal(0,1);
#p[4] ~ lognormal(-3.5, 1);
#p[5] ~ lognormal(2.701, 1);
#y0[1] ~ normal(0, 10);
#y0[2] ~ normal(0, 10);
library(tidyverse)
library(deSolve)
d2 <- read.csv("Data/Algae_Nutrient.csv ")

## make dataframe with 1000 draws from each prior
#parameters  // p[1]=a;  p[2] = k p[3] = l p[4]=death1  p[5] = f 
param_dat <- data.frame(
  a = rlnorm(1000,2.9,1),
  k = rlnorm(1000,0.3, 1),
  l = rlnorm(1000,0,1),
  death1 = rlnorm(1000, -3.5,1),
  f = rlnorm(1000,2.701, 1)
  
)

nit_ODE <-function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    
    
    dammonium <- -(a*ammonium)/(k+ammonium)*algae + l*death1*algae  #- 0.028*ammonium  ## see if works better without non-algal losses
    

    dalgae <- (a*ammonium)/(k+ammonium)*algae*f - death1*algae
    
  
    # return the rate of change
    list(c(dammonium, dalgae))
  }) 
}

check1 <- numeric(length = 1000)
check2 <- numeric(length = 1000)
classv <- numeric(length = 1000)
state = c(ammonium = 10, algae = 6)
options(warn = 2)

for(i in 1:nrow(param_dat)){
  temp_parm <- as.list(param_dat[i,])
  
  out <- try(silent = FALSE,
   ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = temp_parm))
  if(class(out)[1] == "try-error") {
    classv[i] <- 1
  } else {
    
  out <- data.frame(out)
  check1[i] <- length(with(out,which(ammonium < 0)))
  check2[i] <- length(with(out,which(algae < 0)))}

  
  print(i)
}

options(warn = 1)
param_dat$class <- classv
plot(param_dat)

##output with fitted param from odes
#c(alpha = 5.577709e-03, 
#  beta = 1.626124e+01,
#  omega=1.401032e+01,
#  death1=2.954388e-03,
#  death2=6.663002e-04,
#  pred_nh40 = 6 ,
#  pred_chl0 = 50, 
#  gamma=9.698845e-02 


param = list( a = 0.3
   ,k = 0.5
   ,l =  .3
   , death1 =1 
   , f= 7)

state = c(ammonium = 12000, algae =43)

out <- ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param)
out <- data.frame(out)

names(out) <- c("date1", "nh4","chl")

d2$treat1 <- as.numeric(as.factor(d2$treat))

## select treatment (I guess not necessary for exploration)
test1 <- d2 %>% filter(treat1 == 4) 

## convert nh4 from mg/L to ug/L
test1 <- test1 %>% mutate(nh4 =nh4*1000)

ggplot(test1, aes(date1, chl)) + geom_point() + geom_line(data = out)


ggplot(test1, aes(date1, nh4)) + geom_point() + geom_line(data = out)


test1 <- d2 %>% filter(treat1 == 1)
ggplot(test1, aes(date1, nh4)) + geom_point(aes(color = as.factor(rep)))



