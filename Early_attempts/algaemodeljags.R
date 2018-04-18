library(R2jags)
library(coda)
library(emdbook)    ## for lump.mcmc.list() and as.mcmc.bugs()
library(arm)        ## for coefplot
library(lattice)
library(dotwhisker)



#mod <- function() {
  
  for (i in 1:N) {
  y[i] ~ dlnorm( muOfLogY, 1/sigmaOfLogY^2 ) 
  }
  
  sigmaOfLogY ~ dunif(0.001*sdOfLogY , 1000*sdOfLogY )
  muOfLogY ~ dnorm( meanOfLogY, 0.001*1/sdOfLogY^2 )
  
  muOfY <- exp(muOfLogY+sigmaOfLogY^2/2)
  sigmaOfY <- sqrt(exp(2*muOfLogY+sigmaOfLogY^2)*(exp(sigmaOfLogY^2)-1))
} 

load("cleandat.R")
mod2 <- function() {
  
  for (i in 2:N) {
    for (j in 1:K) {
      
    chl[i, j] ~ dlnorm( mu[i, j], sigma ) 
    mu[i, j] <- chl[i-1,j]+
    (a*nh4[i-1,j]/(S+nh4[i-1,j]))*chl[i-1,j] - (d1*chl[i-1,j]^(1/d2))*chl[i-1,j]
  }
  }
  sigma ~ dunif(0.001, 1000)
    a ~ dunif(0.001,1000)
    S ~ dunif(0.001,1000)
    d1 ~ dunif(0.001,1000)
    d2 ~ dunif(0.001,1000)
}

mod.fit <- jags(data = list('N'= nrow(obs_chl),
                            'chl'= obs_chl,
                            'K'= ncol(obs_chl),
                            'nh4'=obs_chl_n
                          ),
              parameters.to.save = c("a","S", "d1","d2"), 
              n.chains = 3,
              n.iter = 10000,
              n.burnin = 2000,
              model.file = mod2)


mm <- as.mcmc(mod.fit)
xyplot(mm)
densityplot(mm)

ggplot(aes(date1, chl), data = dat) + geom_point() + geom_line(data=pr)


fun1 <- function(a, S, z){
  #exp(a)*z/(exp(S)+z)
  exp(a*z / (S + z))
}
##plot curve of speed of uptake
pr <- data.frame(
  Conc= seq(min(nh1),max(nh1),0.1),
  up = fun1(
    a = unlist(mod.fit$BUGSoutput$median[2]),
    S = unlist(mod.fit$BUGSoutput$median[1]),
    z = seq(min(nh1),max(nh1),0.1)))
plot(pr)

#prediction attempt
fun2 <- function(a, S, z, y){
  exp(rowMeans(y)*(a*rowMeans(z)/(S+rowMeans(z))))
}

pr <- data.frame(
  day = seq(1, 11, 1),
  up = fun2(
    a = unlist(mod.fit$BUGSoutput$median[2]),
    S = unlist(mod.fit$BUGSoutput$median[1]),
 #  D = unlist(mod.fit$BUGSoutput$median[1]),
    z = nh1,
    y = ch1))

