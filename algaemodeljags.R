library(R2jags)
library(coda)
library(emdbook)    ## for lump.mcmc.list() and as.mcmc.bugs()
library(arm)        ## for coefplot
library(lattice)
library(dotwhisker)

d <- rlnorm(50, meanlog = 2, sdlog = 1)
x <- c(1:50)


y = d

mod <- function() {
  
  for (i in 1:N) {
  y[i] ~ dlnorm( muOfLogY, 1/sigmaOfLogY^2 ) 
  }
  
  sigmaOfLogY ~ dunif(0.001*sdOfLogY , 1000*sdOfLogY )
  muOfLogY ~ dnorm( meanOfLogY, 0.001*1/sdOfLogY^2 )
  
  muOfY <- exp(muOfLogY+sigmaOfLogY^2/2)
  sigmaOfY <- sqrt(exp(2*muOfLogY+sigmaOfLogY^2)*(exp(sigmaOfLogY^2)-1))
} 

mod2 <- function() {
  
  for (i in 2:N) {
    for (j in 1:K) {
      
    y[i, j] ~ dlnorm( mu[i, j], sigma ) 
    mu[i, j] <- (a*z[i-1,j]/(S+z[i-1,j]))*y[i-1,j] #- D*y[i-1,j]^2
  }
  }
  sigma ~ dunif(0.001, 1000)
    a ~ dunif(0.001,1000)
    S ~ dunif(0.001,1000)
 #   D ~ dunif(0.001,1000)

}

mod.fit <- jags(data = list('N'= nrow(ch1),
                            'y'= ch1,
                            'K'= ncol(ch1),
                            'z'=nh1
                          ),
              parameters.to.save = c("a", "sigma","S", "mu"), 
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

