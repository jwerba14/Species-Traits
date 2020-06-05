## functions and set up for fitting



## build matrix from which to sample

tt <- sobolDesign(lower = c(a =1, k= 1000, l = 0.01, death1 = 0.05, g = 0.01),
                  upper = c(a =50, k= 9000, l = 1, death1 = 2, g = 1),
                  500000)

## ode to run

nit_ODE <-function(times, state, parameters) {
  with(as.list(c(state, parameters)),{
    
    
    dammonium <- -(a*ammonium)/(k+ammonium)*algae + l*death1*algae  #- 0.028*ammonium  ## see if works better without non-algal losses
    
    
    dalgae <- (a*ammonium)/(k+ammonium)*algae*g - death1*algae
    
    
    # return the rate of change
    list(c(dammonium, dalgae))
  }) 
}

## run ODE
prediction <- function (params, times) {
  out <- ode(
    func=nit_ODE,
    y=c(ammonium = mean(d2[d2$date1 ==1, "ammonium"]), algae =  mean(d2[d2$date1 ==1, "algae"])),   
    times=c(0, times),
    parms=params
  )
  out[-1, ] 
}

## calculate log likelihood

dnorm2 <- function(x,mean,log=FALSE) {
  sd <- sqrt(mean((x-mean)^2))
  dnorm(x,mean,sd,log=log)
}


loglik <- function (params, data) {
  times <- data$date1
  pred <- prediction(params, times)
  sum(dnorm(x=data$algae,mean=pred[,3],sd=params["sigma"],log=TRUE))+
    sum(dnorm(x=data$ammonium, mean=pred[,2], sd=params["sigma"],log=TRUE))
}  
