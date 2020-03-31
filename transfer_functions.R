#possible transfer functions

# all feeding on algae by zooplankton is saturating- 
#hollings type II (could be linear...)
# a is attack rate, h is handling time
hollings2 <- function (a,h,r) {
  (a*h)/(1+a*h*r)
}

#Holling's type 1 is  linear with respect to prey density (a) 
#so just multiply by constant

# algae uptake of nutrients is saturating- michaelis menten
#where S is concentration of nutrient, vmax is the maximum rate possible, 
#k is half of vmax
mich_men <- function(vmax, s, k) {
  
  vmax * s / (k + s)
  
}

# death functions- going to assume non-linear density dependent

# where g is the state variable- 
#then this is multiplied by the state variable e*g*g
death <- function (e,g) {
  e*g 
}

# saturating function for nutrient release where K is independent state variable
sat_fun <- function(a,b,k) {
  
  a * k / (k + b)
  
}


sat_fun_log <- function(a,b,k) {
  
  log(a) + log(k) - log(b+k)
  
}

sat_fun_inv <- function(a,b,k) {
  
   (k + b) / (a * k)
  
}


# exp dist fun
exp_dist_fun <- function(a,k) {
  a * exp(-a*k)
}


# power law 
pow_law <- function(a,b,k) {
  a*k^(1/b)
}

# sigmoidal
sig_fun <- function(a,b,c,k) {
  (a * k ^ b) / (c + k ^ b)
}

#lognormal
lognor <- function(a,b,c){
  e^-((log(c)^2)/(2*a^2))/(c*a*sqrt(2*pi))
}

# linear regression

lin <- function(m,b,c){
  m*c + b
}

lin2 <- function(m,c){
  m*c
}

lin3 <- function(m,t,c){
  m*t*c
}


## exponential

expon <- function(b,days){
  exp(-days/b)
}

 
#logistic function for ODE
logist <- function(r,k,t,a0, debug=FALSE) {
    if (debug) cat("r,k:",r,k,"\n")
    k/ (1+((k/a0)-1)*exp(-r*t))
}

#alt parameterization logistic

#' @param r growth rate
#' @param a0 initial algal density
#' @param n0 initial (= total) nitrogen
#' @param x nitrogen use efficiency
#' @param phi nitrogen uptake rate
#' @param t time
log_alt <- function(phi,x,a0,n0,t) {
    r <- n0*phi
    k <- n0/x
    k/(1+((k/a0)-1)*exp(-r*t))
}

library(deSolve)
log_grad <- function(time, state, params) {
    grad <- with(as.list(c(state,params)), ## magic for param/state names
                 c(chl=r*chl*(1-chl/k)))
    return(list(grad))
}

## function to get fit for either parameterization

##' @param a0 starting vector (algal concentration)
##' @param t time vector
##' @param params parameter vector: may contain *either* (r,k)
##' *or* (phi,x,n0)
ode_pred <- function(params,t,a0) {
    alt_params <- all(c("phi","x","n0") %in% names(params))
    if (!alt_params && !all(c("r","k") %in% names(params))) {
        stop("params should have *either* phi, x, n0 *or* r, k")
    }
    if (alt_params) {
        ## translate params
        params <- with(as.list(params),
                       c(r=n0*phi, k=n0/x))
    }
    ode_res <- ode(y=c(chl=a0), ## starting state value, named
                   times=t,
                   func=log_grad,
                   parms=params)
    chl_res <- ode_res[,"chl"]
    cat("r,k:",params[1],params[2],"\n")
    plot.new()
    plot(t,chl_res)
    return(chl_res)
}






