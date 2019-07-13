source("../transfer_functions.R")
library(deSolve)
# state variables given completely arbitary values but wanted to get names on them

param <- read.csv("parameters_no_cerio.csv")

state <- c(dammonium = 4.4,
           daph_j = 0,
           daph_a = 20,
           dalgae = 15
           
)

#parameters- will need parameters for each function
parameters <- c(
  alpha = 4.379716e-03, beta = 29,
  death1 = .007, death2 = 0.000082,
  gamma =  0.365 , omega = 10.5,
  growth_a = median(param$growth_a), growth_b = median(param$growth_b),
  death_b = median(param$death_b), a_feed_m = median(param$a_feed_m),
  a_exc_m = median(param$a_exc_m), j_feed_m = median(param$j_feed_m),
  j_exc_m = median(param$j_exc_m), amm_param_b = median(param$amm_param_b),
  fec_a = median(param$fec_a), fec_b = median(param$fec_b)
) 


# write ODE- return has to be in same order as state variable equations are listed

d_equations <-function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    # rate of change
    # ammonium is added from release by both juvenile and adult daphnia   
    ddammonium <- daph_a*.0004*a_feed_m*dalgae + daph_j*.0004*.037*dalgae -  ## need to fix excretion- not right
      
       dalgae*dammonium*alpha*omega/(omega+dammonium) + 
      
     # ((dammonium*alpha*omega/(omega+dammonium)) / 10) * (death1*dalgae + death2*(dalgae^2)) -
      
       gamma/2 * (death1*dalgae + death2*(dalgae^2)) - 
      
      # lagvalue(t - 5, 1) -
      
      (1-amm_param_b)*dammonium
    
    
  
    ddaph_j <- sat_fun(fec_a,fec_b,dalgae)*daph_a - daph_j*(35/70) 
    
    #daphnia adults are added from growth by juveniles and are lost from death
    ddaph_a <- 1/(sat_fun(growth_a,growth_b,dalgae))*daph_j- daph_a*(1/death_b)
    
    
  
    ddalgae <- beta * dalgae*dammonium*alpha*omega/(omega+dammonium) - death1*dalgae - death2*(dalgae^2)-
      daph_a*a_feed_m*dalgae - daph_j*j_feed_m*dalgae
    
    
    
    # return the rate of change
    list(c(ddammonium, ddaph_j,ddaph_a,ddalgae))
  }) # end with(as.list ...
}


out <- ode(y = state, times = seq(0,42,0.1), func = d_equations, parms = parameters)
plot(out)
tail(out)
