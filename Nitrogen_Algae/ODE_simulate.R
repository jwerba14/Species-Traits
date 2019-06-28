library(deSolve)
# state variables given completely arbitary values but wanted to get names on them

state <- c(pred_nh4 = 6,
           pred_chl = 50
           )

#parameters- will need parameters for each function
parameters <- c(
  alpha = .4, beta = 10,
  death1 = 0.007,
  gamma = 1 , omega = .004,
  cammonium = 0.4 # proportional ammonium lost to env-- calc in nutrient_air.R
) 


# write ODE- return has to be in same order as state variable equations are listed

full_equations <-function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    # rate of change
    # ammonium is added from release by both juvenile and adult daphnia   
    dpred_nh4 <- -pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) + gamma *(death1*pred_chl)-cammonium*pred_nh4
    
    # algae is added from growth based on uptake of ammonium and nitrate #need to add contanst to mm
    dpred_chl <- beta * pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) - death1*pred_chl 
    
     # return the rate of change
    list(c(dpred_nh4,dpred_chl))
  }) # end with(as.list ...
}


out <- ode(y = state, times = seq(0,100,0.1), func = full_equations, parms = parameters)
plot(out)

out1 <- data.frame(out)

## dataframe of simulated data
simdat <- data.frame(
  time = rep(out1$time, 10),
  nh4 = rep(out1$pred_nh4, 10),
  chl = rep(out1$pred_chl,10),
  rep = rep(1,10, each = 124)
)

## add error
nh4_err = abs(rnorm(simdat$nh4,0,1))
chl_err = abs(rnorm(simdat$chl,0,1))

simdat$nh4 <- simdat$nh4+nh4_err
simdat$chl <- simdat$chl + chl_err


