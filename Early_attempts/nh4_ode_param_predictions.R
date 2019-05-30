library(deSolve) # library for solving differential equations
library(minpack.lm)

diff_eq <- function (t, state, params, ...) {
  
  state_update <- list(S = state[1], 
                       I = matrix partitioning the rest of state)
  
  with(as.list(c(params, state_update)), {
    
    dS <- stuff
    
    dI <- stuff
    
    list(c(dS, dI))
  }
  )
  
}



# parameters 
param <- c(
  da = 0.03,
  t = 1,
  h = .0005,
  cammonium = .0000000001
  
)
#

state <- c(ammonium = dat$nh4[1:30],
           algae = dat$chl[1:30])
           


#known - algae =chl, ammonium = nh4
trial <-function(t, state, param) {

  state_update <- list(ammonium = matrix(state[1:30]), 
                       algae = matrix(state[31:60]))
  
    with(as.list(c(state_update, param)),{
      dammonium <- 
        # also add a constant released based on density dependent death
        algae*da-
        # lose from uptake and a density dependent (?) from air
        algae*mich_men(t,h,ammonium) #- cammonium*ammonium      
      
dalgae <- algae*mich_men(t,h,ammonium) -
  # density dependent death
  algae*da


# return the rate of change
list(c(dammonium,dalgae))
  }) # end with(as.list ...
}

dat_nh4 <- data.frame(dat %>%
  select(nh4,uni,date1) %>%
  group_by(date1) %>%
  spread(key = uni,nh4))
dat_nh4 <- dat_nh4 %>%
  select(-date1)

dat_chl <- dat %>%
  select(chl,uni,date1) %>%
  group_by(date1) %>%
  spread(key = uni,chl) 

dat_chl <- dat %>%
  select(-date1)

out <- ode(y = state, times = seq(0,100,0.1), func = trial, parms = param)

ssq <- function(par) {
  out <- ode(y = state, times = seq(0,100,0.1), func = trial, parms = param)
  outdf <- data.frame(out)
  #take only time points that match my data (so 11 points)
  outdf1 <- outdf %>%
    filter(time <= 1)
  out_nh4 <- outdf1 %>%
    select(starts_with("ammo")) ## bah not working
 
   # Evaluate predicted vs experimental residual
  
  ssqres1 = outdf1$ammonium- dat$nh4
  ssqres2 = outdf1$algae - dat$chl
  
  
  # return predicted vs experimental residual
  return(ssqres1) 
}



dat_nh4 <- dat %>%
  select(nh4,uni,date1) %>%
  group_by(date1) %>%
  spread(key = uni,nh4)

dat_chl <- dat %>%
  select(chl,uni,date1) %>%
  group_by(date1) %>%
  spread(key = uni,chl)

#parameter fitting using levenberg marquart algorithm
# initial guess for parameters
parms=c(
  da = 0.0003,
  t = 20,
  h = 5,
  cammonium = .1
  
)
# fitting
fitval=nls.lm(par=parms, fn=ssq)  






melt(outdf,id.var="time",variable.name="species",value.name="conc")
expdf=melt(df,id.var="time",variable.name="species",value.name="conc")
ssqres=preddf$conc-expdf$conc



