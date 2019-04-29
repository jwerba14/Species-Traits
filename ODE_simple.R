# attempt to write system of differential equations without cerio or no3
## param as in a way that matches what data I actually took
source("transfer_functions.r")
library(deSolve)
# state variables given completely arbitary values but wanted to get names on them

state <- c(ammonium = 5,
           daph_j = 10,
           daph_a = 10,
            algae = 50
           
)

#parameters- will need parameters for each function
parameters <- c(
  a = 0.1,b = 0.1,  # daphnia adult release Nh4
  c= 0.01, d = 0.01, # daphnia juv release Nh4
  dj =0.000001 , drj = 0.000001, # daphnia juv death
  dm = 0.00001, drm = 0.00001, # daphnia adult death
  da = 0.001, dra = 0.001, # algae death
  t = 0.01, h = 0.01, # mich men uptake nh4
  cammonium = .000001 + .0001, # ammonium lost to env-- calc in nutrient_air.R and # nitrification constant  -- calc in nutrient_air.R ##  combined with cammonium since no longer tracking no3
  z =0.001, w = 0.0001, # birth of juvenile daphnia
  l =.000026, m= -448,  # uptake of algae by juvenile hollings type II
  j = 0.0000939, k= 10.7 # uptake of algae by adult daphnia hollings type II
) 




# write ODE- return has to be in same order as state variable equations are listed

full_equations <-function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    # rate of change
    # ammonium is added from release by both juvenile and adult daphnia   
    dammonium <- daph_a*sat_fun(a,b,algae) + daph_j*sat_fun(c,d,algae)+ 
       
      # also add a constant released based on density dependent death
      daph_j*death(dj,daph_j)*drj + daph_a*death(dm,daph_a)*drm - 
      algae*death(da,algae)*dra-
      # lose from uptake and a density dependent (?) from air
      algae*mich_men(t,h,ammonium) - cammonium*ammonium
    
    
    
    
    
    #juvenile daphia are added by density dependent (large daphnia) birth      
    ddaph_j <- sat_fun(z,w,algae)*daph_a -
      #loss through death and growth -where they reach a certain threshold size --- don't know how to put that in
      daph_j*death(dj,daph_j) - sat_fun(l,m,algae)*daph_j
    
    #daphnia adults are added from growth by juveniles and are lost from death
    ddaph_a <- sat_fun(l,m,algae)*daph_j- daph_a*death(dm,daph_a)
    
    
    
    # algae is added from growth based on uptake of ammonium and nitrate #need to add contanst to mm
    dalgae <- algae*mich_men(t,h,ammonium) -
      # algae is lost from being eaten by each of the 3 zooplankon
      -daph_a*hollings2(j,k,algae) + daph_j*hollings2(l,m,algae) -
      # density dependent death
      algae*death(da,algae)
    
    
    # return the rate of change
    list(c(dammonium, ddaph_j,ddaph_a,dalgae))
  }) # end with(as.list ...
}


out <- ode(y = state, times = seq(0,100,0.1), func = full_equations, parms = parameters)
plot(out)
