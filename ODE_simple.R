# attempt to write system of differential equations without cerio or no3
## param as in a way that matches what data I actually took
source("transfer_functions.r")
library(deSolve)
# state variables given completely arbitary values but wanted to get names on them

state <- c(dammonium = 10,
           daph_j = 0,
           daph_a = 10,
           dalgae = 50
           
)

#parameters- will need parameters for each function
parameters <- c(
  a = 0.1,b = 0.1,  # daphnia adult release Nh4
  c= 0.01, d = 0.01, # daphnia juv release Nh4
  dj =3/70 , drj = 0.000001, # daphnia juv death
  dm =22.42625, drm = 0.03606, # daphnia adult death
  alpha = 4.379716e-03, beta = 2.335828e+01,
  death1 = 7.698981e-03, death2 = 3.331607e-04,
  gamma = 1.193833e-01, omega = 1.663828e+01,
  z =9.828, w = 20.512, # birth of juvenile daphnia
  cammonium = 0.04085, # proportional ammonium lost to env-- calc in nutrient_air.R
  l =.000026, m= -448,  # uptake of algae by juvenile hollings type II
  j = 0.0000939, k= 10.7 # uptake of algae by adult daphnia hollings type II
) 


# write ODE- return has to be in same order as state variable equations are listed

full_equations <-function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    # rate of change
    # ammonium is added from release by both juvenile and adult daphnia   
    dammonium <- daph_a*sat_fun(a,b,dalgae) + daph_j*sat_fun(c,d,dalgae)+ 
       
      # also add a constant released based on density dependent death
      daph_j*dj*drj + daph_a*death(dm,daph_a)*drm - 
      dalgae*dammonium*alpha*omega/(omega+dammonium) + gamma *(death1*dalgae + death2*(dalgae^2))-cammonium*dammonium
    
    
    
    
    
    #juvenile daphia are added by density dependent (large daphnia) birth      
    ddaph_j <- sat_fun(z,w,dalgae)*daph_a -
      #loss through death and growth -where they reach a certain threshold size --- don't know how to put that in
      daph_j*dj - sat_fun(l,m,dalgae)*daph_j
    
    #daphnia adults are added from growth by juveniles and are lost from death
    ddaph_a <- sat_fun(l,m,dalgae)*daph_j- daph_a*death(dm,daph_a)
    
    
    
    # algae is added from growth based on uptake of ammonium and nitrate #need to add contanst to mm
    dalgae <- beta * dalgae*dammonium*alpha*omega/(omega+dammonium) - death1*dalgae - death2*(dalgae^2)
      # algae is lost from being eaten by each of the 3 zooplankon
      -daph_a*hollings2(j,k,dalgae) + daph_j*hollings2(l,m,dalgae)
      
    
    
    # return the rate of change
    list(c(dammonium, ddaph_j,ddaph_a,dalgae))
  }) # end with(as.list ...
}


out <- ode(y = state, times = seq(0,100,0.1), func = full_equations, parms = parameters)
plot(out)
