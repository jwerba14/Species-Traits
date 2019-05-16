# attempt to write system of differential equations
source("transfer_functions.r")
library(deSolve)
# state variables given completely arbitary values but wanted to get names on them

state <- c(ammonium = 5,
           nitrate = 5,
           daph_j = 10,
           daph_a = 10,
           cerio = 20,
           algae = 50
  
)

#parameters- will need parameters for each function
parameters <- c(
  a = 0.1,b = 0.1,  # daphnia adult release Nh4
  c= 0.01, d = 0.01, # daphnia juv release Nh4
  e = 0.001, f =0.001, #cerio release Nh4
  dj =0.000001 , drj = 0.000001, # daphnia juv death
  dm = 0.00001, drm = 0.00001, # daphnia adult death
  dc = 0.00001, drc = 0.0001, # cerio death
  da = 0.001, dra = 0.001, # algae death
  t = 0.01, h = 0.01, # mich men uptake nh4
  cammonium = .0001, # ammonium lost to env-- calc in nutrient_air.R
  n = .0001, # nitrification constant  -- calc in nutrient_air.R
  p= .001, q = .001, # mich men uptake nitrate
  cnitrate = .000001, # nitrate lost to env-- calc in nutrient_air.R
  z =0.001, w = 0.0001, # birth of juvenile daphnia
  l =.000026, m= -448,  # uptake of algae by juvenile hollings type II
  g = 0.0001, i= 0.001, # birth of cerio
  j = 0.0000939, k= 10.7, # uptake of algae by adult daphnia hollings type II
  y= .0000001, r = -500 # uptake of algae by cerio hollings type II (prob need to change to linear)
  ) 




# write ODE- return has to be in same order as state variable equations are listed

 full_equations <-function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
  # rate of change
        # ammonium is added from release by all 3 zoo species (each times a constant?)   
      dammonium <- daph_a*sat_fun(a,b,algae) + daph_j*sat_fun(c,d,algae)+ 
        cerio*sat_fun(e,f,algae)*y +
        # also add a constant released based on density dependent death
        daph_j*death(dj,daph_j)*drj + daph_a*death(dm,daph_a)*drm + cerio*death(dc,cerio)*drc - 
        algae*death(da,algae)*dra-
        # lose from uptake and a density dependent (?) from air
        algae*mich_men(t,h,ammonium) - cammonium*ammonium
      
      
        # nitrate is added by nitrification from ammonium at a constant rate
      dnitrate <- n*ammonium -
        # nitrate is loss to air at a density dependent rate and from uptake by algae
        algae*mich_men(p,q, nitrate) - cnitrate*nitrate
      
        #juvenile daphia are added by density dependent (large daphnia) birth      
      ddaph_j <- sat_fun(z,w,algae)*daph_a -
        #loss through death and growth -where they reach a certain threshold size --- don't know how to put that in
        daph_j*death(dj,daph_j) - sat_fun(l,m,algae)*daph_j
      
        #daphnia adults are added from growth by juveniles and are lost from death
      ddaph_a <- sat_fun(l,m,algae)*daph_j- daph_a*death(dm,daph_a)
        
        #ceriodaphnia are added by birth and are lost from death
      dcerio <- sat_fun(g,i,algae)*cerio - cerio*death(dc, cerio)
     
        # algae is added from growth based on uptake of ammonium and nitrate #need to add contanst to mm
       dalgae <- algae*mich_men(t,h,ammonium) + algae*mich_men(p,q, nitrate) 
        # algae is lost from being eaten by each of the 3 zooplankon
         -daph_a*hollings2(j,k,algae) + daph_j*hollings2(l,m,algae)+cerio*hollings2(y,r,algae) -
         # density dependent death
         algae*death(da,algae)
      
       
        # return the rate of change
         list(c(dammonium, dnitrate, ddaph_j,ddaph_a,dcerio,dalgae))
       }) # end with(as.list ...
 }
 
 
 out <- ode(y = state, times = seq(0,100,0.1), func = full_equations, parms = parameters)
 plot(out)
