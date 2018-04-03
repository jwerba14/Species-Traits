# attempt to write system of differential equations

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
parameters <- c(x,x,x)




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
     
        # algae is added from growth based on uptake of ammonium and nitrate
       dalgae <- algae*mich_men(t,h,ammonium) + algae*mich_men(p,q, nitrate) -
        # algae is lost from being eaten by each of the 3 zooplankon
         -daph_a*hollings2(j,k,algae) + daph_j*hollings2(l,m,algae)+cerio*hollings2(y,r,algae) -
         # density dependent death
         algae*death(da,algae)
      
       
        # return the rate of change
         list(c(dammonium, dnitrate, ddaph_j,ddaph_a,dcerio,dalgae))
       }) # end with(as.list ...
   }
