full_ODE <-function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    
    # ammonium is added from release by both juvenile and adult daphnia  and lost through algal update and nitritfication 
    dammonium <- -(a*ammonium)/(k+ammonium)*algae + l*death1*algae  + daph_a*xa + daph_j*xj
    
    
    
    
    
    #juvenile daphia are added by density dependent (large daphnia) birth      
    ddaph_j <- daph_a*(b1*algae)/(b2+algae) - daph_j*death3
    
    
    
    #daphnia adults are added from growth by juveniles and are lost from death
    ddaph_a <- -daph_a*(1/death2) + daph_j*(1/g) 
    
    
    
    # algae is added from growth based on uptake of ammonium 
    dalgae <- (a*ammonium)/(k+ammonium)*algae*f - death1*algae- daph_a*ha - daph_j*hj
    
    
    
    # return the rate of change
    list(c(dammonium, ddaph_j,ddaph_a,dalgae))
  }) 
}


