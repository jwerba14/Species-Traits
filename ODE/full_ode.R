full_ODE <-function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    
    # ammonium is added from release by both juvenile and adult daphnia  and lost through algal update and nitritfication 
    dammonium <- -(a*ammonium)/(k+ammonium)*algae + l*death1*algae  + daph_a*xa*ha*algae + daph_j*xj*hj*algae #- .028*ammonium
    
    
    
    #juvenile daphia are added by density dependent (large daphnia) birth      
    ddaph_j <- daph_a*(b1*algae)/(b2+algae) - daph_j*death3 - daph_j*(1/g)
    
    
    
    #daphnia adults are added from growth by juveniles and are lost from death
    ddaph_a <- -daph_a*(1/death2) + daph_j*(1/g) 
    
    
    
    # algae is added from growth based on uptake of ammonium 
    dalgae <- (a*ammonium)/(k+ammonium)*algae*f - death1*algae - daph_a*ha*algae - daph_j*hj*algae
    
    
    
    # return the rate of change
    list(c(dammonium, ddaph_j,ddaph_a,dalgae))
  }) 
}


