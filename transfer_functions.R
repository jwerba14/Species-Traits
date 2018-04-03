#possible transfer functions

# all feeding on algae by zooplankton is saturating- hollings type II (could be linear...)
hollings2 <- function (a,h,r) {
  (a*h)/(1+a*h*r)
}

#Holling's type 1 is  linear with respect to prey density (a) so just multiply by constant

# algae uptake of nutrients is saturating- michaelis menten
#where S is concentration of nutrient
mich_men <- function(vmax, s, k) {
  
  vmax * s / (k + s)
  
}

# death functions- going to assume non-linear density dependent

# where g is the state variable- then this is multiplied by the state variable e*g*g
death <- function (e,g) {
  e*g 
}

# saturating function for nutrient release where K is independent state variable
sat_fun <- function(a,b,k) {
  
  a * b / (k + b)
  
}