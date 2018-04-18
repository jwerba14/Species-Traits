
## mich men growth
mikmen <- function(a, s, nh4) {
  # nh4 <- seq(0.01, 40, by = 0.1)
  a * nh4 / (s + nh4)
  # plot(nh4, out)
}

#decelerating death function
death <- function(a,b,chl){
  a*chl^(1/b)
}

#accelerating death function
death2 <- function(a, chl,int){
  (a*chl^2)+int
}

par(mfrow = c(1,1));plot(death2(0.00005, seq(40, 200, by = 10), 0.5)) 
par(mfrow = c(1,1));plot(death(0.01, 10, seq(40, 200, by = 10))) 
par(mfrow = c(1,1)); plot(mikmen(.31, 684, seq(1, 40, by = 1)))
