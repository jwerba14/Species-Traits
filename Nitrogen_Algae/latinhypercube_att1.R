library("lhs")



temp <- data.frame (
  alpha =  0,
  beta = 0,
  omega = 0,
  death1 = 0,
  death2 = 0,
  pred_nh40 = 13,
  pred_chl0 = 43,
  gamma = 0,
  sd1 =  0 ,    
  sd2 =  0,
  loglik = 0,
  ttime  = 0
)



## create latin hypercube on scale that I want --- not full range of params but mostly-- didn't include most extreme values
set.seed(100)
hc <- improvedLHS(2000, 10)

## 
hc[,1] <- 1e-3 + hc[,1]
hc[,2] <- 1 +  100*hc[,2]
hc[,3] <- 0.001 + 700*hc[,3]
hc[,4] <- 0.3*hc[,4]
hc[,5] <- 0.002*hc[,5]
hc[,6] <- 13
hc[,7] <- 43
hc[,8]<- 1.5*hc[,8]
hc[,9]<- 8*hc[,9]
hc[,10] <- 20 + 80*hc[,10]

hc <- data.frame(hc)
names(hc) <- c("alpha","beta","omega","death1","death2","pred_nh40","pred_chl0","gamma","sd1","sd2")

for(i in 1:nrow(hc)) {
  newstart <- with(hc[i,], 
                   list(
                     alpha =  alpha,
                     beta = beta,
                     omega = omega,
                     death1 = death1,
                     death2 = death2,
                     pred_nh40 = pred_nh40,
                     pred_chl0 = pred_chl0,
                     gamma = gamma,
                     sd1 =  sd1,    
                     sd2 =  sd2
                   ))
  x <- system.time({
  tempm <- try((fitode(chl_nh4_mod, data= dat_nit_9, start = newstart, tcol = "date1", 
                       solver.opts = list(method="rk4", hini=0.1))),silent = TRUE)
  if (class(tempm) == "try-error") {
    temp[i,] <- "NA"
  } else {
    temp[i,1:10] <- coef(tempm)
    temp$loglik[i] <- logLik(tempm)
    
  }})
  temp$ttime[i] <- x[[3]]
  print(i/nrow(hc))
}

#n27 <- temp %>% filter(alpha != "NA")

n9 <- temp %>% filter(alpha != "NA")
n9 %>% filter(loglik == max(loglik))


