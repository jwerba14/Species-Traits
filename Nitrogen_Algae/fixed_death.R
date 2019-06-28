## fixed death
set.seed(100)
hc <- improvedLHS(300, 6)
hc <- data.frame(hc)

temp <- data.frame (
  alpha =  0,
  beta = 0,
  omega = 0,
  pred_nh40 = 13,
  pred_chl0 = 43,
  gamma = 0,
  loglik = 0,
  ttime  = 0
)


## 27 at 30%
hc[,1] <- 0.004+0.008*hc[,1]
hc[,2] <- 12 +  21.2*hc[,2]
hc[,3] <- 9 + 18.5*hc[,3]
hc[,4] <- 13
hc[,5] <- 40
hc[,6] <- 0.06 + 0.15*hc[,6]

## death parameters from best fit with all params for 27

cammonium = 0.04085 # proportional ammonium lost to env-- calc in nutrient_air.R

chl_nh4_mod_nod <- new("model.ode",
                        name = "algal_nit",
                        model = list(
                          pred_nh4 ~ -pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) + gamma *(.002*pred_chl + 0.00058*pred_chl^2 )-cammonium*pred_nh4,
                          pred_chl ~ beta * pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) - 0.002*pred_chl-  0.00058*pred_chl^2
                        ),
                        ## consider using bbmle::dnorm_n ?
                        observation = list(
                          nh4 ~ dnorm2(mean = pred_nh4),
                          chl ~ dnorm2(mean = pred_chl)
                        ),
                        initial = list(pred_nh4 ~ pred_nh40 , pred_chl ~ pred_chl0),
                        par=c("alpha", "beta", "omega", "pred_nh40", "pred_chl0", "gamma")
)




cv <- vector(length= 300, mode = "list")
names(hc) <- c("alpha","beta","omega","pred_nh40","pred_chl0","gamma")

for(i in 1:nrow(hc)) {
  newstart <- with(hc[i,], 
                   list(
                     alpha =  alpha,
                     beta = beta,
                     omega = omega,
                     pred_nh40 = pred_nh40,
                     pred_chl0 = pred_chl0,
                     gamma = gamma
                     
                   ))
  x <- system.time({
    tempm <- try((fitode(chl_nh4_mod_nod, data= dat_nit_27, start = newstart, tcol = "date1", 
                         solver.opts = list(method="rk4", hini=0.1))),silent = TRUE)
    if (class(tempm) == "try-error") {
      temp[i,] <- "NA"
      cv[[i]] <- "NA"
      
    } else {
      temp[i,1:6] <- coef(tempm)
      temp$loglik[i] <- logLik(tempm)
      cv[[i]] <- vcov(tempm)
    }})
  
  temp$ttime[i] <- x[[3]]
  print(i/nrow(hc))
}

