library("lhs")
library("fitode")

chl_nh4_mod <- new("model.ode",
                   name = "algal_nit",
                   model = list(
                     pred_nh4 ~ -pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) + gamma *(death2*(pred_chl^2))-cammonium*pred_nh4 ,
                     pred_chl ~ beta * pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) - death2*(pred_chl^2)
                   ),
                   ## consider using bbmle::dnorm_n ?
                   observation = list(
                     nh4 ~ dnorm2(mean = pred_nh4),
                     chl ~ dnorm2(mean = pred_chl)
                   ),
                   initial = list(pred_nh4 ~ pred_nh40 , pred_chl ~ pred_chl0),
                   par=c("alpha", "beta", "omega", "death2", "pred_nh40", "pred_chl0", "gamma")
)




temp <- data.frame (
  alpha =  0,
  beta = 0,
  omega = 0,
  death2 = 0,
  pred_nh40 = 13,
  pred_chl0 = 43,
  gamma = 0,
  loglik = 0,
  ttime  = 0
)



## create latin hypercube on scale that I want --- not full range of params but mostly-- didn't include most extreme values


set.seed(100)
hc <- improvedLHS(300, 7)
hc <- data.frame(hc)



hc[,1] <- 0.005+0.006*hc[,1]
hc[,2] <- 14 +  18*hc[,2]
hc[,3] <- 12 + 16*hc[,3]
hc[,4] <- 0.002 + 0.003*hc[,4]
hc[,5] <- 0.0005 + 0.00073*hc[,5]
hc[,6] <- 13
hc[,7] <- 43
hc[,8]<- 0.08 + 0.11*hc[,8]


hc[,1] <- 0.004+0.008*hc[,1]
hc[,2] <- 12 +  20*hc[,2]
hc[,3] <- 8 + 20*hc[,3]
hc[,4] <- 0.001 + 0.009*hc[,4]
hc[,5] <- 13
hc[,6] <- 43
hc[,7] <- 0.03 + 0.06*hc[,7]


## chl 9
hc[,1] <- 0.002577731+0.004787215*hc[,1]
hc[,2] <- 24 +  45*hc[,2]
hc[,3] <- 147 + 273*hc[,3]
hc[,4] <- 0.0005 + 0.001*hc[,4]
hc[,5] <- 13
hc[,6] <- 43
hc[,7] <- 0.05 + 0.4*hc[,7]


cv <- vector(length= 300, mode = "list")
names(hc) <- c("alpha","beta","omega","death2","pred_nh40","pred_chl0","gamma")

for(i in 1:nrow(hc)) {
  newstart <- with(hc[i,], 
                   list(
                     alpha =  alpha,
                     beta = beta,
                     omega = omega,
                     death2 = death2,
                     pred_nh40 = pred_nh40,
                     pred_chl0 = pred_chl0,
                     gamma = gamma
                     
                   ))
  x <- system.time({
    tempm <- try((fitode(chl_nh4_mod, data= dat_nit_9, start = newstart, tcol = "date1", 
                         solver.opts = list(method="rk4", hini=0.1))),silent = TRUE)
    if (class(tempm) == "try-error") {
      temp[i,] <- "NA"
      cv[[i]] <- "NA"
      
    } else {
      temp[i,1:7] <- coef(tempm)
      temp$loglik[i] <- logLik(tempm)
      cv[[i]] <- vcov(tempm)
    }})
  
  temp$ttime[i] <- x[[3]]
  print(i/nrow(hc))
}




n27 <- temp %>% filter(alpha != "NA")

ngraph <- n27 %>% 
  gather(-loglik, key = "parameter", value = "value") %>%
  filter(parameter != "ttime") %>%
  filter(loglik > -500 )


ggplot(ngraph, aes(loglik,value)) + geom_point(aes(color= loglik)) + facet_wrap(~parameter, scales = "free_y") +scale_y_log10()

n9 <- read.csv("chl_9_30per.csv")
nngraph <- n9 %>% 
  gather(-loglik, key = "parameter", value = "value") %>%
  filter(parameter != "ttime" & parameter != "X") %>%
  filter(loglik > -500 )
ggplot(nngraph, aes(loglik,value)) + geom_point(aes(color= loglik)) + facet_wrap(~parameter, scales = "free_y") +scale_y_log10()

