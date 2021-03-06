library("lhs")
library("fitode")


temp <- data.frame (
  alpha =  0,
  beta = 0,
  omega = 0,
  death1 = 0,
  death2 = 0,
  pred_nh40 = 13,
  pred_chl0 = 43,
  gamma = 0,
  loglik = 0,
  ttime  = 0
)



## create latin hypercube on scale that I want --- not full range of params but mostly-- didn't include most extreme values
#set.seed(100)
#hc <- improvedLHS(200, 10)

## 
#hc[,1] <- 1e-3 + hc[,1]
#hc[,2] <- 1 +  100*hc[,2]
#hc[,3] <- 0.001 + 700*hc[,3]
#hc[,4] <- 0.3*hc[,4]
#hc[,5] <- 0.002*hc[,5]
#hc[,6] <- 13
#hc[,7] <- 43
#hc[,8]<- 1.5*hc[,8]

## create hypercube with smaller range +/- 10% of fitted values to see if I still get so few fits

set.seed(100)
hc <- improvedLHS(300, 10)
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
hc[,5] <- 0.0001 + 0.001*hc[,5]
hc[,6] <- 13
hc[,7] <- 43
hc[,8]<- 0.05 + 0.4*hc[,8]


##9 at 30%
hc[,1] <- 0.002+0.006*hc[,1]
hc[,2] <- 24 +  45*hc[,2]
hc[,3] <- 25 + 48*hc[,3]
hc[,4] <- 0.008 + 0.02*hc[,4]
hc[,5] <- 0.0005 + 0.001*hc[,5]
hc[,6] <- 5
hc[,7] <- 33
hc[,8]<- 0.02 + 0.06*hc[,8]


names(hc) <- c("alpha","beta","omega","death1","death2","pred_nh40","pred_chl0","gamma")


cv <- vector(length= 300, mode = "list")

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
                     gamma = gamma
                     
                   ))
  x <- system.time({
  tempm <- try((fitode(chl_nh4_mod, data= dat_nit_27, start = newstart, tcol = "date1", 
                       solver.opts = list(method="rk4", hini=0.1))),silent = TRUE)
  if (class(tempm) == "try-error") {
    temp[i,] <- "NA"
    cv[[i]] <- "NA"
  } else {
    temp[i,1:8] <- coef(tempm)
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

