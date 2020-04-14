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


set.seed(100)
hc <- improvedLHS(300, 8)
hc <- data.frame(hc)



hc[,1] <- 0.005+0.006*hc[,1]
hc[,2] <- 14 +  18*hc[,2]
hc[,3] <- 12 + 16*hc[,3]
hc[,4] <- 0.002 + 0.003*hc[,4]
hc[,5] <- 0.0005 + 0.00073*hc[,5]
hc[,6] <- 13
hc[,7] <- 43
hc[,8]<- 0.08 + 0.11*hc[,8]

## 27 at 30%
hc[,1] <- 0.004+0.008*hc[,1]
hc[,2] <- 12 +  21.2*hc[,2]
hc[,3] <- 9 + 18.5*hc[,3]
hc[,4] <- 0.002 + 0.004*hc[,4]
hc[,5] <- 0.0004 + 0.0009*hc[,4]
hc[,6] <- 13
hc[,7] <- 40
hc[,8] <- 0.06 + 0.15*hc[,8]


## nh4 9 
hc[,1] <- 0.002577731+0.004787215*hc[,1]
hc[,2] <- 24 +  45*hc[,2]
hc[,3] <- 147 + 273*hc[,3]
hc[,4] <- 0.0005 + 0.001*hc[,4]
hc[,5] <- 13
hc[,6] <- 43
hc[,7] <- 0.05 + 0.4*hc[,7]


## nh4 9 after
hc[,1] <- 0.0025+0.005*hc[,1]
hc[,2] <- 24 +  45*hc[,2]
hc[,3] <- 1252 + 2326*hc[,3]
hc[,4] <- 0.0005 + 0.001*hc[,4]
hc[,5] <- 6
hc[,6] <- 34
hc[,7] <- 0.03 + 0.06*hc[,7]


## nh4 3 at 30%
hc[,1] <- 0.002+0.006*hc[,1]
hc[,2] <- 24 +  45*hc[,2]
hc[,3] <- 25+ 48*hc[,3]
hc[,4] <- 0.008 + 0.02*hc[,4]
hc[,5] <- 0.0005 + 0.0012*hc[,4]
hc[,6] <- 5
hc[,7] <- 33
hc[,8] <- 0.02 + 0.06*hc[,8]


##nh4 54 at 30%
hc[,1] <-  0.003+0.0062*hc[,1]
hc[,2] <- 4 +  8.5*hc[,2]
hc[,3] <- 13+ 25*hc[,3]
hc[,4] <- 0.004 + 0.009*hc[,4]
hc[,5] <- 1.482797e-08 + 2.753765e-08*hc[,4]
hc[,6] <- 25
hc[,7] <- 35
hc[,8] <- 2 + 5*hc[,8]



cv <- vector(length= 300, mode = "list")
names(hc) <- c("alpha","beta","omega","death1","death2","pred_nh40","pred_chl0","gamma")

for(i in 1:nrow(hc)) {
  newstart <- with(hc[i,], 
                   list(
                     alpha =  alpha,
                     beta = beta,
                     omega = omega,
                     death1=death1,
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

