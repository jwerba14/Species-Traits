library(tidyverse)
library(rlist)
chl9_all <- read.csv(file = "chl_9_allparam_30.csv")
vcv9 <- readRDS(file = "chl9_allparams_30.RDS" )


chl27_all <- read.csv(file ="nh4_27_allparam_30.csv")
vcv27 <- readRDS(file = "nh4_27_allparam30_cov.RDS")

chl3_all <- read.csv(file = "Nh4_3_allparam_30.csv")
vcv3 <- readRDS(file = "Nh4_3_allparam30_cov.RDS")

chl54_all <- read.csv(file = "nh4_54_allparam_30.csv")
vcv54 <- readRDS(file = "nh4_54_allparam30_cov.RDS")

####9 

chl9_all$corrgood <- 0

for (i in 1:length(vcv9)) {
  if (vcv9[[i]] != "NA" & !is.na(vcv9[[i]][1])) {
    vcv9[[i]] <- cov2cor(vcv9[[i]]) 
  t <- length(which(vcv9[[i]] >1 ))
  v <- length(which(vcv9[[i]] < -1))
  if (t ==0 & v == 0)   {
    chl9_all$corrgood[i] <- 1 }
  }

}



chl9n <- chl9_all %>% filter(alpha != "NA" & loglik > -435)
with(chl9n, plot(loglik))

nrow(chl9n %>% filter(corrgood == 1))

nrow(chl9_all %>% filter(corrgood == 1))
chl <- chl9_all %>% filter(loglik != "NA") %>% filter(loglik > -432)
chl1 <- chl %>% filter(corrgood == 1)
chl2 <- chl %>% gather(key = "parameter", value = "value", -c(ttime,loglik,corrgood,X))
ggplot(chl2, aes(loglik, value)) + geom_point(aes(color= as.factor(corrgood))) + 
  facet_wrap(~parameter, scale = "free")
ggplot(chl, aes(alpha, beta)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(alpha, omega)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(alpha, death2)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(alpha, death1)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(alpha, gamma)) + geom_point(aes(color = as.factor(corrgood)))


ggplot(chl, aes(beta, omega)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(beta, death2)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(beta, death1)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(beta, gamma)) + geom_point(aes(color = as.factor(corrgood)))

ggplot(chl, aes(omega, death2)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(omega, death1)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(omega, gamma)) + geom_point(aes(color = as.factor(corrgood)))

ggplot(chl, aes(death2, death1)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(death2, gamma)) + geom_point(aes(color = as.factor(corrgood)))

ggplot(chl, aes(death1, gamma)) + geom_point(aes(color = as.factor(corrgood)))


## ones with best fit and correlations that make sense (chl1- seem to all cluster at values)
## take median parameters to plot and see...

med_param_9 <- chl1 %>% 
  gather(key = "parameter", value = "value", -c(ttime,loglik,corrgood,X)) %>%
  select (-c(ttime,loglik,corrgood,X))  %>%
  group_by(parameter) %>%
  summarize(parmmed = median(value))


newstart <- setNames(med_param_9$parmmed, as.character(med_param_9$parameter))
newstart[[8]] <- 8
newstart[[7]] <- 40


cammonium = 0.04085 # proportional ammonium lost to env-- calc in nutrient_air.R

chl_nh4_mod <- new("model.ode",
                   name = "algal_nit",
                   model = list(
                     pred_nh4 ~ -pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) + gamma *(death1*pred_chl + death2*(pred_chl^2))-cammonium*pred_nh4 ,
                     pred_chl ~ beta * pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) - death1*pred_chl - death2*(pred_chl^2)
                   ),
                   ## consider using bbmle::dnorm_n ?
                   observation = list(
                     nh4 ~ dnorm2(mean = pred_nh4),
                     chl ~ dnorm2(mean = pred_chl)
                   ),
                   initial = list(pred_nh4 ~ pred_nh40 , pred_chl ~ pred_chl0),
                   par=c("alpha", "beta", "omega", "death1","death2", "pred_nh40", "pred_chl0", "gamma")
)






ss <- ode.solve(chl_nh4_mod, 1:11, newstart,
                solver.opts=list(method="rk4", hini=0.1))

plot(dat_nit_9$date1, dat_nit_9$nh4, ylim=c(0, 30))
lines(ss@solution$pred_nh4)

plot(dat_nit_9$date1, dat_nit_9$chl)
lines(ss@solution$pred_chl)


## 27

chl27_all$corrgood <- 0

for (i in 1:length(vcv27)) {
  if (vcv27[[i]] != "NA" & !is.na(vcv27[[i]][1])) {
    vcv27[[i]] <- cov2cor(vcv27[[i]]) 
    t <- length(which(vcv27[[i]] >1 ))
    v <- length(which(vcv27[[i]] < -1))
    if (t ==0 & v == 0)   {
      chl27_all$corrgood[i] <- 1 }
  }
  
}



chl27 <- chl27_all %>% filter(alpha != "NA" & loglik > -440)
with(chl27, plot(loglik))



nrow(chl27_all %>% filter(corrgood == 1))

chl <- chl27_all %>% filter(loglik != "NA") %>% filter(loglik > -440)
chl1 <- chl %>% filter(corrgood == 1)
chl2 <- chl %>% gather(key = "parameter", value = "value", -c(ttime,loglik,corrgood,X))
ggplot(chl2, aes(loglik, value)) + geom_point(aes(color= as.factor(corrgood))) + 
  facet_wrap(~parameter, scale = "free")
ggplot(chl, aes(alpha, beta)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(alpha, omega)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(alpha, death2)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(alpha, death1)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(alpha, gamma)) + geom_point(aes(color = as.factor(corrgood)))


ggplot(chl, aes(beta, omega)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(beta, death2)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(beta, death1)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(beta, gamma)) + geom_point(aes(color = as.factor(corrgood)))

ggplot(chl, aes(omega, death2)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(omega, death1)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(omega, gamma)) + geom_point(aes(color = as.factor(corrgood)))

ggplot(chl, aes(death2, death1)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(death2, gamma)) + geom_point(aes(color = as.factor(corrgood)))

ggplot(chl, aes(death1, gamma)) + geom_point(aes(color = as.factor(corrgood)))


## ones with best fit and correlations that make sense (chl1- seem to all cluster at values)
## take median parameters to plot and see...

med_param_9 <- chl1 %>% 
  gather(key = "parameter", value = "value", -c(ttime,loglik,corrgood,X)) %>%
  select (-c(ttime,loglik,corrgood,X))  %>%
  group_by(parameter) %>%
  summarize(parmmed = median(value))


newstart <- setNames(med_param_9$parmmed, as.character(med_param_9$parameter))
newstart[[8]] <- 8
newstart[[7]] <- 40


cammonium = 0.04085 # proportional ammonium lost to env-- calc in nutrient_air.R

chl_nh4_mod <- new("model.ode",
                   name = "algal_nit",
                   model = list(
                     pred_nh4 ~ -pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) + gamma *(death1*pred_chl + death2*(pred_chl^2))-cammonium*pred_nh4 ,
                     pred_chl ~ beta * pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) - death1*pred_chl - death2*(pred_chl^2)
                   ),
                   ## consider using bbmle::dnorm_n ?
                   observation = list(
                     nh4 ~ dnorm2(mean = pred_nh4),
                     chl ~ dnorm2(mean = pred_chl)
                   ),
                   initial = list(pred_nh4 ~ pred_nh40 , pred_chl ~ pred_chl0),
                   par=c("alpha", "beta", "omega", "death1","death2", "pred_nh40", "pred_chl0", "gamma")
)






ss <- ode.solve(chl_nh4_mod, 1:11, newstart,
                solver.opts=list(method="rk4", hini=0.1))

plot(dat_nit_9$date1, dat_nit_9$nh4, ylim=c(0, 30))
lines(ss@solution$pred_nh4)

plot(dat_nit_9$date1, dat_nit_9$chl)
lines(ss@solution$pred_chl)


### 3
 
chl3_all$corrgood <- 0

for (i in 1:length(vcv3)) {
  if (vcv3[[i]] != "NA" & !is.na(vcv3[[i]][1])) {
    vcv3[[i]] <- cov2cor(vcv3[[i]]) 
    t <- length(which(vcv3[[i]] >1 ))
    v <- length(which(vcv3[[i]] < -1))
    if (t ==0 & v == 0)   {
      chl3_all$corrgood[i] <- 1 }
  }
  
}



chl3n <-chl3_all %>% filter(alpha != "NA" & loglik > -285)
with(chl3n, plot(loglik))

nrow(chl3_all %>% filter(corrgood == 1))

chl <- chl3_all %>% filter(loglik != "NA") %>% filter(loglik > -285)

chl2 <- chl %>% gather(key = "parameter", value = "value", -c(ttime,loglik,corrgood,X))
ggplot(chl2, aes(loglik, value)) + geom_point(aes(color= as.factor(corrgood))) + 
  facet_wrap(~parameter, scale = "free")
ggplot(chl, aes(alpha, beta)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(alpha, omega)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(alpha, death2)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(alpha, death1)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(alpha, gamma)) + geom_point(aes(color = as.factor(corrgood)))


ggplot(chl, aes(beta, omega)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(beta, death2)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(beta, death1)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(beta, gamma)) + geom_point(aes(color = as.factor(corrgood)))

ggplot(chl, aes(omega, death2)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(omega, death1)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(omega, gamma)) + geom_point(aes(color = as.factor(corrgood)))

ggplot(chl, aes(death2, death1)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(death2, gamma)) + geom_point(aes(color = as.factor(corrgood)))

ggplot(chl, aes(death1, gamma)) + geom_point(aes(color = as.factor(corrgood)))



med_param_3<- chl %>% 
  gather(key = "parameter", value = "value", -c(ttime,loglik,corrgood,X)) %>%
  select (-c(ttime,loglik,corrgood,X))  %>%
  group_by(parameter) %>%
  summarize(parmmed = median(value))


newstart <- setNames(med_param_3$parmmed, as.character(med_param_3$parameter))

ss <- ode.solve(chl_nh4_mod, 1:11, newstart,
                solver.opts=list(method="rk4", hini=0.1))

plot(dat_nit_3$date1, dat_nit_3$nh4, ylim=c(0, 30))
lines(ss@solution$pred_nh4)

plot(dat_nit_3$date1, dat_nit_3$chl)
lines(ss@solution$pred_chl)


###54

chl54_all$corrgood <- 0

for (i in 1:length(vcv54)) {
  if (vcv54[[i]] != "NA" & !is.na(vcv54[[i]][1])) {
    vcv54[[i]] <- cov2cor(vcv54[[i]]) 
    t <- length(which(vcv54[[i]] >1 ))
    v <- length(which(vcv54[[i]] < -1))
    if (t ==0 & v == 0)   {
      chl54_all$corrgood[i] <- 1 }
  }
  
}



chl54 <-chl54_all %>% filter(alpha != "NA" & loglik > -425)
with(chl54, plot(loglik))

nrow(chl54_all %>% filter(corrgood == 1))

chl <- chl54_all %>% filter(loglik != "NA") %>% filter(loglik > -425)

chl2 <- chl %>% gather(key = "parameter", value = "value", -c(ttime,loglik,corrgood,X))
ggplot(chl2, aes(loglik, value)) + geom_point(aes(color= as.factor(corrgood))) + 
  facet_wrap(~parameter, scale = "free")
ggplot(chl, aes(alpha, beta)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl[chl$omega < 200, ], aes(alpha, omega)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(alpha, death2)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(alpha, death1)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl[chl$gamma < 1000, ], aes(alpha, gamma)) + geom_point(aes(color = as.factor(corrgood)))


ggplot(chl[chl$omega < 200, ], aes(beta, omega)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(beta, death2)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl, aes(beta, death1)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl[chl$gamma < 1000, ], aes(beta, gamma)) + geom_point(aes(color = as.factor(corrgood)))

ggplot(chl[chl$omega < 200, ], aes(omega, death2)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl[chl$omega < 200, ], aes(omega, death1)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl[chl$gamma < 1000, ], aes(omega, gamma)) + geom_point(aes(color = as.factor(corrgood)))

ggplot(chl, aes(death2, death1)) + geom_point(aes(color = as.factor(corrgood)))
ggplot(chl[chl$gamma < 1000, ], aes(death2, gamma)) + geom_point(aes(color = as.factor(corrgood)))

ggplot(chl[chl$gamma < 1000, ], aes(death1, gamma)) + geom_point(aes(color = as.factor(corrgood)))


## ones with best fit and correlations that make sense (chl1- seem to all cluster at values)
## take median parameters to plot and see...

chl1 <- chl %>% filter(corrgood == 1) 
med_param_54a <- chl1 %>% 
  gather(key = "parameter", value = "value", -c(ttime,loglik,corrgood,X)) %>%
  select (-c(ttime,loglik,corrgood,X))  %>%
  group_by(parameter) %>%
  summarize(parmmed = median(value))


med_param_54 <- chl %>% 
  gather(key = "parameter", value = "value", -c(ttime,loglik,corrgood,X)) %>%
  select (-c(ttime,loglik,corrgood,X))  %>%
  group_by(parameter) %>%
  summarize(parmmed = median(value))


newstart <- setNames(med_param_54$parmmed, as.character(med_param_54$parameter))
newstart1 <- setNames(med_param_54a$parmmed, as.character(med_param_54a$parameter))


cammonium = 0.04085 # proportional ammonium lost to env-- calc in nutrient_air.R

chl_nh4_mod <- new("model.ode",
                   name = "algal_nit",
                   model = list(
                     pred_nh4 ~ -pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) + gamma *(death1*pred_chl + death2*(pred_chl^2))-cammonium*pred_nh4 ,
                     pred_chl ~ beta * pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) - death1*pred_chl - death2*(pred_chl^2)
                   ),
                   ## consider using bbmle::dnorm_n ?
                   observation = list(
                     nh4 ~ dnorm2(mean = pred_nh4),
                     chl ~ dnorm2(mean = pred_chl)
                   ),
                   initial = list(pred_nh4 ~ pred_nh40 , pred_chl ~ pred_chl0),
                   par=c("alpha", "beta", "omega", "death1","death2", "pred_nh40", "pred_chl0", "gamma")
)


ss <- ode.solve(chl_nh4_mod, 1:11, newstart,
                solver.opts=list(method="rk4", hini=0.1))

ss1 <- ode.solve(chl_nh4_mod, 1:11, newstart1,
                 solver.opts=list(method="rk4", hini=0.1))

plot(dat_nit_54$date1, dat_nit_54$nh4) #, ylim=c(0, 30))


plot(dat_nit_54$date1, dat_nit_54$nh4) #, ylim=c(0, 30))
lines(ss1@solution$pred_nh4)
lines(ss@solution$pred_nh4, col = "blue")



plot(dat_nit_54$date1, dat_nit_54$chl)
lines(ss1@solution$pred_chl)
lines(ss@solution$pred_chl, col= "blue")
