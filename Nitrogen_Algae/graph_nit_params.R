## graphing CI for algae and nitrogen parameters
library(tidyverse)
library(gridExtra)
source("../Graphing_Set_Up.R")
source("fit_functions.R")


d2 <- read.csv("Data/Algae_Nutrient.csv")
d2$ammonium <- d2$nh4*1000  ## put nh4 in ug so that it is one the same scale as chl
d2$algae <- d2$chl 



dd1 <- read.csv("alg_nit_param_CI.csv")
param_ll <- read.csv("param_ll.csv")
param_ll1 <- param_ll %>% pivot_longer(-c(X, rep, treatment, loglik), names_to = "param", values_to = "value")
param_ll2 <- param_ll1 %>% dplyr::select(treatment,param,value)

#write.csv(param_ll2, "alg_nit_loglik.csv")

dat1 <- dd1 %>% pivot_wider(c(-X), names_from = "quant", values_from = "value")
names(dat1) <- c("param", "treatment", "lwr","med", "upr")

dat2 <- left_join(dat1, param_ll2, by=c("param","treatment"))

ga <- ggplot(dat2, aes(value, as.factor(treatment))) + geom_point() + geom_errorbarh(aes(xmin = lwr, xmax = upr))+ 
  geom_point(aes(med, as.factor(treatment)), color = "red") +
  facet_wrap(~param, scales = "free") +
  ylab("Ammonium Treatment")  + ggtitle("Paramters: Nitrogen-Algae") + xlab("Value")

print(ga)

## graph fits 
parm <- read.csv("alg_nit_param_graph.csv")

param_ext <- parm %>% filter(treatment == 0.5)

param_med = unlist(param_ext[2, -c(1,2,3)])
param_upr = unlist(param_ext[3, -c(1,2,3)])
param_lwr = unlist(param_ext[1, -c(1,2,3)])
param_ll_b = unlist(param_ll[1, -c(1,7,8,9)])


state = c(ammonium = mean(d2[d2$treat == 0.5 & d2$date1 == 1, "ammonium"]), algae = mean(d2[d2$treat == 0.5 & d2$date1 == 1, "algae"]))
out <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param_med))
out2 <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param_ll_b))
names(out) <- c("date1", "ammonium","chl")
names(out2) <- c("date1", "ammonium", "chl")
t0.5_amm <- ggplot(out, aes(date1, ammonium)) + geom_point(data=d2[d2$treat == 0.5, ], aes(color = as.factor(rep))) + 
  geom_line(data = out, color = "red")+ geom_line(data = out2) + theme(legend.position = "NULL")  +xlab(" ") +ylab("")+ 
  ggtitle("Treatment: 0.5")

t0.5_alg <-ggplot(out, aes(date1, chl)) + geom_point(data=d2[d2$treat == 0.5, ], aes(color = as.factor(rep))) +  geom_line(data = out2)  +
  geom_line(data = out, color = "red")+ theme(legend.position = "NULL")  +xlab(" ") +ylab("")+ 
  ggtitle("Treatment: 0.5")


### treat 3 ###


param_ext <- parm %>% filter(treatment == 3)
param_med = unlist(param_ext[2, -c(1,2,3)])
param_upr = unlist(param_ext[3, -c(1,2,3)])
param_lwr = unlist(param_ext[1, -c(1,2,3)])
param_ll_b = unlist(param_ll[2, -c(1,7,8,9)])


state = c(ammonium = mean(d2[d2$treat == 3 & d2$date1 == 1, "ammonium"]), algae = mean(d2[d2$treat == 3 & d2$date1 == 1, "algae"]))
out <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param_med))
out2 <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param_ll_b))
names(out) <- c("date1", "ammonium","chl")
names(out2) <- c("date1", "ammonium", "chl")

t3_amm <-ggplot(out, aes(date1, ammonium)) + geom_point(data=d2[d2$treat == 3, ], aes(color = as.factor(rep))) + 
  geom_line(data = out, color = "red") + geom_line(data = out2) + theme(legend.position = "NULL") +xlab(" ") +ylab("")+ 
  ggtitle("Treatment: 3")

t3_alg <-ggplot(out, aes(date1, chl)) + geom_point(data=d2[d2$treat == 3, ], aes(color = as.factor(rep))) + geom_line(data = out2) +
  geom_line(data = out, color = "red") + theme(legend.position = "NULL")+xlab(" ") +ylab("")+ 
  ggtitle("Treatment: 3")


## treat 9 ##
  
param_ext <- parm %>% filter(treatment == 9)
param_med = unlist(param_ext[2, -c(1,2,3)])
param_upr = unlist(param_ext[3, -c(1,2,3)])
param_lwr = unlist(param_ext[1, -c(1,2,3)])
param_ll_b = unlist(param_ll[3, -c(1,7,8,9)])
  
  
state = c(ammonium = mean(d2[d2$treat == 9 & d2$date1 == 1, "ammonium"]), algae = mean(d2[d2$treat == 9 & d2$date1 == 1, "algae"]))
out <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param_med))
out2 <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param_ll_b))
names(out) <- c("date1", "ammonium","chl")
names(out2) <- c("date1", "ammonium", "chl")
  
t9_amm <- ggplot(out, aes(date1, ammonium)) + geom_point(data=d2[d2$treat == 9, ], aes(color = as.factor(rep))) + 
    geom_line(data = out, color = "red") + geom_line(data = out2) + theme(legend.position = "NULL")  +xlab(" ") +ylab("Ammonium (ug/L)")+ 
  ggtitle("Treatment: 9")
  
t9_alg <- ggplot(out, aes(date1, chl)) + geom_point(data=d2[d2$treat == 9, ], aes(color = as.factor(rep))) + geom_line(data = out2) +
  geom_line(data = out, color = "red")  + theme(legend.position = "NULL")  +xlab(" ") +ylab("Chl (ug/L)")+ 
  ggtitle("Treatment: 9")
  

## treat 27 ## 
param_ext <- parm %>% filter(treatment == 27)
param_med = unlist(param_ext[2, -c(1,2,3)])
param_upr = unlist(param_ext[3, -c(1,2,3)])
param_lwr = unlist(param_ext[1, -c(1,2,3)])
param_ll_b = unlist(param_ll[4, -c(1,7,8,9)])
  
  
state = c(ammonium = mean(d2[d2$treat == 27 & d2$date1 == 1, "ammonium"]), algae = mean(d2[d2$treat == 27 & d2$date1 == 1, "algae"]))
out <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param_med))
out2 <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param_ll_b))
names(out) <- c("date1", "ammonium","chl")
names(out2) <- c("date1", "ammonium", "chl")
  
t27_amm <-ggplot(out, aes(date1, ammonium)) + geom_point(data=d2[d2$treat == 27, ], aes(color = as.factor(rep))) + 
  geom_line(data = out, color = "red") + geom_line(data = out2) + theme(legend.position = "NULL") +xlab(" ") +ylab("")+ 
  ggtitle("Treatment: 27")
  
t27_alg <-ggplot(out, aes(date1, chl)) + geom_point(data=d2[d2$treat == 27, ], aes(color = as.factor(rep))) + geom_line(data = out2) +
 geom_line(data = out, color = "red") + theme(legend.position = "NULL")  +xlab(" ") +ylab("")+ 
  ggtitle("Treatment: 27") 
  
## treat 54 ##

param_ext <- parm %>% filter(treatment == 54)
param_med = unlist(param_ext[2, -c(1,2,3)])
param_upr = unlist(param_ext[3, -c(1,2,3)])
param_lwr = unlist(param_ext[1, -c(1,2,3)])

param_ll_b = unlist(param_ll[5, -c(1,7,8,9)])
 
 
state = c(ammonium = mean(d2[d2$treat == 54 & d2$date1 == 1, "ammonium"]), algae = mean(d2[d2$treat == 54 & d2$date1 == 1, "algae"]))
out <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param_med))
out2 <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param_ll_b))
names(out) <- c("date1", "ammonium","chl")
names(out2) <- c("date1", "ammonium", "chl")
 
t54_amm <- ggplot(out, aes(date1, ammonium)) + geom_point(data=d2[d2$treat == 54, ], aes(color = as.factor(rep))) + 
   geom_line(data = out, color = "red") + geom_line(data = out2) + theme(legend.position = "NULL") +xlab(" ") +ylab("")+ 
  ggtitle("Treatment: 54")
 
t54_alg <- ggplot(out, aes(date1, chl)) + geom_point(data=d2[d2$treat == 54, ], aes(color = as.factor(rep))) + geom_line(data = out2) +
 geom_line(data = out, color = "red") + theme(legend.position = "NULL") + xlab(" ") + ylab("") + ggtitle("Treatment: 54")  
  
 
 
## treat 108 ## 
 
param_ext <- parm %>% filter(treatment == 108)
param_med = unlist(param_ext[2, -c(1,2,3)])
param_upr = unlist(param_ext[3, -c(1,2,3)])
param_lwr = unlist(param_ext[1, -c(1,2,3)])
param_ll_b = unlist(param_ll[6, -c(1,7,8,9)])
 
 
state = c(ammonium = mean(d2[d2$treat == 108 & d2$date1 == 1, "ammonium"]), algae = mean(d2[d2$treat == 108 & d2$date1 == 1, "algae"]))
out <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param_med))
out2 <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param_ll_b))
names(out) <- c("date1", "ammonium","chl")
names(out2) <- c("date1", "ammonium", "chl")
 
t108_amm <- ggplot(out, aes(date1, ammonium)) + geom_point(data=d2[d2$treat == 108, ], aes(color = as.factor(rep))) + 
   geom_line(data = out, color = "red") + geom_line(data = out2) + theme(legend.position = "NULL") +xlab("Time") +
  ylab(" ") + ggtitle("Treatment: 108")
 
t108_alg <-ggplot(out, aes(date1, chl)) + geom_point(data=d2[d2$treat == 108, ], aes(color = as.factor(rep))) + geom_line(data = out2) +
 geom_line(data = out, color = "red") + theme(legend.position = "NULL") + theme(legend.position = "NULL") +
  xlab("Time") + ylab(" ") + ggtitle("Treatment: 108")

grid.arrange(t0.5_amm,t0.5_alg, t3_amm, t3_alg, t9_amm, t9_alg, t27_amm, t27_alg, t54_amm, t54_alg, t108_amm, t108_alg,
             ncol = 2) 
 
 
  
  
  
