## graphing nitrogen fits
library(tidyverse)
library(deSolve)
library(pomp)
source("../Graphing_Set_Up.R")
source("fit_functions.R")

d2 <- read.csv("Data/Algae_Nutrient.csv")
d2$ammonium <- d2$nh4*1000  ## put nh4 in ug so that it is one the same scale as chl
d2$algae <- d2$chl 

treat_0.5 <- readRDS("Fits_RDS/Treat_0.5_ll.RDS")
treat_3 <- readRDS("Fits_RDS/Treat_3_ll.RDS")
treat_9 <- readRDS("Fits_RDS/Treat_9_ll.RDS")
treat_27 <- readRDS("Fits_RDS/Treat_27_ll.RDS")
treat_54 <- readRDS("Fits_RDS/Treat_54_ll.RDS")
treat_108 <- readRDS("Fits_RDS/Treat_108_ll.RDS")

## make dataframe of all best loglik params
mm_0.5 <- treat_0.5 %>% group_by(a) %>% summarize(new_ll = sum(loglik))
z = mm_0.5[mm_0.5$new_ll == max(mm_0.5$new_ll), ]
param_ext_0.5 <- treat_0.5 %>% filter(a == z$a)
param_ext_0.5$treatment <- 0.5

mm3 <- treat_3 %>% group_by(a) %>% summarize(new_ll = sum(loglik))
z = mm3[mm3$new_ll == max(mm3$new_ll), ]
param_ext_3 <- treat_3 %>% filter(a == z$a)
param_ext_3$treatment <- 3


mm9 <- treat_9 %>% group_by(a) %>% summarize(new_ll = sum(loglik))
z = mm9[mm9$new_ll == max(mm9$new_ll), ]
param_ext_9 <- treat_9 %>% filter(a == z$a)
param_ext_9$treatment <- 9


mm27 <- treat_27 %>% group_by(a) %>% summarize(new_ll = sum(loglik))
z = mm27[mm27$new_ll == max(mm27$new_ll), ]
param_ext_27 <- treat_27 %>% filter(a == z$a)
param_ext_27$treatment <- 27


mm54 <- treat_54 %>% group_by(a) %>% summarize(new_ll = sum(loglik))
z = mm54[mm54$new_ll == max(mm54$new_ll), ]
param_ext_54 <- treat_54 %>% filter(a == z$a)
param_ext_54$treatment <- 54


mm108 <- treat_108 %>% group_by(a) %>% summarize(new_ll = sum(loglik))
z = mm108[mm108$new_ll == max(mm108$new_ll), ]
param_ext_108 <- treat_108 %>% filter(a == z$a)
param_ext_108$treatment <- 108


param_ll <- rbind(param_ext_0.5[1,],
                  param_ext_3[1,],
                  param_ext_9[1,],
                  param_ext_27[1,],
                  param_ext_54[1,],
                  param_ext_108[1,])

#write.csv(param_ll, file = "param_ll.csv")

## i guess take treatment 3 since actual experiment started at ~1.5
#mm3 <- treat_3 %>% group_by(a) %>% summarize(new_ll = sum(loglik))
#z = mm3[mm3$new_ll == max(mm3$new_ll), ]
#param_ext <- treat_3 %>% filter(a == z$a)
#alg_param = data.frame(unlist(param_ext[1, -c(6,7)]))
#names(alg_param) <- "value"
#alg_param$param <- rownames(param)
#write.csv(alg_param, file = "algal_parameters.csv")


## graph treat 0.5
mm_0.5 <- treat_0.5 %>% group_by(a) %>% summarize(new_ll = sum(loglik))
z = mm_0.5[mm_0.5$new_ll == max(mm_0.5$new_ll), ]
param_ext <- treat_0.5 %>% filter(a == z$a)
param = unlist(param_ext[1, -c(6,7)])
state = c(ammonium = mean(d2[d2$treat == 0.5 & d2$date1 == 1, "ammonium"]), algae = mean(d2[d2$treat == 0.5 & d2$date1 == 1, "algae"]))
out <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param))
names(out) <- c("date1", "ammonium","chl")

ggplot(out, aes(date1, ammonium)) + geom_point(data=d2[d2$treat == 0.5, ], aes(color = as.factor(rep))) + geom_line(data = out)

pp <- which(mm_0.5$new_ll > max(mm_0.5$new_ll)-3)
ll_0.5 <- treat_0.5[pp, ]

out_df <- data.frame(times = seq(0,11,0.1),
                    index = rep(seq(1, nrow(ll_0.5)), each = 111))

ammonium = data.frame(ammonium = numeric(length = nrow(out_df)))
algae = numeric(length = nrow(out_df))

for (i in 1:nrow(ll_0.5)){
  param_ext <- ll_0.5[i,]
  param = unlist(param_ext[, -c(6,7)])
  state = c(ammonium = mean(d2[d2$treat == 0.5 & d2$date1 == 1, "ammonium"]), algae = mean(d2[d2$treat == 0.5 & d2$date1 == 1, "algae"]))
  out_t <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param))
  out_t$index <- i
  
  if (i == 1) {
    out_t.f <- out_t
  } else {
    out_t.f <- rbind(out_t.f, out_t)
  }
  
}

names(out_t.f) <- c("date1", "ammonium","chl", "index")
out_t.f$loglik <- rep(ll_0.5$loglik, each =111)

t <- (max(out_t.f$loglik) -5)

out_t.f$cat <- as.numeric(0)
if(out_t.f$loglik > (t)){
  out_t.f$cat <- 1 } else {out_t.f$cat <- 2 }
  


g0.5_amm <- ggplot(out, aes(date1, chl)) + 
  geom_point(data = d2[d2$treat == 0.5, ]) + 
  geom_line(data = out_t.f) #, aes(color = as.factor(cat)))

plot(g0.5_amm)


## graph treat 
mm3 <- treat_3 %>% group_by(a) %>% summarize(new_ll = sum(loglik))
z = mm3[mm3$new_ll == max(mm3$new_ll), ]
param_ext <- treat_3 %>% filter(a == z$a)
param = unlist(param_ext[1, -c(6,7)])
state = c(ammonium = mean(d2[d2$treat == 3 & d2$date1 == 1, "ammonium"]), algae = mean(d2[d2$treat == 3 & d2$date1 == 1, "algae"]))
out <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param))
names(out) <- c("date1", "ammonium","chl")
ggplot(out, aes(date1, chl)) + geom_point(data = d2[d2$treat == 3, ], aes(color = as.factor(rep))) + geom_line(data = out)
ggplot(out, aes(date1, ammonium)) + geom_point(data=d2[d2$treat == 3, ]) + geom_line(data = out)


## graph treat 9
mm9 <- treat_9 %>% group_by(a) %>% summarize(new_ll = sum(loglik))
z = mm9[mm9$new_ll == max(mm9$new_ll), ]
param_ext <- treat_9 %>% filter(a == z$a)
param = unlist(param_ext[1, -c(6,7)])
state = c(ammonium = mean(d2[d2$treat == 9 & d2$date1 == 1, "ammonium"]), algae = mean(d2[d2$treat == 9 & d2$date1 == 1, "algae"]))
out <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param))
names(out) <- c("date1", "ammonium","chl")
ggplot(out, aes(date1, chl)) + geom_point(data = d2[d2$treat == 9, ], aes(color = as.factor(rep))) + geom_line(data = out)
ggplot(out, aes(date1, ammonium)) + geom_point(data=d2[d2$treat == 9, ]) + geom_line(data = out)

## graph treat 27
mm27 <- treat_27 %>% group_by(a) %>% summarize(new_ll = sum(loglik))
z = mm27[mm27$new_ll == max(mm27$new_ll), ]
param_ext <- treat_27 %>% filter(a == z$a)
param = unlist(param_ext[1, -c(6,7)])
state = c(ammonium = mean(d2[d2$treat == 27 & d2$date1 == 1, "ammonium"]), algae = mean(d2[d2$treat == 27 & d2$date1 == 1, "algae"]))
out <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param))
names(out) <- c("date1", "ammonium","chl")
ggplot(out, aes(date1, chl)) + geom_point(data = d2[d2$treat == 27, ], aes(color = as.factor(rep))) + geom_line(data = out)
ggplot(out, aes(date1, ammonium)) + geom_point(data=d2[d2$treat == 27, ]) + geom_line(data = out)

## graph treat 54
mm54 <- treat_54 %>% group_by(a) %>% summarize(new_ll = sum(loglik))
z = mm54[mm54$new_ll == max(mm54$new_ll), ]
param_ext <- treat_54 %>% filter(a == z$a)
param = unlist(param_ext[1, -c(6,7)])
state = c(ammonium = mean(d2[d2$treat == 54 & d2$date1 == 1, "ammonium"]), algae = mean(d2[d2$treat == 54 & d2$date1 == 1, "algae"]))
out <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param))
names(out) <- c("date1", "ammonium","chl")
ggplot(out, aes(date1, chl)) + geom_point(data = d2[d2$treat == 54, ], aes(color = as.factor(rep))) + geom_line(data = out)
ggplot(out, aes(date1, ammonium)) + geom_point(data=d2[d2$treat == 54, ]) + geom_line(data = out)


## graph treat 108
mm108 <- treat_108 %>% group_by(a) %>% summarize(new_ll = sum(loglik))
z = mm108[mm108$new_ll == max(mm108$new_ll), ]
param_ext <- treat_108 %>% filter(a == z$a)
param = unlist(param_ext[1, -c(6,7)])
state = c(ammonium = mean(d2[d2$treat == 108 & d2$date1 == 1, "ammonium"]), algae = mean(d2[d2$treat == 0.5 & d2$date1 == 1, "algae"]))
out <- data.frame(ode(y = state, times = seq(0,11,0.1), func = nit_ODE, parms = param))
names(out) <- c("date1", "ammonium","chl")
ggplot(out, aes(date1, chl)) + geom_point(data = d2[d2$treat == 108, ], aes(color = as.factor(rep))) + geom_line(data = out)
ggplot(out, aes(date1, ammonium)) + geom_point(data=d2[d2$treat == 108, ]) + geom_line(data = out)
