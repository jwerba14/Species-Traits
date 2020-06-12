## read in Daphnia parameters
## this script needs to be broken up
library(gridExtra)
library(deSolve)
library(tidyverse)
source("../Graphing_Set_Up.R")
source("clean_data_full.R")

daph <- read.csv("../Daphnia/daphnia_params.csv")

daph <- daph %>% dplyr::select(-X)


alg_param <- read.csv("../Nitrogen_Algae/alg_nit_loglik.csv")
alg_param <- alg_param %>% dplyr::select(-X)
alg_param$method <- "dat_only"
alg_param <- alg_param %>% filter(treatment == "9") %>% dplyr::select(-c(X, treatment))
alg_param$quant <- "median"

state = c(
  ammonium = 5000,
  daph_j = 0,
  daph_a = 2,
  algae  = 15
)

## full ode
source("full_ode.R")
source("functions_ll.R")

parameters <- rbind(daph, alg_param)

gg <- expand.grid(
  b1 = c("wide","lit", "mm", "hyper","conb1"),
  b2 = c("wide","lit", "mm", "hyper","conb1"),
  ha = c("wide", "mixvs","mmimp","litimp","litvs"),
  xa = c("wide", "mixvs","mmimp"),
  hj = "wide",
  g = "wide",
  xj = "wide",
  a = "dat_only",
  k = "dat_only",
  l = "dat_only",
  death1 = "dat_only",
  f = "dat_only",
  death2 =  c("unweighted","weight_sd","weight_rep", "wide","informed")
  
)

## because b1 and b2 are estimated together only want rows where they are the same method


gg2 <- gg %>% filter(b1 == b2)



for (i in 1:nrow(gg2)){
gg.t <- pivot_longer(gg2[i, ], everything(), names_to = 'param', values_to = "method_select")

pp <- parameters %>% filter(
  param %in% names(gg2),
) %>% group_by(param) %>%
  left_join(., gg.t) %>% 
  filter(method == method_select) %>%
  filter(quant == "median") %>%
  dplyr::select(param, value)

pp <- pp %>% pivot_wider(names_from = param, values_from = value)
pp <- as.list(pp)
death3 <- list(death3 = 0.25)
pp <- c(pp, death3)


tt <- ode(
  func=full_ODE,
  y=state,   
  times=seq(1,40),
  parms=pp,
  method = "rk4",
  hini=0.1,
  maxstep = 100000
) 

 tt1 <- as.data.frame(tt)
 tt1$index <- i
  if (i == 1) {
  tt.g <- tt1 
} else {
  tt.g <- rbind(tt.g,tt1)
}
 
}


tt.g1 <- tt.g %>% filter(time == 40)  ## all unique combinations run the whole way

tt.f <- tt.g %>% filter(index %in% tt.g1$index) %>% filter(!is.na(algae)) %>% droplevels() ##hmm only 7290 complete cases....



## first for undisturbed, all states present
dd_all_u <- dd %>% filter(treatment == 3)
pop_all_u <- pop_fin1 %>% filter(treatment == 3)


ll <- data.frame(ll = numeric(length = nrow(tt.g1)*length(unique(dd_all_u$TankNum))),
                 index = numeric(length = nrow(tt.g1)*length(unique(dd_all_u$TankNum))),
                 rep = numeric(length = nrow(tt.g1)*length(unique(dd_all_u$TankNum))))
uni_vec <- as.vector(unique(tt.f$index))

tank_vec <- as.vector(unique(dd_all_u$TankNum))

## find loglik for each fit for each combination
qq <- 1
for (k in 1:nrow(ll)) {
  
for(j in 1:length(unique(tank_vec))){
  
  dd1 <- dd_all_u %>% filter(TankNum == tank_vec[j])
  pop_fin2 <- pop_all_u %>% filter(TankNum == tank_vec[j])
  
for (i in 1:length(uni_vec)) {
  
  tt.f1 <- tt.f %>% filter(index == uni_vec[i])
  ll$ll[qq] <- loglik2(tt.f1)
  ll$index[qq] <- i
  ll$rep[qq] <- j
  qq <- qq + 1
  }
 }
}

## add up loglik by index so that can find the best set of parameters

#write.csv(ll, "loglik_full.csv")
#ll <- read.csv("loglik_full.csv")

 


ll2 <- ll %>% group_by(as.factor(index)) %>% summarise(sum_ll = sum(ll, na.rm = T))

z <- which(ll2$sum_ll == max(ll2$sum_ll))

gg.best <- data.frame(
  param         = unlist(names(gg2[z, ]))
, method_select = unlist(gg2[z, ])
)

best_parm <- parameters %>% # filter(
#  param %in% names(gg2[z, ]),
#) %>% 
  group_by(param) %>%
  left_join(., gg.best) %>% 
  filter(method == method_select) %>%
  filter(quant == "median") %>%
  dplyr::select(param, value)

best_parm <- best_parm %>% pivot_wider(names_from = param, values_from = value)
best_parm <- as.list(best_parm)
death3 <- list(death3 = .2)
best_parm1 <- c(best_parm, death3) 

best_parm1$ha[1] <- best_parm1$ha[1]
best_parm1$l[1] <- 0.70791971
best_parm1$f[1] <- 0.05

out_best <- ode(
  func=full_ODE,
  y=state,   
  times=seq(1,40),
  parms=best_parm1,
  method = "rk4",
  hini=0.01,
  maxstep = 10000
) 

out_best <-data.frame(out_best)

names(out_best) <- c("times", "ammonium","Juv","Adult", "algae")

best_g_amm <- ggplot(dd_all_u, aes(times, ammonium)) + geom_point() + geom_line(data = out_best)
print(best_g_amm)


best_g_alg <- ggplot(dd_all_u[dd_all_u$algae < 250, ], aes(times, algae)) + geom_point() + geom_line(data = out_best) ## removes two points that seem way out of range
print(best_g_alg)

best_g_daphA <- ggplot(pop_all_u, aes(times, Adult)) + geom_point() + geom_line(data = out_best) ## removes two points that seem way out of range
print(best_g_daphA)

best_g_daphJ <- ggplot(pop_all_u, aes(times, Juv)) + geom_point() + geom_line(data = out_best) ## removes two points that seem way out of range
print(best_g_daphJ)

#wide_only <- [ ,]
#lit_only <- 



## for algae only ## hmm loop isn't necessary necessary because only 1 set of algal params... only estimated in one way.
state = c(
  ammonium = 5500,
  daph_j = 0,
  daph_a = 0,
  algae  = 15
)



out_alg <- ode(
  func=full_ODE,
  y=state,   
  times=seq(1,40),
  parms=best_parm1,
  method = "rk4",
  hini=0.1,
  maxstep = 10000
) 

##algal only tanks
dd_algae <- dd %>% filter(treatment == 1)

out_alg <- data.frame(out_alg)
names(out_alg)[1] <- "times"

alg_g_amm <- ggplot(dd_algae, aes(times, ammonium)) + geom_point() + geom_line(data = out_alg)
print(alg_g_amm)

  
alg_g_alg <- ggplot(dd_algae[dd_algae$algae < 600, ], aes(times, algae)) + geom_point() + geom_line(data = out_alg) ## removes two points that seem way out of range
print(alg_g_alg)



## for all disturbed

## for algae disturbed




# run with params from our lab data only
daph_med <- daph %>% filter(method == "dat_only") %>% filter(quant == "median") %>% dplyr::select(param, value)
val.list <- daph_med$value
parameters_med <- c(as.character(daph_med$param))
names(val.list) <- parameters_med
par_m <- c(alg_param,val.list)
out_med <- ode(y = state, times = seq(0,42,0.1), func = full_ODE, parms = par_m)

daph_upr <- daph %>% filter(method == "dat_only") %>% filter(quant == "upr") %>% dplyr::select(param, value)
val.list1 <- daph_upr$value
parameters_upr <- c(as.character(daph_upr$param))
names(val.list1) <- parameters_upr
par_u <- c(alg_param,val.list1)
out_upr <- ode(y = state, times = seq(0,42,0.1), func = full_ODE, parms = par_u)


daph_lwr <- daph %>% filter(method == "dat_only") %>% filter(quant == "lwr") %>% dplyr::select(param, value)
val.list2 <- daph_lwr$value
parameters_lwr <- c(as.character(daph_lwr$param))
names(val.list2) <- parameters_lwr
par_l <- c(alg_param,val.list2)
out_lwr <- ode(y = state, times = seq(0,42,0.1), func = full_ODE, parms = par_l)


## run with params from literature only (where exists)
daph_medl <- daph %>% filter(method == "lit_only") %>% filter(quant == "median") %>% dplyr::select(param, value)
val.listl <- daph_medl$value
parameters_medl <- c(as.character(daph_medl$param))
names(val.listl) <- parameters_medl
par_ml <- c(alg_param,val.listl)
out_medl <- ode(y = state, times = seq(0,42,0.1), func = full_ODE, parms = par_ml)

daph_uprl <- daph %>% filter(method == "lit_only") %>% filter(quant == "upr") %>% dplyr::select(param, value)
val.list1l <- daph_uprl$value
parameters_uprl <- c(as.character(daph_uprl$param))
names(val.list1l) <- parameters_uprl
par_ul <- c(alg_param,val.list1l)
out_uprl <- ode(y = state, times = seq(0,42,0.1), func = full_ODE, parms = par_ul)


daph_lwrl <- daph %>% filter(method == "lit_only") %>% filter(quant == "lwr") %>% dplyr::select(param, value)
val.list2l <- daph_lwrl$value
parameters_lwrl <- c(as.character(daph_lwrl$param))
names(val.list2l) <- parameters_lwrl
par_ll <- c(alg_param,val.list2l)
out_lwrl <- ode(y = state, times = seq(0,42,0.1), func = full_ODE, parms = par_ll)

## run with mixed models (where exists)
daph_medm <- daph %>% filter(method == "mixed") %>% filter(quant == "median") %>% dplyr::select(param, value)
val.listm <- daph_medm$value
parameters_medm <- c(as.character(daph_medm$param))
names(val.listm) <- parameters_medm
par_mm <- c(alg_param,val.listm)
out_medm <- ode(y = state, times = seq(0,42,0.1), func = full_ODE, parms = par_mm)

daph_uprm <- daph %>% filter(method == "mixed") %>% filter(quant == "upr") %>% dplyr::select(param, value)
val.list1m <- daph_uprm$value
parameters_uprm <- c(as.character(daph_uprm$param))
names(val.list1m) <- parameters_uprm
par_um <- c(alg_param,val.list1m)
out_uprm <- ode(y = state, times = seq(0,42,0.1), func = full_ODE, parms = par_um)


daph_lwrm <- daph %>% filter(method == "mixed") %>% filter(quant == "lwr") %>% dplyr::select(param, value)
val.list2m <- daph_lwrm$value
parameters_lwrm <- c(as.character(daph_lwrm$param))
names(val.list2m) <- parameters_lwrm
par_lm <- c(alg_param,val.list2m)
out_lwrm <- ode(y = state, times = seq(0,42,0.1), func = full_ODE, parms = par_lm)


## graph
### with lab data
source()

## with lit data
source()


## mixed
source()



