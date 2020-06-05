## read in Daphnia parameters
## this script needs to be broken up
library(gridExtra)
library(deSolve)
library(tidyverse)
source("../Graphing_Set_Up.R")

daph <- read.csv("../Daphnia/daphnia_params.csv")
alg_param <- read.csv("../Nitrogen_Algae/algal_parameters.csv")
alg_param$method <- "dat_only"
alg_param$quant <- "median"

state = c(
  ammonium = 10,
  daph_j = 0,
  daph_a = 20,
  algae  = 20
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
death3 <- list(death3 = 0.02)
pp <- c(pp, death3)


tt <- ode(
  func=full_ODE,
  y=state,   
  times=seq(1,40),
  parms=pp
) 
 tt1 <- as.data.frame(tt)
 tt1$index <- i
  if (i == 1) {
  tt.g <- tt1 
} else {
  tt.g <- rbind(tt.g,tt1)
}
 
}

tt.g1 <- tt.g %>% filter(time == 40)  ## 138 unique combinations run the whole way

tt.f <- tt.g %>% filter(index %in% tt.g1$index)

ll <- data.frame(ll = 0,
                 index = 0,
                 rep = 0)

## find loglik for each fit for the combinations that worked
for(j in 1:length(unique(dd$Rep))){
  dd1 <- dd %>% filter(Rep == j)
  pop_fin2 <- pop_fin1 %>% filter(TankNum == j)
for (i in 1:length(unique(tt.f$index))) {
  tt.f1 <- tt.g %>% filter(index == i)
  ll$ll[i,] <- loglik2(tt.f1)
  ll$index[i,] <- i
  ll$rep[i,] <- j
}
}







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



