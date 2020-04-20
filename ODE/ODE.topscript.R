## read in Daphnia parameters
## this script needs to be broken up
library(gridExtra)
library(deSolve)
source("../Graphing_Set_Up.R")
daph <- read.csv("../Daphnia/daphnia_params.csv")

## graph param estimates comparing 3 methods


## read in algal/nit parameters... clean version doesn't exisit yet
## these are still running so for now use old fits without error 

alg_param <- read.csv("../Nitrogen_Algae/algal_parameters.csv")

## full ode
source("full_ode.R")

state <- c(
  ammonium = 10,
  daph_j = 0,
  daph_a = 20,
  algae  = 20
)

daph <- read.csv("../Daphnia/daphnia_params.csv")

## run with params from our lab data only
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



