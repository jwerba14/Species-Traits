library(tidyverse)
library(fitode)
source("transfer_functions.R")
dat <- read.csv("Algae_Nutrient.csv") 

## look at a single treatment for Nh4 ## patterns are more obvious when just looking at single treatment 
##but doesn't seem to help with fit

dat_nit_27 <- dat %>%
	filter(treat == 27)

dat_nit_9 <- dat %>%
	filter(treat == 9)

dat_nit_3 <- dat %>%
	filter(treat == 3)

dat_nit_108 <- dat %>%
	filter(treat == 108)

dat_nit_54 <- dat %>%
	filter(treat == 54)

dat_nit_0.5 <- dat %>%
	filter(treat == 0.5)


### correct Nh4 for pH based on communication with YSI

# first need temp in kelvin
tempK <- dat$temp + 273.15 
nh3 <- dat$nh4 * (10^(dat$ph-((2726.3/tempK)+0.0963)))

cnitrate = .000001 # nitrate lost to env-- calc in nutrient_air.R
cammonium = .0001 # ammonium lost to env-- calc in nutrient_air.R

chl_nh4_mod <- new("model.ode",
	name = "algal_nit",
	model = list(
		pred_nh4 ~ -pred_chl * alpha * pred_nh4/(K + pred_nh4) + d * pred_chl * gamma * exp(pred_chl/pred_chl_d),
		pred_chl ~ beta * pred_chl * alpha * pred_nh4/(K + pred_nh4) - d * pred_chl * exp(pred_chl/pred_chl_d)
	),
	## consider using bbmle::dnorm_n ?
	observation = list(
		nh4 ~ dnorm(mean = pred_nh4, sd=sd1),
		chl ~ dnorm(mean = pred_chl, sd=sd2)
	),
	initial = list(pred_nh4 ~ pred_nh40 , pred_chl ~ pred_chl0),
	par=c("alpha", "beta", "K", "d", "pred_nh40", "pred_chl0", "gamma", "sd1", "sd2", "pred_chl_d")
)

## maybe figure out initial values
start <- c(alpha = 0.08, 
		beta = 15,
		gamma=0.02,
		K=10,
		d=1/7,
		pred_chl_d=400,
		pred_nh40 = 15 ,
		pred_chl0 = 40, 
		sd1 = 10,
		sd2 = 100)

ss <- ode.solve(chl_nh4_mod, 1:11, start,
				solver.opts=list(method="rk4", hini=0.1))

plot(dat_nit_27$date1, dat_nit_27$nh4, ylim=c(0, 30))
lines(ss@solution$pred_nh4)

plot(dat_nit_27$date1, dat_nit_27$chl)
lines(ss@solution$pred_chl)

sum(dnorm(dat_nit_27$nh4, ss@solution$pred_nh4[match(dat_nit_27$date1, 1:11)], start[["sd1"]], log=TRUE)) +
	sum(dnorm(dat_nit_27$chl, ss@solution$pred_chl[match(dat_nit_27$date1, 1:11)], start[["sd2"]], log=TRUE))

# options(error=recover)  ## stop/browse when error occurs
chl_fit_27_dd <- fitode(
	chl_nh4_mod,
	data = dat_nit_27, 
	start=start,
	tcol = "date1",
	solver.opts=list(method="rk4", hini=0.1)
)

plot(chl_fit_27_dd, level=0.95)
coef(chl_fit_27_dd)

## alpha - NH4 consumption per algae per time: 0.05
## beta - algae per NH4: 13
## K - half max: 6.5
## d - death rate per time: 0.03
## gamma - nh4 release per chl: 0.1
## pred_chl_d - scale for density dependent death: 162

start2 <- coef(chl_fit_27_dd)
start2[["pred_nh40"]] <- 6
start2[["pred_chl0"]] <- 50

## close but not great
chl_fit_9_dd <- fitode(
	chl_nh4_mod,
	data = dat_nit_9,
	start=start2,
	tcol = "date1" #,
	#method="Nelder-Mead"
)
plot(chl_fit_9_dd, level=0.95)

coef(chl_fit_27_dd)
coef(chl_fit_9_dd)

start3 <- coef(chl_fit_9_dd)
start3[["pred_nh40"]] <- 3
start3[["pred_chl0"]] <- 40

##not bad
chl_fit_3_dd <- fitode(
	chl_nh4_mod,
	data = dat_nit_3,
	start=start3,
	tcol = "date1" #,
	#method="Nelder-Mead"
)
plot(chl_fit_3_dd, level=0.95)

coef(chl_fit_9_dd)
coef(chl_fit_3_dd)

start4 <- coef(chl_fit_27_dd)
start4[["pred_nh40"]] <- 22
start4[["pred_chl0"]] <- 40

chl_fit_54 <- fitode(
	chl_nh4_mod,
	data = dat_nit_54,
	start=start4,
	tcol = "date1" #,
	#method="Nelder-Mead"
)
plot(chl_fit_54, level=0.95)

coef(chl_fit_27_dd)
coef(chl_fit_54)

start5 <- coef(chl_fit_54)
start5[["pred_nh40"]] <- 60
start5[["pred_chl0"]] <- 40

chl_fit_108 <- fitode(
	chl_nh4_mod,
	data = dat_nit_108,
	start=start5,
	tcol = "date1" #,
	#method="Nelder-Mead"
)
plot(chl_fit_108)

start6 <- coef(chl_fit_54)
start6[["pred_nh40"]] <- 2.5
start6[["pred_chl0"]] <- 40

##
chl_fit_0.5 <- fitode(
	chl_nh4_mod,
	data = dat_nit_0.5,
	start=start6,
	tcol = "date1" #,
	#method="Nelder-Mead"
)
plot(chl_fit_0.5)

## make a dataframe of all parameters

treat0.5 <- data.frame(confint(chl_fit_0.5))
treat3 <- data.frame(confint(chl_fit_3_dd))
treat9 <- data.frame(confint(chl_fit_9_dd))
treat27 <- data.frame(confint(chl_fit_27_dd))
treat54 <- data.frame(confint(chl_fit_54))
treat108 <- data.frame(confint(chl_fit_108))

all_param <- data.frame(
	model =  rep(c("chl_fit_0.5", "chl_fit_3","chl_fit_9","chl_fit_27","chl_fit_54","chl_fit_108"), each=length(start)),
	parameter = rep(names(start), 6),
	estimate = c(treat0.5$estimate,treat3$estimate,treat9$estimate,treat27$estimate,treat54$estimate,treat108$estimate),
	lowcon = c(treat0.5$X2.5..,treat3$X2.5..,treat9$X2.5..,treat27$X2.5..,treat54$X2.5..,treat108$X2.5..),
	uppcon =  c(treat0.5$X97.5..,treat3$X97.5..,treat9$X97.5..,treat27$X97.5..,treat54$X97.5..,treat108$X97.5..)
	
)

filter_param <- all_param %>%
	filter(!(parameter %in% c("pred_nh40", "pred_chl0", "sd1", "sd2"))) %>%
	filter(model != "chl_fit_108")

ggplot(filter_param, aes(model,estimate)) +
	geom_point() + 
	# geom_errorbar(aes(model, ymin=lowcon, ymax=uppcon)) +
	scale_y_log10() +
	facet_wrap(~parameter, scale="free_y")
