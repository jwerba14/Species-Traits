## build priors
library(tidyverse)
library(fitdistrplus)

litdat <- read.csv("literature_extraction.csv")


## half saturation constant
hist(litdat$half_sat_mg_N_L)

half_sat <- litdat %>% dplyr::select(half_sat_mg_N_L) %>% filter(half_sat_mg_N_L != "NA")

half_sat <- as.vector(half_sat$half_sat_mg_N_L)

ff1 <- fitdistr(half_sat, "lognormal")  ##uses maximum likelihood-- exp(0.12)  # lognormal 0.2664,2.9

hist(half_sat,breaks = 10)
denscomp(ff1)
xfit <- seq(min(half_sat), max(half_sat), length = 40)
yfit <- dlnorm(xfit,meanlog = .2665, sdlog = 2.9)
lines(xfit, yfit)


## growth rate
hist(litdat$growth_day)
growth_rate <- litdat %>% dplyr::select(growth_day) %>% filter(growth_day !="NA")
growth_rate <- as.vector(growth_rate$growth_day)
ff2 <- fitdist(growth_rate, "gamma")  ## gamma(0.968, 1.15)
denscomp(ff2)
?dgamma
hist(dgamma(growth_rate, shape = 1, rate = 9))



## death rate
hist(litdat$death_rate_day)
death_rate <- litdat %>% dplyr::select(death_rate_day) %>% filter(death_rate_day != "NA")
death_rate <- as.vector(death_rate$death_rate_day)
ff3 <- fitdist(death_rate, "gamma")  ## gamma(.79,9.77)
denscomp(ff3)
hist(death_rate)
yfit <- dgamma(death_rate, rate = 9, shape = .79)
lines(density(death_rate))


## nitrogen removal rate
hist(litdat$nit_rem_mg_day, breaks = 15)
nit_rem <- litdat %>% dplyr::select(nit_rem_mg_day) %>% filter(nit_rem_mg_day != "NA")
nit_rem <- as.vector(nit_rem$nit_rem_mg_day)
ff4<- fitdist(nit_rem, "exp")  # exp(0.18)
denscomp(ff4)


## max NH4 uptake (not much data)
hist(litdat$max_uptake_day)
max_up <- litdat %>% dplyr::select(max_uptake_day) %>% filter(max_uptake_day !="NA")
max_up <- as.vector(max_up$max_uptake_day)
ff5 <- fitdist(max_up, "gamma") #gamma(1.13,.12)
denscomp(ff5)
