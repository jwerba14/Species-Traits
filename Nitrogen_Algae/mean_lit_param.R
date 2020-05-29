library(fitdistrplus)
litdat <- read.csv("Data/literature_extraction.csv")

## growth rate
growth_rate <- litdat %>% filter(growth_day !="NA") %>% 
  dplyr::select(growth_rate, units.6, ammoium.conc, units) %>%
  filter(ammoium.conc != "NA")  #g

growth_rate$amm_micro <- growth_rate$ammoium.conc*1000 
growth_rate$gr_day <- as.numeric(0)

for(i in 1:nrow(growth_rate)){
  if(growth_rate$units.6[i] == "per day") {
    growth_rate$gr_day[i] <- growth_rate$growth_rate[i]
  } else if (growth_rate$units.6[i] == "per hour"){
    growth_rate$gr_day[i] <- growth_rate$growth_rate[i] * 24
  }
}


growth_rate <- growth_rate %>% filter(gr_day > 0) %>% filter(ammoium.conc != 0)

## g param is chl/amm so...
g <- growth_rate$gr_day/growth_rate$amm_micro 
mean(g) ##  0.0001181192 ## definitely not confident about this method

fitdist(g, "lnorm") ## -12, 2.2

## death rate
hist(litdat$death_rate_day)
death_rate <- litdat %>% dplyr::select(death_rate_day) %>% filter(death_rate_day != "NA") ##death1
mean(death_rate$death_rate_day) ## 0.08105952 

fitdist(death_rate$death_rate_day, "lnorm")  ## -3.3, 1.6


## nitrogen removal rate

nit_rem <- litdat %>% dplyr::select(nit_rem_mg_day) %>% filter(nit_rem_mg_day != "NA")  ##k??
mean(nit_rem$nit_rem_mg_day) ##  5.528499 in mg in micrograms = 5528

nit_rem$nr1 <- nit_rem$nit_rem_mg_day *1000
fitdist(nit_rem$nr1, "lnorm")  ## 7.3, 1.7

## max NH4 uptake (not much data)
hist(litdat$max_uptake_day)
max_up <- litdat %>% dplyr::select(max_uptake_day, units.2) %>% filter(max_uptake_day !="NA") #a
mean(max_up$max_uptake_day) ## 9.10701
max_up$amm1 <- max_up$max_uptake_day * 1000

fitdist(max_up$amm1, "lnorm") ## 8.6, 1.2
