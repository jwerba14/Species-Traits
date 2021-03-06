## construct prior distributions for parameters from literature values
library(tidyverse)
library(fitdistrplus)
lit <- read.csv("literature_extraction.csv")

parameters <- c(
  alpha = 0.3,    #mg N/(chl*day)
  kappa = 0.3,    #mg N
  e =  1,       #chl/mgN
  death1 = 0.001,   # 1/day
  fa = 0.3,       # chl/(daphniaA*day)
  fj = 0.3,       # chl/(daphniaj*day)
  rho = 1,      # mg N/ chl
  camm = .0000001,     # 1/day
  xa = 0.01,       # mg N/ (daphniaA*day) 
  xj= 0.001,        # mg N/ (daphniaJ*day) 
  g1 =0.3,        # daphniaD/(daphniaJ*day)
  g2=0.3,         # chl
  death2 = 0.001,   # 1/day
  b1 = 0.3,       # daphniaJ/(daphniaD*day)
  b2 = 0.3,        # chl 
  death3 = 0.01   # 1/day
)


## fecundity (b1 and b2 -- units-- baby/(indiv*day))

not_all_na <- function(x) !all(is.na(x))
lit_clean <- (lit
    %>% drop_na(daphnia_reproduction)   ## drop rows with no response
    %>% select_if(not_all_na)
)
#names(lit) -- all columns
#setdiff(names(lit),names(lit_clean))
#all(sapply(lit_clean, not_all_na))

##  -- all values *excluded* from lit_clean
fec_lit <- lit %>% dplyr::select(c("daphnia_reproduction", "sd_repro", "units_reproduction", "Replicates")) %>%
  filter(daphnia_reproduction != "NA")

q <- fitdist(fec_lit$daphnia_reproduction, "lnorm")
y <- seq(0,10, length = 100)
c <- dlnorm(y, meanlog = q$estimate[1], sdlog = q$estimate[2])

df<- data.frame(
  y = y,
  daphnia_reproduction = c
)

p <- ggplot(fec_lit) +
  geom_histogram(aes(x = daphnia_reproduction, y = ..density..),
                 binwidth = .5, fill = "grey", color = "black")+
    scale_x_continuous(limits=c(0,10))


## compute the function on the fly
dlfun <- function(x) {
    with(as.list(q$estimate), dlnorm(x, meanlog, sdlog))
}
p + stat_function(fun=dlfun, color="red")
##p + geom_line(data = df, aes(y = daphnia_reproduction, x = y), color = "red")

## plot(daphnia_reproduction~y,data=df)

### daphnia survival I have days until death if assume exponential than 1/days survival
surv <- lit %>% dplyr::select("Replicates","daphnia_survival","range",
                              "sd_survival","X95CI", "units_survival") %>% drop_na(daphnia_survival)

surv$ci <- c(NA,16.75,NA,10.9,13.5,3,8.3,11.7,8.8)
surv$sd <- (surv$ci*2*sqrt(as.numeric(as.character(surv$Replicates))))/3.92
surv$sd[1] <- 0.26
surv <- surv %>% select(Replicates, daphnia_survival, sd)
##write.csv(surv, file = "survival_literature.csv" )

hist(surv$daphnia_survival)
## subtract average number of days to large size (based on my experiment- because I need this to be a prior for survival after growth to adult)
surv$survival <- surv$daphnia_survival- 4.8
d<-fitdistr(surv$survival, "normal")
hist(surv$survival)
p <- ggplot(surv) +
  geom_histogram(aes(x = survival, y = ..density..),
                 binwidth = 1, fill = "grey", color = "black") 


## compute the function on the fly
dlfun <- function(x) {
  with(as.list(d$estimate), dnorm(x, mean, sd))
}
p + stat_function(fun=dlfun, color="red")


## daphnia growth have days until 1st clutch, which is what i can get most info on but could get growth curves which would match my data better....

## kappa -- half saturation uptake algae-- units mg N
## algal uptake
#uptake <- lit %>% dplyr::select("Replicates","ammoium.conc","nitrogen.removal.1", "Authors") %>% filter(nitrogen.removal.1 != "NA") #%>%
  #filter(ammoium.conc != "NA") %>% 

with(uptake, plot(ammoium.conc, nitrogen.removal.1))

kappa <- lit %>% dplyr::select(c("Replicates","half_sat_mg_N_L","sd_half_sat","range_half_sat")) %>% filter(half_sat_mg_N_L != "NA")

d <- fitdistr(kappa$half_sat_mg_N_L, "lognormal")

hist(kappa$half_sat_mg_N_L)
p <- ggplot(kappa) +
  geom_histogram(aes(x = half_sat_mg_N_L, y = ..density..),
                 binwidth = 1, fill = "grey", color = "black") 

dlfun <- function(x) {
  with(as.list(d$estimate), dlnorm(x, meanlog, sdlog))
}
p + stat_function(fun=dlfun, color="red")


## alpha - max uptake algae of N -- units mgN/(chl*day) 

alpha <- lit %>% dplyr::select("Replicates","max_ammonium_uptake","max_uptake_units","max_uptake_sd")%>% 
                filter(max_ammonium_uptake != "NA")

## e (efficiency of n conversion by algae) -- need data-- have from one paper set of conversion rates vs mg N

## rho (efficiency of death conversion of algae to nh4) -- need data

## death1 -- algae death 1/day

death1 <- lit %>% dplyr::select("Replicates","Genus","species","algal_death_per_day") %>% filter(algal_death_per_day != "NA")
hist(death1$algal_death_per_day)
dd <- fitdistr(death1$algal_death_per_day, "lognormal")
p <- ggplot(death1) +
  geom_histogram(aes(x = algal_death_per_day, y = ..density..),
                 binwidth = 1, fill = "grey", color = "black") 

dlfun <- function(x) {
  with(as.list(dd$estimate), dlnorm(x, meanlog, sdlog))
}
p + stat_function(fun=dlfun, color="red")



## daphnia adult excretion 
x <- lit %>% dplyr::select("Replicates", "Excretion.rate", "sd_error_excretion", "units_excretion", "algal_conc") %>%
  filter(Excretion.rate != "NA") 

write.csv(x, file = "excretion_literature.csv")
## daphnia juvenile excretion

## daphnia adult uptake algae
f <- lit %>% dplyr::select("Replicates","Max_filtering","sd_filtering","CI_filtering_upper","CI_filtering_low","units",
                           "daphnia_size") %>% filter(Max_filtering != "NA") %>%
  filter(units != "millions cells per individual per hour") %>% 
  filter(units != "millions chlorella per indiv per hour")

hist(f$Max_filtering)

## daphnia juvenile uptake algae


## nitrification
n <- lit %>%dplyr::select("Replicates","nitrification","units.6", "convert_mg_N_day", "sd.2","range.2","X75.quartile") %>%
  filter(nitrification != "NA")%>% filter(units.6 != "g NH4-N g per VSS per day") %>% 
  drop_na(convert_mg_N_day) %>% filter(units.6 != "mg N per g per dry weight day")
## VSS is volatile suspended solid...
## would like to select columns that aren't all NAs to make sure i don't miss any important information in my select....
n1 <- lit %>% drop_na(nitrification)%>% select_if(not_all_na)   ## drop rows with no response

d <- fitdist(n$convert_mg_N_day, "lnorm")

p <- ggplot(n) +
  geom_histogram(aes(x = convert_mg_N_day, y = ..density..),
                 binwidth = .2, fill = "grey", color = "black") 


## compute the function on the fly
dlfun <- function(x) {
  with(as.list(d$estimate), dlnorm(x, meanlog, sdlog))
}
p + stat_function(fun=dlfun, color="red")

