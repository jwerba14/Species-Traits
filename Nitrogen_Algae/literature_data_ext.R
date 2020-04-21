## get literature values for algae parameters

library(tidyverse)

litd <- read.csv("~/GitHub/Species-Traits/Master_Data/literature_extraction.csv", header=TRUE)




litalg <- litd %>% dplyr::select(
 Authors, Title,ammoium.conc, units.1, nitrogen.removal, units.2, max_ammonium_uptake,
 max_uptake_mg_day,max_uptake_units, max_uptake_sd, half_sat_amm,units.3, half_sat_mg_N_L,
 sd_half_sat, range_half_sat, nitrogen.removal.1, units.4, sd, death_rate,units_death, algal_death_per_day, 
 X95_conf_death, sd_death, range_death, growth_rate, units.5, sd.1, range.1, time_period.days.,units.6,
 convert_mg_N_day, sd.2, N_conversion.efficiency  )                     


## maximum uptake (a)

max_lit <- litalg %>% dplyr::select(Authors, Title,max_ammonium_uptake,
                                    max_uptake_mg_day,max_uptake_units, max_uptake_sd )

max_lit <- max_lit %>% filter(max_ammonium_uptake != "NA")

##number of studies
length(unique(max_lit$Title)) ##5

## number of data points
nrow(max_lit) ## 5

## but only 4 can be made into correct units-- and one of those is >10x larger than the others?? 

## half saturation (k)
half_lit <- litalg %>% dplyr::select(Authors, Title,half_sat_amm,units.3, half_sat_mg_N_L,
                                     sd_half_sat, range_half_sat)

half_lit <- half_lit%>% filter(half_sat_amm != "NA")

##number of studies
length(unique(half_lit$Title)) ##9

## number of data points
nrow(half_lit) ## 10

## all 10 can be put into correct units but range is huge (0.002-31.5)


## N converstion efficiency (f)

N_con_lit <- litalg %>% dplyr::select(Authors, Title,N_conversion.efficiency)

N_con_lit <- N_con_lit %>% filter(N_conversion.efficiency != "NA")

##number of studies
length(unique(N_con_lit$Title)) ##1

## number of data points
nrow(N_con_lit) ## 1


## death (death1)
a_death_lit <- litalg %>% dplyr::select(Authors, Title, death_rate,units_death, algal_death_per_day, 
                                       X95_conf_death, sd_death, range_death)
a_death_lit <- a_death_lit %>% filter(death_rate != "NA")

##number of studies
length(unique(a_death_lit$Title)) ##7

## number of data points
nrow(a_death_lit) ## 29


## growth rate (whole of mm equation)
growth_lit <- litalg %>% dplyr::select(Authors, Title,growth_rate, units.5, sd.1, range.1, time_period.days.,units.6,
                                       convert_mg_N_day)
growth_lit <- growth_lit %>% filter(growth_rate != "NA")

##number of studies
length(unique(growth_lit$Title)) ##37

## number of data points
nrow(growth_lit) ## 118


