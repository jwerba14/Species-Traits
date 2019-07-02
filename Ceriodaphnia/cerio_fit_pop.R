## transform cerio data to estimate difference per day

cerio <- read.csv("cerio_pop.csv")
cerio$week <- as.numeric(0)
## create column of weeks
for (i in 1:nrow(cerio)) {
 if (cerio$Day[i] >=1 & cerio$Day[i] < 8) {
  cerio$week[i] <- 1 
} else if ( cerio$Day[i] >=8 & cerio$Day[i] < 15) {
  cerio$week[i] <- 2 
} else if ( cerio$Day[i] >=15 & cerio$Day[i] < 22) {
  cerio$week[i] <- 3 
} else if ( cerio$Day[i] >=22 & cerio$Day[i] < 29) {
  cerio$week[i] <- 4 
} else if ( cerio$Day[i] >=29 & cerio$Day[i] < 36) {
  cerio$week[i] <- 5 
} else if ( cerio$Day[i] >=36 & cerio$Day[i] < 43) {
  cerio$week[i] <- 6    
} else if ( cerio$Day[i] >=43 & cerio$Day[i] < 51) {
  cerio$week[i] <- 7    
}
}

cerio$percapita <- cerio$Avg_Chl/ cerio$Population

## need dataframe with treatment, change in pop, per capita chl
new <- cerio %>% group_by(Treatment, Rep) %>%
  arrange(Day) %>%
  mutate(diff = Population - lag(Population, default = first(Population)))

new <- new %>% select(c(Rep, Date, Treatment, percapita, diff, Population, Avg_Chl))
