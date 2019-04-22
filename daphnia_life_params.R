## fit daphnia lifetime fecundity (?) to food 
## fit daphnia time to death to food
## fit daphnia time to adult size to food

library(tidyverse)
library(ggplot2)

daph <- read.csv("daphnia_lifetime.csv")
# filter out individuals that were NOT born in the conditions (original daphnia)
daph <- daph %>%
  filter(adult_only=="N")

##lifetime fecundity
daph_fec <- daph %>% 
  group_by(rep,treatment) %>%
  summarise(life_fec = sum(clutch_size, na.rm = TRUE))

daph_fec_sum <- daph_fec %>%
  group_by(treatment) %>%
  summarise(life_fec_avg = mean(life_fec), life_fec_sd = sd(life_fec))

#plots standard ERROR (ish didn't go back and check how many actual reps)
ggplot(data = daph_fec_sum, aes(treatment,life_fec_avg))+ geom_point()+ 
  geom_errorbar(aes(ymin = life_fec_avg-life_fec_sd/sqrt(10), ymax = life_fec_avg+life_fec_sd/sqrt(10) ))

##time until death
daph_death <- daph %>%
  group_by(rep,treatment) %>%
  count()

daph_death_sum <- daph_death %>%
  group_by(treatment) %>%
  summarise(time_to_death_avg = mean(n), time_to_death_sd = sd(n))


ggplot(data = daph_death_sum, aes(treatment,time_to_death_avg))+ geom_point()+ 
  geom_errorbar(aes(ymin = time_to_death_avg-time_to_death_sd,
                    ymax = time_to_death_avg+time_to_death_sd))


###growth to adult
daph_growth <- daph %>%
  group_by(rep,treatment) %>%
  count(size_class)

daph_growth_j <- daph_growth %>%
  filter(size_class=="J")

daph_growth_sum <- daph_growth_j %>%
  group_by(treatment) %>%
  summarise(growth_avg = mean(n), growth_sd = sd(n))

ggplot(data = daph_growth_sum, aes(treatment,growth_avg))+ geom_point()+ 
  geom_errorbar(aes(ymin = growth_avg-growth_sd,
                    ymax = growth_avg+growth_sd))

