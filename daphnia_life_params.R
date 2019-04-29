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
  summarise(life_fec = sum(clutch_size, na.rm = TRUE), chl = mean(avg_chl), chl_sd_rep = sd(avg_chl))


daph_fec_sum <- daph_fec %>%
  group_by(treatment) %>%
  summarise(life_fec_avg = mean(life_fec), life_fec_sd = sd(life_fec), chl_avg = mean(chl), chl_sd = sd(chl))

#plots standard ERROR (ish didn't go back and check how many actual reps)
ggplot(data = daph_fec_sum, aes(chl_avg,life_fec_avg))+ geom_point()+ 
  geom_errorbar(aes(ymin = life_fec_avg-life_fec_sd/sqrt(10), ymax = life_fec_avg+life_fec_sd/sqrt(10) ))

## to get fecundity parameter fit saturating curve (params z and w in full ode)
#sat_fun(z,w,algae)

## hmm not really what we want probably shouldn't fit to means -- 
## also this is for lifetime so should params be divided by days in adult phase?
fit1 <- nls(life_fec_avg ~ sat_fun(a=a, b=b,chl_avg), data=daph_fec_sum,start = list(a=0.5,b=1))

plot(predict(fit1), daph_fec_sum$life_fec_avg)


ndat <- data.frame(
  chl_avg = daph_fec_sum$chl_avg,
  life_fec_avg = predict(fit1)
  
)

ggplot(data = daph_fec_sum, aes(chl_avg, life_fec_avg))+geom_point()+
  geom_line(data=ndat, aes(chl_avg,life_fec_avg))

### try with fit to all data DEFINITELY WRONG
fit1 <- nls(life_fec ~ sat_fun(a=a, b=b,chl), data=daph_fec,start = list(a=0.5,b=3))

ndat <- data.frame(
  chl = daph_fec$chl,
  life_fec = predict(fit1)
  
)

ggplot(data = daph_fec, aes(chl, life_fec))+geom_point()+geom_line(data=ndat, aes(chl,life_fec))


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

