## fit daphnia lifetime fecundity (?) to food 
## fit daphnia time to death to food
## fit daphnia time to adult size to food

library(tidyverse)


daph <- read.csv("daphnia_lifetime.csv")
# filter out individuals that were NOT born in the conditions (original daphnia)
daph <- daph %>%
  filter(adult_only=="N")

## to get fecundity parameter fit saturating curve (params z and w in full ode)
## need to make per day so need to divide total fecundity by # of days that individual was an adult
daph_fec <- daph %>% group_by(rep, treatment) %>%
  filter(size_class == "A") %>%
  summarize(
    time_adult = n()
  , life_fec   = sum(clutch_size, na.rm = TRUE)
  , chl        = mean(chl_avg)
  , chl_sd_rep = sd(chl_avg)) %>%
  mutate(daily_fec = life_fec / time_adult)

## should i remove individuals that died in 1-2 days of being an adult since that will pull fec down 
## and it will be captured in death term

fec_param <- nls(daily_fec ~ sat_fun(z,w,chl), data = daph_fec[daph_fec$daily_fec > 0.1,],
                 start = list(z=1,w=1))

fec_param1 <- nls(daily_fec ~ sig_fun(z,w,t,chl), data = daph_fec[daph_fec$daily_fec > 0.1,],
                 start = list(z=1,w=1,t=2))
## z = 9.828, w = 20.512
# if want to filter out daphnia who had no/almost no babies [daph_fec$daily_fec > 0.1,]  

newdat <- data.frame(
  chl = rep(seq(0, max(daph_fec$chl), length = 200), 2),
  daily_fec = c(predict(fec_param, newdata = data.frame(chl = seq(0, max(daph_fec$chl), length = 200)))
                , predict(fec_param1, newdata = data.frame(chl = seq(0, max(daph_fec$chl), length = 200)))),
  model = rep(c("sat", "sig"), each = 200)
)

ggplot(data = daph_fec, aes(chl, daily_fec)) + geom_point()+
  geom_line(data = newdat, aes(chl, daily_fec, colour = model))  


## look at some patterns in fecundity
ggplot(daph, aes(chl_avg, clutch_size)) + geom_point(aes(colour = as.factor(treatment)))

ggplot(daph_fec, aes(chl, daily_fec)) + geom_point()

ggplot(daph_fec[daph_fec$time_adult > 3, ], aes(time_adult, daily_fec)) +
  geom_point(aes(colour = as.factor(treatment)), lwd = 4) + facet_wrap(~treatment)

ggplot(daph_fec[daph_fec$time_adult > 3, ], aes(time_adult, life_fec)) +
  geom_point(aes(colour = as.factor(treatment)), lwd = 4) + facet_wrap(~treatment)


daph_fec_sum <- daph_fec %>%
  group_by(treatment) %>%
  summarise(life_fec_avg = mean(life_fec), life_fec_sd = sd(life_fec), chl_avg = mean(chl), chl_sd = sd(chl))


#########

##time until death
daph_adult_death <- daph %>%
  filter(size_class == "A")%>%
  group_by(rep,treatment) %>%
  summarize(days_adult = n(),
            chl = mean(chl_avg),
            chl_sd_rep = sd(chl_avg)) 


adult_death <- nls(days_adult ~ sat_fun(a,b,k=chl), data = daph_adult_death[daph_adult_death$days_adult > 1, ],
                   start = list(a=0.1,b=0.1))


#
newdat <- data.frame(
#  chl = daph_adult_death[daph_adult_death$days_adult > 1, ]$chl,
  chl = seq(0, max(daph_fec$chl), length = 200),
  days_adult = predict(adult_death, newdata = data.frame(chl = seq(0, max(daph_fec$chl), length = 200)))
)

ggplot(data = daph_adult_death, aes(chl, 1/days_adult)) + geom_point()+
  geom_line(data = newdat, aes(chl, 1/days_adult))  
# need to plot 1/days_adult because that is rate 
   #and can see asymptote quicly at 0 chl as expected



#  geom_line(data = data.frame(
#    chl = newdat$chl,
#    days_adult = sat_fun(a = 22.42, b = 0.036, k = newdat$chl)))

## visualize adult death

ggplot(data = daph_adult_death, aes(chl, days_adult))+ geom_point(aes(color = as.factor(treatment))) 

daph_death_sum <- daph_death %>%
  group_by(treatment) %>%
  summarise(time_to_death_avg = mean(n), time_to_death_sd = sd(n))


ggplot(data = daph_death_sum, aes(treatment,time_to_death_avg))+ geom_point()+ 
  geom_errorbar(aes(ymin = time_to_death_avg-time_to_death_sd,
                    ymax = time_to_death_avg+time_to_death_sd))



## filter for J that die as J
daph %>% group_by(rep,treatment) %>% filter(sum(size_class == "A") == 0)
# check
daph %>% filter(rep == "49D", treatment == 3)

## count time in J
## summmarize(days_juv = n())

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

