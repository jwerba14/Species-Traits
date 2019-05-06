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





## filter for J that die as J
juv_death <- daph %>% group_by(rep,treatment) %>% filter(sum(size_class == "A") == 0)
# check
daph %>% filter(rep == "49D", treatment == 3)


##hmm looks like i only had 3 not make it to adult... so thats a problem
juv_death <- juv_death %>% 
  group_by(rep,treatment) %>%
  summarise(days_to_death = n(), chl = mean(chl_avg), chl_sd = sd(chl_avg))


###growth to adult
daph_growth <- daph %>%
  group_by(rep,treatment) %>%
  filter(sum(size_class == "A") != 0)

daph_growth_j <- daph_growth %>%
  filter(size_class=="J") %>%
  group_by(treatment, rep) %>%
  summarize(days_to_adult = n(), chl = mean(chl_avg), chl_sd = sd(chl_avg) )

## looks pretty constant
ggplot(data = daph_growth_j, aes(chl, days_to_adult))+ geom_point(aes(color= as.factor(treatment)))

##but needs to go through origin -because at 0 chl will not grow
grow <- nls(days_to_adult ~ sat_fun(a=a,b=b, k= chl), start= list(a=1, b=1), data = daph_growth_j)

newdat <- data.frame(
  chl = seq(0, max(daph_growth_j$chl), length = 200),
  days_to_adult = predict(grow, newdata = data.frame(chl = seq(0, max(daph_fec$chl), length = 200)))
)

ggplot(data = daph_growth_j, aes(chl, 1/days_to_adult)) + geom_point(aes(color = as.factor(treatment)))+
  geom_line(data = newdat, aes(chl, 1/days_to_adult)) 

