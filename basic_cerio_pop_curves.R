# read in
dat <- read.csv("cerio_pop.csv")
library(minpack.lm)
trial <- nlsLM(Population ~ Population * r*(1-1/(K*Population/Avg_Chl)), start = list(r=.1, K= 75), 
             data = dat)

newpred <- predict()

                                    
#this works even though it makes no sense Population ~ Population * (r - r/K * (Population/Avg_Chl)) *Population


m <- nls(feeding.rate.mean~(Chl_conc*h)/(1+Chl_conc*h*r), start = list(h=1,r=1), data = dat )
library(tidyverse)
library(ggplot2)
library(lme4)

mod <- lm(log(pop) ~ week*as.factor(treat), data= week_chl[week_chl$pop!=0,])
mod <- lm((pop) ~ week*as.factor(treat), data= week_chl)

#average chlor by week
# create week column
dat$week <- floor(dat$Day/7) 
#create average chl by week
week_chl <- dat %>%
  group_by(Rep,week) %>%
  summarise(aver_chl = mean(Avg_Chl, na.rm = TRUE), 
            pop = mean(Population, na.rm = TRUE),
            sd_chl = sd(Avg_Chl,na.rm = TRUE),
            sd_pop = sd(Population, na.rm = TRUE),
            treat= mean(Treatment))

(gg2 <- ggplot(aes(week, log(pop)), data = week_chl) +
    geom_point(aes(color= as.factor(treat))) + facet_wrap(~Rep))

(gg2 <- ggplot(aes(week, (pop)), data = week_chl) +
    geom_point(aes(color= as.factor(treat))) + facet_wrap(~Rep))
week_chl$per_capita <- week_chl$aver_chl/week_chl$pop
week_chl2 <- week_chl %>%
  group_by(Rep) %>%
  mutate(delta_pop = (c(pop,0)/c(0, pop))[-length(pop)],
         treat= treat)



week_chl2 <- week_chl2 %>%
  filter(delta_pop > 1 & delta_pop != Inf)
(newplot<- ggplot(aes(per_capita,delta_pop),data = week_chl2) + 
  geom_point(aes(color=as.factor(treat))) + facet_wrap(~as.factor(treat)) )

(pl10 <- ggplot(aes(log(per_capita),delta_pop), 
               data = week_chl2[week_chl2$treat==10,]) +
  geom_point(aes(color=as.factor(week))))

(pl25 <- ggplot(aes(log(per_capita),delta_pop), 
                data = week_chl2[week_chl2$treat==25,]) +
    geom_point(aes(color=as.factor(week)), lwd=4))


(gg2 <- ggplot(aes(Day, log(Population)), data = dat) +
  geom_point(aes(color= as.factor(Treatment))) + facet_wrap(~Rep))


(gg2 <- ggplot(aes(Day, (Population)), data = dat) +
    geom_point(aes(color= as.factor(Treatment))) + facet_wrap(~Rep))

gg3 <- ggplot(aes(Initial.Chl, Population), data = dat) +
  geom_point() 
dat_rep1 <- dat %>%
 filter(Rep == 1) %>%
  group_by(Day)%>%
  summarize(mean_pop = mean(Population,na.rm = TRUE))
dat_rep1 <- data.frame(dat_rep1)


(gg <- ggplot(aes(Day,mean_pop), data = dat_rep1) +geom_point())

##
dat_rep7 <- dat %>%
  filter(Rep == 7) %>%
  group_by(Day)%>%
  summarize(mean_pop = mean(Population,na.rm = TRUE))
dat_rep7 <- data.frame(dat_rep7)


(gg <- ggplot(aes(Day,mean_pop), data = dat_rep7) +geom_point())


##
dat_rep13 <- dat %>%
  filter(Rep ==13) %>%
  group_by(Day)%>%
  summarize(mean_pop = mean(Population,na.rm = TRUE))
dat_rep13 <- data.frame(dat_rep13)


(gg <- ggplot(aes(Day,mean_pop), data = dat_rep13) +geom_point())

##
dat_rep17 <- dat %>%
  filter(Rep == 17) %>%
  group_by(Day)%>%
  summarize(mean_pop = mean(Population,na.rm = TRUE))
dat_rep17 <- data.frame(dat_rep17)


(gg <- ggplot(aes(Day,mean_pop), data = dat_rep17) +geom_point())


##
dat_rep2 <- dat %>%
  filter(Rep == 2) %>%
  group_by(Day)%>%
  summarize(mean_pop = mean(Population,na.rm = TRUE))
dat_rep2<- data.frame(dat_rep2)


(gg <- ggplot(aes(Day,mean_pop), data = dat_rep2) +geom_point())

##
dat_rep8 <- dat %>%
  filter(Rep == 8) %>%
  group_by(Day)%>%
  summarize(mean_pop = mean(Population,na.rm = TRUE))
dat_rep8<- data.frame(dat_rep8)



(gg <- ggplot(aes(Day,mean_pop), data = dat_rep8) +geom_point())


##
dat_rep12 <- dat %>%
  filter(Rep == 12) %>%
  group_by(Day)%>%
  summarize(mean_pop = mean(Population,na.rm = TRUE))
dat_rep12<- data.frame(dat_rep12)


(gg <- ggplot(aes(Day,mean_pop), data = dat_rep12) +geom_point())


##
dat_rep18 <- dat %>%
  filter(Rep == 18) %>%
  group_by(Day)%>%
  summarize(mean_pop = mean(Population,na.rm = TRUE))
dat_rep18<- data.frame(dat_rep18)


(gg <- ggplot(aes(Day,mean_pop), data = dat_rep18) +geom_point())


##
dat_rep23 <- dat %>%
  filter(Rep == 23) %>%
  group_by(Day)%>%
  summarize(mean_pop = mean(Population,na.rm = TRUE))
dat_rep23<- data.frame(dat_rep23)


(gg <- ggplot(aes(Day,mean_pop), data = dat_rep23) +geom_point())


##
dat_rep24 <- dat %>%
  filter(Rep == 24) %>%
  group_by(Day)%>%
  summarize(mean_pop = mean(Population,na.rm = TRUE))
dat_rep24<- data.frame(dat_rep24)


(gg <- ggplot(aes(Day,mean_pop), data = dat_rep24) +geom_point())



## goal to take up to the highest point for each rep and fit poisson glmm (?)


newdat <- dat %>%
  group_by(Rep,Day) %>%
  mutate(avg_pop = mean(Population,na.rm = TRUE) )


newdat <- newdat %>%
  group_by(Rep) %>%
  mutate(row_seq = seq(1:n())) %>%
  filter(row_seq <= max(which(avg_pop == max(avg_pop, na.rm = TRUE))))

newdat

newg <- ggplot(aes(week,Population), data = newdat) + geom_point() + facet_wrap(~Rep)
newg


#drop reps 2, 22, 6,,4, 4A,5,11, and 10 due to dcrashing in less than 3 weeks
newdat <- newdat %>%
  filter(Rep != "2" & Rep != "22" & Rep != "6" & Rep != "4" & Rep != "4A" & Rep != "5" & 
          Rep != "11" & Rep !="10")

pop_grow <- glmer(Population ~ 0 + week + Avg_Chl + (1|Rep), data = newdat, family = poisson)
  

