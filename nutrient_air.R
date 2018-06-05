library(tidyverse)
library(ggplot2)
library(bbmle)
nitrate <- read.csv("NO3_Air.csv")
nitrate <- nitrate %>% drop_na()

#for parameter cnitrate and n? should remove n since cant measure nitrification
#drop reps 1 and 15 because first 2 of the day and machine was weird 
#and you can see that when looking at numbers
nitrate <- nitrate %>%
  filter(Rep != 1 & Rep != 15)

n1 <- with(nitrate, lm(NO3~ Day))
(g1 <- ggplot(nitrate, aes(Day,NO3)) +
    geom_point(aes(color=as.factor(Treat))))
# intercept = 1.55 std error = 0.02 p <<< 0.001
# slope over time = 0.1057 std error = 0.006576 p<<0.001
# no3 = 0.1057(day) + 1.55
# dn/dt = 0.1057

#for parameter cammonium
ammonium <- read.csv("Nh4_Air.csv")

#remove replicates that spilled (e.g NH4 == NA)

ammonium <- ammonium %>% drop_na()
#same early morning machine problem
ammonium <- ammonium %>%
  filter(Rep != 1 & Rep != 15)

(g2 <- ggplot(ammonium2, aes(Day,NH4)) + geom_point(aes(color=as.factor(Treat))))

m <- with(ammonium, lm(NH4 ~ Day*(Treat)))

ammonium2 <- ammonium %>%
  filter(!(Treat==4 & Day==4 & Rep==52))

inidf <- ammonium2 %>%
  filter(Day==1) %>%
  group_by(Treat) %>%
  summarize(initial=mean(NH4))

fits <- merge(ammonium2, inidf) %>%
  group_by(Treat) %>%
  #do() applies function to each group
  do(m=mle2(NH4~dnorm(mean=exp(log.A0) * exp(-exp(log.m) * (Day-1)), sd=exp(log.sigma)),
         start=list(log.A0=log(unique(.$initial)), log.m=log(0.1), log.sigma=log(1)),
         data=.)) # . is dataframe of that treatment

simdf <- lapply(fits$m, function(m){
  cc <- coef(m)
  
  A0 <- exp(cc[1])
  m <- exp(cc[2])
  
  data.frame(
    Day=1:5,
    NH4=A0 * exp(-m * 0:4)
  )
}) %>%
  bind_rows(.id="Treat")

g2 +
  geom_line(data=simdf, aes(col=Treat))

lapply(fits$m, function(m){
  data.frame(
    m=exp(coef(m)[2])
  )
}) %>%
  bind_rows(.id="Treat") %>%
  filter(Treat != 4) %>%
  summarize(m=mean(m))


