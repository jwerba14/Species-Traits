library(tidyverse)
library(ggplot2)
nitrate <- read.csv("NO3_Air.csv")
nitrate <- nitrate %>% drop_na()

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
ammonium <- read.csv("Nh4_Air.csv")

#remove replicates that spilled (e.g NH4 == NA)

ammonium <- ammonium %>% drop_na()
#same early morning machine problem
ammonium <- ammonium %>%
  filter(Rep != 1 & Rep != 15)

(g1 <- ggplot(ammonium, aes(Day,NH4)) + geom_point(aes(color=as.factor(Treat))))

m <- with(ammonium, lm(NH4 ~ Day*as.factor(Treat)))
