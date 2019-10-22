library(tidyverse)


## daphnia
popA <- pop %>% select(TankNum, treatment, ExptDay, DaphniaC1Adult,DaphniaC2Adult,DaphniaC3,
                       DaphniaC4Adult,Liters) %>% filter(treatment != 5, treatment != 6)
popA <- popA %>% gather(key = "subsample", value= "count", -c(TankNum,treatment, ExptDay, Liters))

popJ <- pop %>% select(TankNum, treatment, ExptDay, DaphniaC1Juv,DaphniaC2Juv,DaphniaC3Juv,
                       DaphniaC4Juv,Liters)%>% 
  gather(key = "subsample", value= "count", -c(TankNum,treatment, ExptDay, Liters)) %>%
  filter(treatment != 5, treatment != 6)

## by 10 mL subsamples
ggplot(popA, aes(ExptDay,count)) +geom_point() + facet_wrap(~treatment)
ggplot(popJ, aes(ExptDay, count)) + geom_point()+ facet_wrap(~treatment)


## algae
str(dat)
ggplot(dat[dat$Chl < 100 & dat$Chl > 0,], aes(ExptDay, Chl)) + geom_point() + facet_wrap(~treatment)
