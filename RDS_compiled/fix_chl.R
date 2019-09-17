## figuring out which days flourimeter wasn't working right... before we noticed
source("treatments.R")
dat1 = dat %>% filter(treatment == 1)
date<- dat1 %>% unite("date", Day:Month, sep = "_", remove = TRUE)
dat1$date <- date$date


broke <- dat1 %>% filter(is.na(Chl))  ## February 11 and 12

dat2 <- dat1 %>% filter(Month == "January")  ## all looks good
ggplot(dat2, aes(date,Chl)) + geom_point()

dat3 <- dat1 %>% filter(Month == "February") %>% filter(Day < 12)
ggplot(dat3, aes(date,Chl)) + geom_point() ## the 10th is clearly bad

ggplot(dat3[dat3$Chl< 1000,], aes(date,Chl)) + geom_point() ## 9th might be ok but huge jump so to make sure will remove it

