dat <- read.csv("Algae_Nutrient.csv")
library(tidyr)

dat$rep1 <- rep(c(1:30),11)

ch <- data.frame(dat$date1,dat$chl,dat$rep1)
colnames(ch)<- c("date1","chl","rep1")
ch1 <- ch %>% 
    spread(rep1,chl)
ch1 <- as.matrix(ch1)
ch1 <- ch1[,-1]
nh <- data.frame(dat$date1,dat$nh4,dat$rep1)
colnames(nh)<- c("date1","nh4","rep1")
nh1 <- nh %>% 
  spread(rep1,nh4)
nh1 <- nh1[,-1]


#ggplot(dat[dat$treat==54,], aes(nh4,chl)) +geom_point()+geom_smooth