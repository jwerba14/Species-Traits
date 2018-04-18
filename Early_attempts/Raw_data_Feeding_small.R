 library(readxl)
 library(tidyverse)
 library(ggplot2)
dat <- read_excel("Data_sheet_FeedingExpt.xlsx")
names(dat); str(dat)dfodoidh
ggplot(dat, aes(Treatment, 'Chl 2' / 'Chl 1'))+geom_point(lwd = 2)

#sort chl and nh4 by time and treatment - obviously this currently is assuming all same number of daphnia and same time elapsed
cleandat <- dat %>%
  group_by(Treatment) %>%
  summarize_all(funs(mean,sd), na.rm=TRUE)

cleandat<- cleandat[,c(1,5,7,11,13,21,23,27,29)]

cdat1 <- data.frame(cleandat[,c(2,3,6,7)])
colnames(cdat1) <- c("chl_mean", "Nh4_mean", "chl_SD", "Nh4_SD")
cdat2 <- cleandat[,c(4,5,8,9)]
colnames(cdat2) <- c("chl_mean", "Nh4_mean", "chl_SD", "Nh4_SD")
newdat <- bind_rows(cdat1,cdat2)
newdat$time <- rep(1:2, each=5)
newdat$treat <- as.factor(rep(1:5, 2))

newdat1 <- newdat[newdat$time == "1", ]
newdat2 <- newdat[newdat$time == "2", ]
new_dat <- cbind(newdat1, data.frame(chl_mean_end = newdat2$chl_mean))

ggplot(new_dat, aes(chl_mean, chl_mean_end / chl_mean))+geom_point(lwd = 5)

ggplot(newdat, aes(time, chl_mean))+geom_point(aes(color=treat))+ 
  geom_errorbar(aes(ymin=chl_mean-chl_SD, ymax=chl_mean+chl_SD))

ggplot(newdat, aes(time, Nh4_mean))+geom_point(aes(color=treat))+ 
  geom_errorbar(aes(ymin=Nh4_mean-Nh4_SD, ymax=Nh4_mean+Nh4_SD))
                                