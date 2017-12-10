library(readxl)
library(tidyverse)
library(ggplot2)
dat1 <- read_excel("data_sheet_FeedingExpt.xlsx")
dat <- read_excel("Ceriodaphnia_Feeding_Nov07_2017.xlsx")
dat2 <- read_excel("Daphnia_large_Feeding_Nov11.xlsx")



Hollings1 <- function (t,x,a) {
  a*t*x
  
}



#large
dat2$diff_nh4 <- dat2$nh42-dat2$nh41
dat2$diff_nh4_ind <- as.numeric(length(dat2))
for (i in 1:nrow(dat2)){
  if (dat2[i, ]$control == 1) {
     dat2$diff_nh4_ind[i] <- dat2$diff_nh4[i]/dat2$Num_Daphnia[i]
  } else {
    dat2$diff_nh4_ind[i] <- dat2$diff_nh4[i]
  } 
}


newdat2 <- dat2[dat2$control != 2,]
fit2 <- nlxb(formula = diff_nh4_ind ~ a * Nh4_Time_Diff *chl1 ,
            start = c(a = 1), data = data.frame(newdat2))

newdat2$pred_vals <- Hollings1(t = newdat2$Nh4_Time_Diff, x = newdat2$chl1, a = fit2$coefficients)
newdat2$pred_upper <- Hollings1(t = newdat2$Nh4_Time_Diff, x =  newdat2$chl1, a = fit2$coefficients+4.457e-06)
newdat2$pred_lower <- Hollings1(t = newdat2$Nh4_Time_Diff, x =  newdat2$chl1, a = fit2$coefficients-4.457e-06)



ggplot(newdat2, aes(chl1, diff_nh4_ind)) + geom_point(lwd = 4, aes(colour = as.factor(Treatment))) +
  geom_point(aes(chl1, pred_vals), lwd = 2) + geom_errorbar(aes(ymin=pred_lower,ymax=pred_upper, x=chl1))+
  theme_classic()+ theme(legend.position = "none")+
  xlab("Initial Chlorohyll Conc (mg/L)")+ylab("Individual Release of Ammonium (mg/L*unit time)")












##cerio
dat$diff_nh4 <- dat$`Nh4 2`-dat$`Nh4 1`
dat$control <- rep(c(rep(1,5),rep(2,3)),5)
dat$diff_nh4_ind <- as.numeric(length(dat))
for (i in 1:nrow(dat)){
  if (dat[i, ]$control == 1) {
    dat$diff_nh4_ind[i] <- dat$diff_nh4[i]/dat$`# of ceriodaphnia`[i]
  } else {
    dat$diff_nh4_ind[i] <- dat$diff_nh4[i]
  } 
}

names(dat)[8] <- "Chl_1"
names(dat)[9] <- "Nh4_1"
newdat <- dat[dat$control != 2,]
fit <- nlxb(formula = diff_nh4_ind ~ a * Nh4_Time_Diff *Chl_1 ,
             start = c(a = 1), data = data.frame(newdat))

newdat$pred_vals <- Hollings1(t = newdat$Nh4_Time_Diff, x = newdat$Chl_1, a = fit$coefficients)
newdat$pred_upper <- Hollings1(t = newdat$Nh4_Time_Diff, x =  newdat$Chl_1, a = fit$coefficients+3.111e-07)
newdat$pred_lower <- Hollings1(t = newdat$Nh4_Time_Diff, x =  newdat$Chl_1, a = fit$coefficients-3.111e-07)



ggplot(newdat, aes(Chl_1, diff_nh4_ind)) + geom_point(lwd = 4, aes(colour = as.factor(Treatment))) +
  geom_point(aes(Chl_1, pred_vals), lwd = 2) + geom_errorbar(aes(ymin=pred_lower,ymax=pred_upper, x=Chl_1))+
  theme_classic()+ theme(legend.position = "none")+
  xlab("Initial Chlorohyll Conc (mg/L)")+ylab("Individual Release of Ammonium (mg/L*unit time)")



##small

dat1$diff_nh4 <- dat1$`Nh4 2`-dat1$`Nh4 1`

dat1$diff_nh4_ind <- as.numeric(length(dat1))
dat1$control <- rep(c(rep(1,5),rep(2,3)),5)
for (i in 1:nrow(dat1)){
  if (dat1[i, ]$control == 1) {
    dat1$diff_nh4_ind[i] <- dat1$diff_nh4[i]/dat1$`# of Daphnia`[i]
  } else {
    dat1$diff_nh4_ind[i] <- dat1$diff_nh4[i]
  } 
}

names(dat1)[6] <- "Chl_1"
names(dat1)[9] <- "Nh4_1"
newdat1 <- dat1[dat1$control != 2,]
fit1 <- nlxb(formula = diff_nh4_ind ~ a * Nh4_Time_Dif *Chl_1 ,
             start = c(a = 1), data = data.frame(newdat1))

newdat1$pred_vals <- Hollings1(t = newdat1$Nh4_Time_Dif, x = newdat1$Chl_1, a = fit1$coefficients)
newdat1$pred_upper <- Hollings1(t = newdat1$Nh4_Time_Dif, x =  newdat1$Chl_1, a = fit1$coefficients+1.743e-06)
newdat1$pred_lower <- Hollings1(t = newdat1$Nh4_Time_Dif, x =  newdat1$Chl_1, a = fit1$coefficients-1.743e-06)



ggplot(newdat1, aes(Chl_1, diff_nh4_ind)) + geom_point(lwd = 4, aes(colour = as.factor(Treatment))) +
  geom_point(aes(Chl_1, pred_vals), lwd = 2) + geom_errorbar(aes(ymin=pred_lower,ymax=pred_upper, x=Chl_1))+
  theme_classic()+ theme(legend.position = "none")+
  xlab("Initial Chlorohyll Conc (mg/L)")+ylab("Individual Release of Ammonium (mg/L*unit time)")






