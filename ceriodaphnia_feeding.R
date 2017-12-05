library(readxl)
library(tidyverse)
library(ggplot2)
library(nlmrt)
dat <- read_excel("Ceriodaphnia_Feeding_Nov07_2017.xlsx")

#need to average by # of daphnia and remove controls

dat$diff_chl <- dat$`Chl 1`-dat$`chl2`
dat$diff_nh4 <- dat$`Nh4 2`-dat$`Nh4 1`
#dat$diff_nh4_ind <- dat$diff_nh4/dat$`# of ceriodaphnia`
dat$control <- rep(c(rep(1,5),rep(2,3)),5)
dat$diff_chl_ind <- as.numeric(length(dat))
dat$diff_nh4_ind <- as.numeric(length(dat))
for (i in 1:nrow(dat)){
  if (dat[i, ]$control == 1) {
    dat$diff_chl_ind[i] <- dat$diff_chl[i]/dat$`# of ceriodaphnia`[i]
    dat$diff_nh4_ind[i] <- dat$diff_nh4[i]/dat$`# of ceriodaphnia`[i]
  } else {
    dat$diff_chl_ind[i] <- dat$diff_chl[i]
    dat$diff_nh4_ind[i] <- dat$diff_nh4[i]
  } 
}

names(dat)[8] <- "Chl_1"
newdat <- dat[dat$control != 2,]

Hollings1 <- function (t,x,a) {
 a*t*x
  
}

fit <- nlxb(formula = diff_chl_ind ~ a * Chl_Time_Diff * Chl_1,
                start = c(a = 1), data = data.frame(newdat))

newdat$pred_vals <- Hollings1(t = newdat$Chl_Time_Diff, x = newdat$Chl_1, a = fit$coefficients)

ggplot(newdat, aes(Chl_1, diff_chl_ind)) + geom_point(lwd = 4, aes(colour = as.factor(Treatment))) +
  geom_point(aes(Chl_1, pred_vals), lwd = 2) + theme_classic()

#newdat$diff_chl_ind <- as.numeric(newdat$diff_chl_ind)
#newdat$diff_nh4_ind <- as.numeric(newdat$diff_nh4_ind)
newdat <- newdat %>%
  group_by(Treatment) %>%
  summarize_each(funs(mean,sd),diff_chl_ind,diff_nh4_ind )

newdat$Start <- unlist(c(cleandat[, 5]))

ggplot(newdat, aes(Start, diff_chl_ind_mean)) + geom_point(lwd = 5)
ggplot(newdat, aes(Start, diff_nh4_ind_mean)) + geom_point(lwd = 5)
