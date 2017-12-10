library(readxl)
library(tidyverse)
library(ggplot2)
library(nlmrt)
dat <- read_excel("Ceriodaphnia_Feeding_Nov07_2017.xlsx")

theme_set(theme_bw())
theme_update(axis.text.x = element_text(size = 16),
             axis.text.y = element_text(size = 16),
             axis.title.x = element_text(size = 16),
             axis.title.y = element_text(size = 16),
             legend.title = element_text(size = 12),
             legend.text = element_text(size = 14),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             panel.margin = unit(0, "lines"),
             legend.key.size = unit(.85, "cm"),
             legend.key = element_rect(fill = "white"),
             panel.margin.y = unit(0.5, "lines"),
             panel.border = element_rect(colour = "black", 
                                         fill = NA, size = 1),
             strip.text.x = element_text(size = 16, colour = "black", 
                                         face = "bold"))

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
newdat$pred_upper <- Hollings1(t = newdat$Chl_Time_Diff, x = newdat$Chl_1, a = fit$coefficients+6.285e-07)
newdat$pred_lower <- Hollings1(t = newdat$Chl_Time_Diff, x = newdat$Chl_1, a = fit$coefficients-6.285e-07)
  
dfnew <- data.frame(
  Chl_1 = seq(0,22,by=0.1),
  Chl_Time_Diff = 360
)

dfnew$pred_vals <- Hollings1(t = dfnew$Chl_Time_Diff, x = dfnew$Chl_1, a = fit$coefficients)
dfnew$pred_upper <- Hollings1(t = dfnew$Chl_Time_Diff, x = dfnew$Chl_1, a = fit$coefficients+6.285e-07)
dfnew$pred_lower <- Hollings1(t = dfnew$Chl_Time_Diff, x = dfnew$Chl_1, a = fit$coefficients-6.285e-07)

ggplot(newdat, aes(Chl_1, diff_chl_ind)) + 
  geom_point(lwd = 2.5) +
 geom_line(data=dfnew,aes(Chl_1, pred_vals), lwd = 1) + 
  geom_line(data=dfnew,aes(Chl_1, pred_upper), lwd = 0.5, color="blue",linetype= "dashed") +
  geom_line(data=dfnew,aes(Chl_1, pred_lower), lwd = 0.5, color="blue", linetype= "dashed") +
  #geom_ribbon(data=dfnew,aes(ymin=pred_lower,ymax=pred_upper, x=Chl_1), alpha=0.2)+
  theme(legend.position = "none")+ 
  xlab("Initial Chlorophyll Conc (ug/L)")+ylab("Individual Uptake of Chlorophyll (mg/L*min)")

#newdat$diff_chl_ind <- as.numeric(newdat$diff_chl_ind)
#newdat$diff_nh4_ind <- as.numeric(newdat$diff_nh4_ind)
#newdat <- newdat %>%
 ## group_by(Treatment) %>%
  #summarize_each(funs(mean,sd),diff_chl_ind,diff_nh4_ind )

#newdat$Start <- unlist(c(cleandat[, 5]))

ggplot(newdat, aes(Start, diff_chl_ind_mean)) + geom_point(lwd = 5)
ggplot(newdat, aes(Start, diff_nh4_ind_mean)) + geom_point(lwd = 5)
