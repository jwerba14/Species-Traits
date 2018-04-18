library(readxl)
library(tidyverse)
library(ggplot2)
dat1 <- read_excel("data_sheet_FeedingExpt.xlsx")

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

#need to average by # of daphnia and remove controls-- small daphnnia

dat1$diff_chl <- dat1$`Chl 1`-dat1$`Chl 2`
dat1$diff_nh4 <- dat1$`Nh4 2`-dat1$`Nh4 1`
dat1$diff_chl_ind <- as.numeric(length(dat1))
dat1$diff_nh4_ind <- as.numeric(length(dat1))
dat1$control <- rep(c(rep(1,5),rep(2,3)),5)
for (i in 1:nrow(dat1)){
  if (dat1[i, ]$control == 1) {
    dat1$diff_chl_ind[i] <- dat1$diff_chl[i]/dat1$`# of Daphnia`[i]
    dat1$diff_nh4_ind[i] <- dat1$diff_nh4[i]/dat1$`# of Daphnia`[i]
  } else {
    dat1$diff_chl_ind[i] <- dat1$diff_chl[i]
    dat1$diff_nh4_ind[i] <- dat1$diff_nh4[i]
    } 
}
  
names(dat1)[6] <- "Chl_1"
newdat1 <- dat1[dat1$control != 2,]

Hollings1 <- function (t,x,a) {
  a*t*x
  
}

fit1 <- nlxb(formula = diff_chl_ind ~ a * Chl_Time_Diff * Chl_1,
            start = c(a = 1), data = data.frame(newdat1))

newdat1$pred_vals <- Hollings1(t = newdat1$Chl_Time_Diff, x = newdat1$Chl_1, a = fit1$coefficients)
newdat1$pred_upper <- Hollings1(t = newdat1$Chl_Time_Diff, x = newdat1$Chl_1, a = fit1$coefficients+2.435e-06)
newdat1$pred_lower <- Hollings1(t = newdat1$Chl_Time_Diff, x = newdat1$Chl_1, a = fit1$coefficients-2.435e-06)





dfnew <- data.frame(
  Chl_1 = seq(0,22,by=0.1),
  Chl_Time_Diff = 360
)

dfnew$pred_vals <- Hollings1(t = dfnew$Chl_Time_Diff, x = dfnew$Chl_1, a = fit1$coefficients)
dfnew$pred_upper <- Hollings1(t = dfnew$Chl_Time_Diff, x = dfnew$Chl_1, a = fit1$coefficients+6.285e-07)
dfnew$pred_lower <- Hollings1(t = dfnew$Chl_Time_Diff, x = dfnew$Chl_1, a = fit1$coefficients-6.285e-07)

ggplot(newdat1, aes(Chl_1, diff_chl_ind)) + 
  geom_point(lwd = 2.5) +
  geom_line(data=dfnew,aes(Chl_1, pred_vals), lwd = 1) + 
  geom_line(data=dfnew,aes(Chl_1, pred_upper), lwd = 0.5, color="blue",linetype= "dashed") +
  geom_line(data=dfnew,aes(Chl_1, pred_lower), lwd = 0.5, color="blue", linetype= "dashed") +
  #geom_ribbon(data=dfnew,aes(ymin=pred_lower,ymax=pred_upper, x=Chl_1), alpha=0.2)+
  theme(legend.position = "none")+ 
  xlab("Initial Chlorophyll Conc (ug/L)")+ylab("Individual Uptake of Chlorophyll (mg/L*min)")






ggplot(newdat1, aes(Chl_1, diff_chl_ind)) + geom_point(lwd = 4, aes(colour = as.factor(Treatment))) +
  geom_point(aes(Chl_1, pred_vals), lwd = 2) + geom_errorbar(aes(ymin=pred_lower,ymax=pred_upper, x=Chl_1))+
  theme_classic()+ theme(legend.position = "none")+
  xlab("Initial Chlorophyll Conc (mg/L)")+ylab("Individual Uptake of Chlorophyll (mg/L*unit time)")


#newdat1$diff_chl_ind <- as.numeric(newdat1$diff_chl_ind)
#newdat1$diff_nh4_ind <- as.numeric(newdat1$diff_nh4_ind)
#newdat1 <- newdat1 %>%
 # group_by(Treatment) %>%
  #summarize_each(funs(mean,sd),diff_chl_ind,diff_nh4_ind )

#newdat1$Start <- unlist(c(cleandat1[, 5]))

ggplot(newdat1, aes(Start, diff_chl_ind_mean)) + geom_point(lwd = 5)
ggplot(newdat1, aes(Start, diff_nh4_ind_mean)) + geom_point(lwd = 5)
