library(readxl)
library(tidyverse)
library(ggplot2)
library(nlmrt)
dat2 <- read_excel("Daphnia_large_Feeding_Nov11.xlsx")
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

dat2$diff_chl <- dat2$chl1-dat2$chl2
dat2$diff_nh4 <- dat2$nh42-dat2$nh41
dat2$diff_chl_ind <- as.numeric(length(dat2))
dat2$diff_nh4_ind <- as.numeric(length(dat2))
#dat2$control <- rep(c(rep(1,5),rep(2,3)),5)
for (i in 1:nrow(dat2)){
  if (dat2[i, ]$control == 1) {
    dat2$diff_chl_ind[i] <- dat2$diff_chl[i]/dat2$Num_Daphnia[i]
    dat2$diff_nh4_ind[i] <- dat2$diff_nh4[i]/dat2$Num_Daphnia[i]
  } else {
    dat2$diff_chl_ind[i] <- dat2$diff_chl[i]
    dat2$diff_nh4_ind[i] <- dat2$diff_nh4[i]
  } 
}


newdat2 <- dat2[dat2$control != 2,]

Hollings1 <- function (t,x,a) {
  a*t*x
  
}

fit2 <- nlxb(formula = diff_chl_ind ~ a * Chl_Time_Diff * chl1,
             start = c(a = 1), data = data.frame(newdat2))

newdat2$pred_vals <- Hollings1(t = newdat2$Chl_Time_Diff, x = newdat2$chl1, a = fit2$coefficients)
newdat2$pred_upper <- Hollings1(t = newdat2$Chl_Time_Diff, x = newdat2$chl1, a = fit2$coefficients+5.439e-06 )
newdat2$pred_lower <- Hollings1(t = newdat2$Chl_Time_Diff, x = newdat2$chl1, a = fit2$coefficients-5.439e-06 )


dfnew <- data.frame(
  chl1 = seq(0,25,by=0.1),
  Chl_Time_Diff = 360
)

dfnew$pred_vals <- Hollings1(t = dfnew$Chl_Time_Diff, x = dfnew$chl1, a = fit2$coefficients)
dfnew$pred_upper <- Hollings1(t = dfnew$Chl_Time_Diff, x = dfnew$chl1, a = fit2$coefficients+7.497e-06)
dfnew$pred_lower <- Hollings1(t = dfnew$Chl_Time_Diff, x = dfnew$chl1, a = fit2$coefficients-7.497e-06)



ggplot(newdat2, aes(chl1, diff_chl_ind)) + 
  geom_point(lwd = 2.5) +
  geom_line(data=dfnew,aes(chl1, pred_vals), lwd = 1) + 
  geom_line(data=dfnew,aes(chl1, pred_upper), lwd = 0.5, color="blue",linetype= "dashed") +
  geom_line(data=dfnew,aes(chl1, pred_lower), lwd = 0.5, color="blue", linetype= "dashed") +
  #geom_ribbon(data=dfnew,aes(ymin=pred_lower,ymax=pred_upper, x=Chl_1), alpha=0.2)+
  theme(legend.position = "none")+ 
  xlab("Initial Chlorophyll Conc (ug/L)")+ylab("Individual Uptake of Chlorophyll (mg/L*min)")


ggplot(newdat2, aes(chl1, diff_chl_ind)) + geom_point(lwd = 4, aes(colour = as.factor(Treatment))) +
  geom_point(aes(chl1, pred_vals), lwd = 2) + geom_errorbar(aes(ymin=pred_lower,ymax=pred_upper, x=chl1))+
 theme(legend.position = "none")+
  xlab("Initial Chlorophyll Conc (ug/L)")+ylab("Individual Uptake of Chlorophyll (mg/L*unit time)")


#newdat2$diff_chl_ind <- as.numeric(newdat2$diff_chl_ind)
#newdat2$diff_nh4_ind <- as.numeric(newdat2$diff_nh4_ind)
#newdat2 <- newdat2 %>%
# group_by(Treatment) %>%
#summarize_each(funs(mean,sd),diff_chl_ind,diff_nh4_ind )

#newdat2$Start <- unlist(c(cleandat2[, 5]))

ggplot(newdat2, aes(Start, diff_chl_ind_mean)) + geom_point(lwd = 5)
ggplot(newdat2, aes(Start, diff_nh4_ind_mean)) + geom_point(lwd = 5)


b <- data.frame(
 diff_chl_ind = newdat2$diff_chl_ind,
 chl1 = newdat2$chl1,
 Treatment = newdat2$Treatment
)
b$time <- as.numeric(newdat2$Chl_Time_Diff)

fit2 <- nlxb(formula = diff_chl_ind ~ a * time * chl1,
             start = c(a = 1), data = data.frame(b))

b$pred_vals <- Hollings1(t = b$time, x = b$chl1, a = fit2$coefficients)
b$pred_upper <- Hollings1(t = b$time, x = b$chl1, a = fit2$coefficients+7.497e-06 )
b$pred_lower <- Hollings1(t = b$time, x = b$chl1, a = fit2$coefficients-7.497e-06 )



ggplot(b, aes(chl1, diff_chl_ind)) + geom_point(lwd = 4, aes(colour = as.factor(Treatment))) +
  geom_point(aes(chl1, pred_vals), lwd = 2) + geom_errorbar(aes(ymin=pred_lower,ymax=pred_upper, x=chl1))+
  theme_classic()+ theme(legend.position = "none")+
  xlab("Initial Chlorophyll Conc (mg/L)")+ylab("Individual Uptake of Chlorophyll (mg/L*unit time)")



ggplot(dat2, aes(chl1,diff_chl_ind))+geom_point(aes(color=as.factor(Treatment)))+
  scale_y_continuous(limits = c(-0.5,2))
