## juvenile daphnia feeding and excretion
source("../../transfer_functions.R")
source("../../Graphing_Set_Up.R")
library(tidyverse)

### hmm some problems with cc change-- need to double check

rdatj <- read.csv("Small_Daph_Feeding.csv")
## missing data for treatments 6 and 7, if I can get back into lab can find missing data...for now drop

names(rdatj)[names(rdatj)=="Rep.."] <- "Rep"

rdatj <- rdatj %>% 
  filter(!is.na(Chl_Time_Diff)) 
dim(rdatj)

cont <- rdatj %>% 
  mutate(chl_time_diff_h = Chl_Time_Diff/60, nh4_time_diff_h = Nh4_Time_Dif/60 ) %>%
  filter(Control.Y.N == "Y") %>% 
  mutate(chl_diff =((Chl.1-Chl.2)/chl_time_diff_h), nh4_diff= ((Nh4.1-Nh4.2)/nh4_time_diff_h)) %>% ## change per hour
  group_by(Treatment) %>%
  summarize(mean_chl = mean(chl_diff, na.rm = T), mean_nh4 = mean(nh4_diff,na.rm = T)) ## onr row in treatment 3 is all NAs..??
dim(cont)

## add mean control avg back to main dataframe
dat <- left_join(rdatj,cont)
dim(dat)

## need to add in diff from control bc if positive algae grew so indiv actually ate more if neg indiv ate less???

dat <- dat %>%
  filter(Control.Y.N == "N") %>%
  mutate(chl_time_diff_h = (Chl_Time_Diff/60),nh4_time_diff_h = (Nh4_Time_Dif/60)) %>%
  mutate(chl_diff = (Chl.1-Chl.2)/chl_time_diff_h, nh4_diff = (Nh4.1-Nh4.2)/nh4_time_diff_h) %>%
  mutate(chl_diff_cc = (chl_diff-mean_chl)/Num_Daphnia, nh4_diff_cc = (nh4_diff-mean_nh4)/Num_Daphnia)
dim(dat)



dat1 <- dat %>%  filter(nh4_diff_cc > -5) %>% ## remove one weird measurement
  dplyr::select(Rep, Treatment,Chl.1,Nh4.1, chl_diff_cc,nh4_diff_cc, Num_Daphnia)

#ggplot(dat1, aes(Chl.1,chl_diff_cc)) + geom_point()

mod_lm <- lm(data = dat1, chl_diff_cc ~ -1+Chl.1)


#newpred <- sat_fun(k= seq(1,100,1), a=960892 ,b =1339121459)

newdata = data.frame(Chl.1 = seq(1,25,0.1))
newpred1 <- as.data.frame(predict(mod_lm, newdata = newdata, interval = "confidence"))
newdata$chl_diff_cc <- newpred1$fit
newdata$lwr <- newpred1$lwr
newdata$upr <- newpred1$upr

j_feed_g <- ggplot(data = dat1, aes(Chl.1, chl_diff_cc)) + geom_point() +
  geom_line(data = newdata) + 
  geom_ribbon(data = newdata, aes(ymin = lwr, ymax= upr),alpha = 0.3) +
  xlab("Chlorphyll a (ug/L)") + 
  ylab(str_wrap("Change in Chlorophyll a/Juvenile Daphnia/Day", width = 25)) +
  ggtitle("LS: Linear Fit")

print(j_feed_g)
