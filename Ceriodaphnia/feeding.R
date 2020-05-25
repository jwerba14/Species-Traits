library(tidyverse)

cerio_feed <- read.csv("Ceriodaphnia_Feeding_Nov07_2017.csv")
dim(cerio_feed)
source("../transfer_functions.R")
source("../Graphing_Set_Up.R")




cont <- cerio_feed %>% 
  filter(Control.Y.N == "Y") %>% 
  mutate(chl_diff =(Chl.1-chl2)/Chl_Time_Diff, nh4_diff= (Nh4.1-Nh4.2)/Nh4_Time_Diff) %>% 
  filter(chl_diff > -1) %>%# subtract first point from second for change
  group_by(Treatment, date) %>%
  summarize(mean_chl = mean(chl_diff, na.rm = T), mean_nh4 = mean(nh4_diff,na.rm = T)) ## one row in treatment 3 is all NAs..??


## add mean control avg back to main dataframe
dat <- left_join(cerio_feed,cont)

dim(dat)

## account for controls

dat <- dat %>%
  mutate(chl_diff = (Chl.1-chl2)/Chl_Time_Diff, nh4_diff = (Nh4.1-Nh4.2)/Nh4_Time_Diff) %>%   ## difference between time 1 and 2
  mutate(chl_diff_cc = chl_diff-mean_chl, nh4_diff_cc = nh4_diff-mean_nh4)%>%                 ## subtract out mean control      
  mutate(chl_diff_ccd = (chl_diff_cc)/(60*60) , nh4_diff_ccd = (nh4_diff_cc)/(60*60)) %>%       ## make into per hour 
  filter(Control.Y.N == "N") %>%                                                                            ## remove rows with controls (no cerio) 
  mutate(chl_diff_ccdi = chl_diff_ccd/X..of.ceriodaphnia, nh4_diff_ccdi = nh4_diff_ccd/X..of.ceriodaphnia) %>%   ## per individual
  filter(chl_diff_ccdi != "NA")

#nrow(cerio_feed %>% filter(Control.Y.N == "N"))


feeding <- lm(chl_diff_ccdi ~ -1+Chl.1, data = dat)



Chl.1 <- data.frame(Chl.1 =  seq(0,25, by =0.1))
confidence <- as.data.frame(predict(feeding, newdata = Chl.1, interval = "confidence"))

newdat_ls <- data.frame(Chl.1 = seq(0,25, by =0.1),
                        chl_diff_ccdi = confidence$fit,
                        upper = confidence$upr,
                        lower = confidence$lwr)



(feed_g <- ggplot(data = dat, aes(Chl.1, chl_diff_ccdi)) + geom_point() + 
    geom_ribbon(data = newdat_ls, aes(ymax = upper, ymin=lower), linetype = "dotdash", alpha = 0.2) + 
    geom_line(data = newdat_ls) +
    xlab("Chlorophyll a (ug/L)") + 
    ylab("Change in Chlorphyll per individual hour") )

