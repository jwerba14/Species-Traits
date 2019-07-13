cerio_feed <- read.csv("Ceriodaphnia_Feeding_Nov07_2017.csv")
cont <- cerio_feed %>% 
  filter(Control.Y.N == "Y") %>% 
  mutate()
  mutate(chl_diff =(Chl.1-chl2)/Chl_Time_Diff, nh4_diff= (Nh4.1-Nh4.2)/Nh4_Time_Diff) %>%  # subtract first point from second for change
  group_by(Treatment) %>%
  summarize(mean_chl = mean(chl_diff, na.rm = T), mean_nh4 = mean(nh4_diff,na.rm = T)) ## onr row in treatment 3 is all NAs..??


## add mean control avg back to main dataframe
dat <- left_join(cerio_feed,cont)

## account for controls

dat <- dat %>%
  mutate(chl_diff = (Chl.1-chl2)/Chl_Time_Diff, nh4_diff = (Nh4.1-Nh4.2)/Nh4_Time_Diff/X..of.ceriodaphnia) %>%
  mutate(chl_diff_cc = chl_diff-mean_chl, nh4_diff_cc = nh4_diff-mean_nh4)

dat1 <- dat %>% filter(Control.Y.N == "N") %>%
  select(Rep.., Treatment,Chl.1,Nh4.1, chl_diff_cc,nh4_diff_cc, X..of.ceriodaphnia)

ggplot(dat1, aes(Chl.1,chl_diff_cc)) + geom_point()
