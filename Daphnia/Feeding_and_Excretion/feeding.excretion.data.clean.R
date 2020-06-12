rdat <- read.csv("Daphnia_large_Feeding_Nov11.csv")
dim(rdat)
## get average change in controls by treatment


#cont <- rdat %>% 
 # mutate(Chl_Time_Diff_day = (Chl_Time_Diff/60),Nh4_Time_Diff_day = (Nh4_Time_Diff/60)) %>%
 # filter(control == 2) %>% 
 # mutate(chl_diff =(chl1-chl2)/Chl_Time_Diff_day, nh4_diff= (nh42-nh41)/Nh4_Time_Diff_day) %>% # subtract 1 from 2 for change over time
 # group_by(Treatment) %>%
  #summarize(mean_chl = mean(chl_diff, na.rm = T), mean_nh4 = mean(nh4_diff,na.rm = T)) ## onr row in treatment 3 is all NAs..??

#dim(cont)
## do per hour instead of per day to match literature
cont <- rdat %>% 
  mutate(Chl_Time_Diff_hour = (Chl_Time_Diff/60),Nh4_Time_Diff_hour = (Nh4_Time_Diff/60)) %>%
  filter(control == 2) %>% 
  mutate(chl_diff =(chl1-chl2)/Chl_Time_Diff_hour, nh4_diff= (nh42-nh41)/Nh4_Time_Diff_hour) %>% # subtract 1 from 2 for change over time
  group_by(Treatment) %>%
  summarize(mean_chl = mean(chl_diff, na.rm = T), mean_nh4 = mean(nh4_diff,na.rm = T))



## add mean control avg back to main dataframe
dat <- left_join(rdat,cont)
dim(dat)
## account for controls


dim(dat)


dat <- dat %>%
  filter(control == 1) %>%
  mutate(Chl_Time_Diff_hour= (Chl_Time_Diff/60),Nh4_Time_Diff_hour = (Nh4_Time_Diff/60)) %>%
  mutate(chl_diff = (chl1-chl2)/Chl_Time_Diff_hour, nh4_diff = (nh41-nh42)/Nh4_Time_Diff_hour) %>%
  mutate(chl_diff_cc = (chl_diff-mean_chl)/Num_Daphnia, nh4_diff_cc = (nh4_diff-mean_nh4)/Num_Daphnia)


dat1 <- dat %>% filter(control == 1) %>% filter(!is.na(chl_diff_cc)) %>% ## 1 entry is NA
  dplyr::select(Rep, Treatment,chl1,nh41, chl_diff_cc,nh4_diff_cc, Num_Daphnia)


### add in literature
feed_lit <- read.csv("feeding.csv")
excretion_lit <- read.csv("excretion_lit.csv")


#with(feed_lit,which(!is.na(algal_conc_cellperml)))

feed_lit <- feed_lit %>% 
  filter(!is.na(algal_conc_cellperml)) %>% 
  dplyr::select(Title, replicates,point_est,point_est_cell_indiv_day,point_error,algal_conc_cellperml,sd)


index_sd <- with(feed_lit, which(is.na(sd))) ## index of which rows have missing SD 
missing_n <- length(index_sd)

##copy dataframe
feed_lit1 <- feed_lit

## convert cells to chl in lit
feed_lit1$chl <- cell_adj(feed_lit1$algal_conc_cellperml)
feed_lit1$sd_feed <- cell_adj(feed_lit1$sd)
feed_lit1$diff <- cell_adj(feed_lit1$point_est)

## replace NAs with dummy
feed_lit1[is.na(feed_lit1)] <- 100

## convert chl to cells in my data
dat1$cells <- chl_adj(dat1$chl1)
dat1$cell_diff <-chl_adj(dat1$chl_diff_cc) 


### first only incorporate feeding lit that has sds (only 3 studies) and allow slope to vary
feed_lit2 <- feed_lit1 %>% filter(sd != 100)


