## graph with just lab data

source("../Graphing_Set_Up.R")
source("../treatments.R")

predlow <- as.data.frame(out_lwr)
predlow$quantile <- "low"
predup <- as.data.frame(out_upr)
predup$quantile <- "upper"
predmed <- as.data.frame(out_med)
predmed$quantile <- "med"


# the filter remves 3 points at 60 which have to be wrong...all on the same day
## what a mess............
dat_daph <- dat %>% filter(treatment == 3) %>% filter(NH4 < 20)%>%  
  dplyr::select(TankNum, NH4, Chl,Day)
names(dat_daph) <- c("TankNum", "ammonium","algae","time")
amm_g <- ggplot(predmed, aes(time,ammonium)) + geom_line(linetype = "solid") + 
  geom_line(data= predup, linetype = "dotdash") + geom_line(data=predlow, linetype ="dotdash")+
 geom_point(data = dat_daph,aes(time,ammonium) )+
  ylab("Ammonium mg/L")+xlab("Days")


print(amm_g)

alg_g <- ggplot(predmed, aes(time,algae)) + geom_line(linetype = "solid")+ 
  geom_line(data= predup, linetype = "dotdash") + geom_line(data=predlow, linetype ="dotdash")+
  theme_bw()+ geom_point(data = dat_daph,aes(time,algae) )+
  ylab("Chl a ug/L")+xlab("Days")+
  theme(axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 32),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 32),
        strip.text = element_text(size = 0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank()) 

print(alg_g)

source("pop_curves.R")
adult <- popA %>% filter(treatment == 3) %>% group_by(TankNum,ExptDay) %>%
  summarize(pop_mean = mean(count)*(mean(Liters)/.01)) %>% dplyr::select(TankNum, ExptDay, pop_mean)
names(adult) <- c("TankNum", "time","daph_a")

juv <- popJ %>% filter(treatment == 3) %>% group_by(TankNum,ExptDay) %>%
  summarize(pop_mean = mean(count)*(mean(Liters)/.01))
names(juv) <- c("TankNum","time","daph_j")

ad_graph <- ggplot(predmed, aes(time,daph_a)) + geom_line(linetype = "solid")+ 
  geom_line(data= predup, linetype = "dotdash") + geom_line(data=predlow, linetype ="dotdash")+
  theme_bw()+ geom_point(data = adult,aes(time,daph_a) )+
  ylab("Adult Daphnia")+xlab("Days")+
  theme(axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 32),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 32),
        strip.text = element_text(size = 0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank()) 

juv_graph <- ggplot(predmed, aes(time,daph_j)) + geom_line(linetype = "solid")+ 
  geom_line(data= predup, linetype = "dotdash") + geom_line(data=predlow, linetype ="dotdash")+
  theme_bw()+ geom_point(data = juv,aes(time,daph_j) )+
  ylab("Adult Daphnia")+xlab("Days")+
  theme(axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 32),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 32),
        strip.text = element_text(size = 0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
