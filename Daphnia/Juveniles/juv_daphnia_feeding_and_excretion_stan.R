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


## ammonium excretion

#lm(data=dat1[dat1$nh4_diff_cc < 0 ,], nh4_diff_cc/chl_diff_cc ~ 1)


#ggplot(dat1, aes(Nh4.1,nh4_diff_cc/Chl.1)) + geom_point()+geom_smooth(method = "lm")
#mod_nh4 <- lm(data = dat1, nh4_diff_cc ~ Nh4.1)
#mod_int_only <- lm(data = dat1, nh4_diff_cc ~1)
#pred <- predict(mod_int_only)
#with(dat1, plot(Nh4.1,nh4_diff_cc))
#lines(pred)


#mod_sat <- nlxb(data = dat1, nh4_diff_cc ~ (Nh4.1*a)/(Nh4.1+b), start = list(a=1,b=1))

##newpred <- sat_fun(k= seq(5,25,.1), a= -0.00161455 ,b =15.609)


#newdata = data.frame(Nh4.1 = seq(5,25,0.1))
#newpred2 <- predict(mod_nh4, newdata = newdata)
#newpred3 <- predict(mod_int_only, newdata = newdata)


## fit in stan
daph_feex_listj <- 
  list(
    N = nrow(dat1),
    chl = dat1$Chl.1,
    diff_chl = dat1$chl_diff_cc,
    diff_nh4 = dat1$nh4_diff_cc
  )


## same as adult model, should I force excretion to be about 0?? 
##we really can't estimate it unfortunately-- could add in machine error (~2mg, and force above 0)?

if(!file.exists("../RDS_Files/feed_Exc.fit.j.RDS")){
  
  fit_j <- stan(file = "feed_exc_j.stan",  
                data = daph_feex_listj, verbose = F, chains = 4)  
  saveRDS(fit_j, file = "../RDS_Files/feed_Exc.fit.j.RDS")
} else {
  fit_j <- readRDS("../RDS_Files/feed_Exc.fit.j.RDS")
}

## launch_shinystan(fit_j)
t_j <- rstan::extract(fit_j,permuted = FALSE)
fit_sum_fej <- summary(fit_j)
fit_sum_param_fej <- fit_sum_fej$summary[c(1:4),]

slope_feeding_j <- rbind(t_j[,1,1],t_j[,2,1], t_j[,3,1], t_j[,4,1]) ## all rows, all chains 


newdat_fej<- data.frame(Chl.1 = seq(1,35))

pred_feed_fej <- apply(newdat_fej,1,lin2,m=slope_feeding_j)
pred_feed_sum_fej <- apply(pred_feed_fej, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_fej <- data.frame(Chl.1 = seq(1,35), chl_diff_cc = pred_feed_sum_fej[1,])

upper_fej <- data.frame(Chl.1 = seq(1,35), chl_diff_cc = pred_feed_sum_fej[3,])
med_fej <- data.frame(Chl.1 = seq(1,35), chl_diff_cc = pred_feed_sum_fej[2,])

stan_fej_g <- ggplot(dat1, aes(Chl.1, chl_diff_cc)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_fej, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_fej, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_fej, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
  ylab("Chl (ug/L) per Daphnia*hr ") + ggtitle("A. Juvenile Feeding Fit with Excretion ")

#print(stan_fej_g)

## graph excretion
slope_exc_j <- rbind(t_j[,1,3],t_j[,2,3], t_j[,3,3], t_j[,4,3])

newdat_1 <- data.frame(Chl.1 = seq(1,50)
)

pred_exc_j <- apply(newdat_1,1,lin3, m=slope_exc_j, t=slope_feeding_j )
pred_exc_sum_j <- apply(pred_exc_j, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_wide_jx <- data.frame(Chl.1 = seq(1,50), nh4_diff_cc = pred_exc_sum_j[1,], chl_diff_cc = seq(0.01,.28,0.0055))
upper_wide_jx <- data.frame(Chl.1 = seq(1,50), nh4_diff_cc = pred_exc_sum_j[3,], chl_diff_cc =  seq(0.01,.28,0.0055))
med_wide_jx <- data.frame(Chl.1 = seq(1,50), nh4_diff_cc = pred_exc_sum_j[2,], chl_diff_cc =  seq(0.01,.28,0.0055))

stan_wideexj_g <- ggplot(dat1, aes(chl_diff_cc, nh4_diff_cc)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_wide_jx, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_wide_jx, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_wide_jx, linetype = "solid", lwd =1.25) + xlab("Change in Chlorophyll a (ug/L)/ Daphnia*hr") +
  ylab("Nh4 (mg/L) perDaphnia*hr ") + ggtitle("B. Juvenile Excretion Wide Priors")

#print(stan_wideexj_g)



