library(tidyverse)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(shinystan)
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



### so bad likely bc data for excretion is bad...

#cerio_fx_list <- 
 # list(
  #  N = nrow(dat),
  #  chl = dat$Chl.1,
  #  diff_chl = dat$chl_diff_ccdi,
 #   diff_nh4 = dat$nh4_diff_ccdi
#  )

#if(!file.exists("cerio.fx.RDS")){
  
 # fit <- stan(file = "feed_exc_wide.stan", 
  #              data = cerio_fx_list, verbose = F, chains = 4,
   #           control = list(max_treedepth = 15,adapt_delta = 0.9))  
  #saveRDS(fit, file = "cerio.fx.RDS")
#} else {
 # fit <- readRDS("cerio.fx.RDS")
#}


## can we fit just feeding
cerio_f_list <- 
  list(
    N = nrow(dat),
    chl = dat$Chl.1,
    daily_fec = dat$chl_diff_ccdi
  )

if(!file.exists("feed.cerio.RDS")){
  
  fit_wide <- stan(file = "fec_linear_wideprior.stan", 
                   data = cerio_f_list, verbose = F, chains = 4,
                   control = list(max_treedepth = 22,adapt_delta = 0.99),
                   iter = 5000)  
  saveRDS(fit_wide, file = "feed.cerio.RDS")
} else {
  fit_wide <- readRDS("feed.cerio.RDS")
}


#launch_shinystan(fit_wide)

t_wide <- rstan::extract(fit_wide,permuted = FALSE)
fit_sum_wide <- summary(fit_wide)
fit_sum_param_wide <- fit_sum_wide$summary[c(1:2),]

slope_pred_wide <- rbind(t_wide[,1,1],t_wide[,2,1], t_wide[,3,1], t_wide[,4,1]) ## all rows, all chains 


newdat_wide <- data.frame(Chl.1 = seq(0,25))

pred_out_wide <- apply(newdat_wide,1,lin2,m=slope_pred_wide)
pred_sum_wide <- apply(pred_out_wide, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_wide <- data.frame(Chl.1 = seq(0,25), chl_diff_ccdi = pred_sum_wide[1,])
upper_wide <- data.frame(Chl.1 = seq(0,25), chl_diff_ccdi = pred_sum_wide[3,])
med_wide <- data.frame(Chl.1 = seq(0,25), chl_diff_ccdi = pred_sum_wide[2,])

stan_wide_g <- ggplot(dat, aes(Chl.1, chl_diff_ccdi)) + geom_point(alpha = 0.6, size = 2, aes(color= as.factor(Treatment)) ) +
  geom_line(data = lower_wide, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_wide, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_wide, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
  ylab(" ") + ggtitle("Stan: Wide Priors")

print(stan_wide_g)
## its hard to look at this graph and not assume something went wrong with one treatment... so fit without that treatment....


dat1 <- dat %>% filter(Treatment != 2)


cerio_f2_list <- 
  list(
    N = nrow(dat1),
    chl = dat1$Chl.1,
    daily_fec = dat1$chl_diff_ccdi
  )

if(!file.exists("feed.cerio2.RDS")){
  
  fit_wide <- stan(file = "fec_linear_wideprior.stan", 
                   data = cerio_f2_list, verbose = F, chains = 4,
                   control = list(max_treedepth = 22,adapt_delta = 0.99),
                   iter = 5000)    
  saveRDS(fit_wide, file = "feed.cerio2.RDS")
} else {
  fit_wide <- readRDS("feed.cerio2.RDS")
}


