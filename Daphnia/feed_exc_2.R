## goal to fit feeding uptake parameter and ammonium excretion parameter at the same time 

source("../transfer_functions.R")
source("../chl_adj.R")
source("../Graphing_Set_Up.R")
library(tidyverse)
library(gridExtra)
library(nlmrt)
library(fitdistrplus)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(shinystan)

rdat <- read.csv("Daphnia_large_Feeding_Nov11.csv")
dim(rdat)
## get average change in controls by treatment


cont <- rdat %>% 
  mutate(Chl_Time_Diff_day = (Chl_Time_Diff/60)/24,Nh4_Time_Diff_day = (Nh4_Time_Diff/60)/24) %>%
  filter(control == 2) %>% 
  mutate(chl_diff =(chl1-chl2)/Chl_Time_Diff_day, nh4_diff= (nh42-nh41)/Nh4_Time_Diff_day) %>% # subtract 1 from 2 for change over time
  group_by(Treatment) %>%
  summarize(mean_chl = mean(chl_diff, na.rm = T), mean_nh4 = mean(nh4_diff,na.rm = T)) ## onr row in treatment 3 is all NAs..??

dim(cont)
## do per hour instead of per day to match literature
cont <- rdat %>% 
  mutate(Chl_Time_Diff_day = (Chl_Time_Diff/60),Nh4_Time_Diff_day = (Nh4_Time_Diff/60)) %>%
  filter(control == 2) %>% 
  mutate(chl_diff =(chl1-chl2)/Chl_Time_Diff_day, nh4_diff= (nh42-nh41)/Nh4_Time_Diff_day) %>% # subtract 1 from 2 for change over time
  group_by(Treatment) %>%
  summarize(mean_chl = mean(chl_diff, na.rm = T), mean_nh4 = mean(nh4_diff,na.rm = T))



## add mean control avg back to main dataframe
dat <- left_join(rdat,cont)
dim(dat)
## account for controls


dim(dat)


dat <- dat %>%
  filter(control == 1) %>%
  mutate(Chl_Time_Diff_day = (Chl_Time_Diff/60),Nh4_Time_Diff_day = (Nh4_Time_Diff/60)) %>%
  mutate(chl_diff = (chl1-chl2)/Chl_Time_Diff_day, nh4_diff = (nh41-nh42)/Nh4_Time_Diff_day) %>%
  mutate(chl_diff_cc = (chl_diff-mean_chl)/Num_Daphnia, nh4_diff_cc = (nh4_diff-mean_nh4)/Num_Daphnia)


dat1 <- dat %>% filter(control == 1) %>% filter(!is.na(chl_diff_cc)) %>% ## 1 entry is NA
  dplyr::select(Rep, Treatment,chl1,nh41, chl_diff_cc,nh4_diff_cc, Num_Daphnia)



daph_feex_list <- 
  list(
    N = nrow(dat1),
    chl = dat1$chl1,
    diff_chl = dat1$chl_diff_cc,
    diff_nh4 = dat1$nh4_diff_cc
    )

if(!file.exists("RDS_Files/feed_Exc.fit.wide.RDS")){
  
  fit_1 <- stan(file = "feed_exc_wide.stan", 
                data = daph_feex_list, verbose = F, chains = 4)  
  saveRDS(fit_1, file = "RDS_Files/feed_Exc.fit.wide.RDS")
} else {
  fit_1 <- readRDS("RDS_Files/feed_Exc.fit.wide.RDS")
}



 
launch_shinystan(fit_1)

t_wide <- rstan::extract(fit_1,permuted = FALSE)
fit_sum_wide <- summary(fit_1)
fit_sum_param_wide <- fit_sum_wide$summary[c(1:4),]

slope_feeding_wide <- rbind(t_wide[,1,1],t_wide[,2,1], t_wide[,3,1], t_wide[,4,1]) ## all rows, all chains 


newdat_wide <- data.frame(chl = seq(1,100))

pred_feed_wide <- apply(newdat_wide,1,lin2,m=slope_feeding_wide)
pred_feed_sum_wide <- apply(pred_feed_wide, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_wide <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_feed_sum_wide[1,])

upper_wide <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_feed_sum_wide[3,])
med_wide <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_feed_sum_wide[2,])

stan_wide_g <- ggplot(dat1, aes(chl1, chl_diff_cc)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_wide, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_wide, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_wide, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
  ylab("Chl (ug/L) per Daphnia*hr ") + ggtitle("Stan: Fit with Excretion Wide Priors")

print(stan_wide_g)

## graph excretion
slope_exc_wide <- rbind(t_wide[,1,3],t_wide[,2,3], t_wide[,3,3], t_wide[,4,3])

newdat_1 <- data.frame(chl1 = seq(0,70),
    )

pred_exc_wide <- apply(newdat_wide,1,lin3, m=slope_exc_wide, t=slope_feeding_wide )
pred_exc_wide <- apply(pred_exc_wide, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_wide_x <- data.frame(chl1 = seq(1,100), nh4_diff_cc = pred_exc_wide[1,], chl_diff_cc = seq(0.01,1,0.01))
upper_wide_x <- data.frame(chl1 = seq(1,100), nh4_diff_cc = pred_exc_wide[3,], chl_diff_cc =  seq(0.01,1,0.01))
med_wide_x <- data.frame(chl1 = seq(1,100), nh4_diff_cc = pred_exc_wide[2,], chl_diff_cc =  seq(0.01,1,0.01))

stan_wideex_g <- ggplot(dat1, aes(chl_diff_cc, nh4_diff_cc)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_wide_x, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_wide_x, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_wide_x, linetype = "solid", lwd =1.25) + xlab("Change in Chlorophyll a (ug/L)/ Daphnia*hr") +
  ylab("Nh4 (mg/L) perDaphnia*hr ") + ggtitle("Stan: Fit with Excretion Wide Priors")

print(stan_wideex_g)



stan_wideex_g2 <- ggplot(dat1, aes(chl1, nh4_diff_cc)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_wide_x, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_wide_x, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_wide_x, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
  ylab("Nh4 (mg/L) perDaphnia*hr ") + ggtitle("Stan: Fit with Excretion Wide Priors")

print(stan_wideex_g2)


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


list_mixed <- list(
  N = as.numeric(nrow(dat1)),
  L = as.numeric(nrow(feed_lit2)),
  chl = dat1$chl1,
  diff = dat1$chl_diff_cc,
  lit_chl = as.numeric(feed_lit2$chl),
  diff_lit = as.numeric(feed_lit2$diff),
  sd_lit = (feed_lit2$sd_feed),
  title = feed_lit2$Title,
  M = as.numeric(length(unique(feed_lit2$Title))),
  diff_nh4 = dat1$nh4_diff_cc
  )
  
  
fit_mixed <- stan(file = "adult_feeding_exc_mix.stan", 
  data = list_mixed, chains = 4, control = list(max_treedepth =22, adapt_delta = 0.99))


  
    if(!file.exists("RDS_Files/feed_exc.fit.mix.RDS")){
    
while (test_div > 0) {     
      
    fit_mixed <- stan(file = "adult_feeding_exc_mix.stan", init = list(
      list(sigma = 0.10 + rnorm(1, 0, 0.025)
           , sigma_slope = 0.23 + rnorm(1, 0, 0.05)
           , slope_bar = 0.30 + rnorm(1, 0, 0.05)), 
      list(sigma = 0.10 + rnorm(1, 0, 0.025)
           , sigma_slope = 0.23 + rnorm(1, 0, 0.05)
           , slope_bar = 0.30 + rnorm(1, 0, 0.05)),
      list(sigma = 0.10 + rnorm(1, 0, 0.025)
           , sigma_slope = 0.23 + rnorm(1, 0, 0.05)
           , slope_bar = 0.30 + rnorm(1, 0, 0.05)),
      list(sigma = 0.10 + rnorm(1, 0, 0.025)
           , sigma_slope = 0.23 + rnorm(1, 0, 0.05)
           , slope_bar = 0.30 + rnorm(1, 0, 0.05))),
      data = list_mixed, chains = 4, control = list(adapt_delta = 0.99,
                                                    max_treedepth = 22))
      saveRDS(fit_mixed, file = "RDS_Files/feed_exc.fit.mix.RDS")
      
test_div <- sum(attr(fit_mixed@sim$samples[[4]], "sampler_params")$divergent__[1001:2000]) +
  sum(attr(fit_mixed@sim$samples[[3]], "sampler_params")$divergent__[1001:2000]) +
  sum(attr(fit_mixed@sim$samples[[2]], "sampler_params")$divergent__[1001:2000]) +
  sum(attr(fit_mixed@sim$samples[[1]], "sampler_params")$divergent__[1001:2000])

}
      
   } else {
    fit_mixed <- readRDS("RDS_Files/feed_exc.fit.mix.RDS")
  } 
  
  launch_shinystan(fit_mixed)  


### use all feeding lit and impute sd, but don't allow slope to vary
  # model with both lit and my data with imputation (no varying slope for literature)
  list_imp2 <- list(
    N = as.numeric(nrow(dat1)),
    L = as.numeric(nrow(feed_lit1)),
    M = as.numeric(length(unique(feed_lit2$Title))),
    chl = dat1$chl1,
    diff = dat1$chl_diff_cc,
    lit_chl = as.numeric(feed_lit1$chl),
    diff_lit = as.numeric(feed_lit1$diff),
    sd_lit = feed_lit1$sd_feed,
    sd_index = index_sd,
    miss = missing_n,
    diff_nh4 = dat1$nh4_diff_cc
  )
  
  
  
  if(!file.exists( "RDS_Files/feex.fit.mix.imp.RDS")){
    
    
    fit_mix_imp <- stan(file = "feed.mix.imp.stan", 
                    data = list_imp2)
    
    saveRDS(fit_mix_imp, file = "RDS_Files/feex.fit.mix.imp.RDS")
  } else {
    fit_mix_imp  <- readRDS( "RDS_Files/feex.fit.mix.imp.RDS")
  }
  
  
  
  launch_shinystan(fit_mix_imp)
  
  
  
  fit_sum_mimp <- summary(fit_mix_imp)
  (fit_sum_param_mimp <- fit_sum_mimp$summary[c(1:3,56,57 ),])
  t_mimp <- rstan::extract(fit_mix_imp,permuted = FALSE)
  fr_pred_imp <- rbind(t_mimp[,1,1],t_mimp[,2,1],t_mimp[,3,1],t_mimp[,4,1]) 
  er_pred_imp <- rbind(t_mimp[,1,56],t_mimp[,2,56],t_mimp[,3,56],t_mimp[,4,56]) 
  
  
  
  pred_out_mimp <- apply(newdat_wide,1,lin2,m= fr_pred_imp)
  
  pred_sum_mimp <- apply(pred_out_mimp, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))
  
  lower_mimp <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_sum_mimp[1,])
  upper_mimp <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_sum_mimp[3,])
  med_mimp <- data.frame(chl1 = seq(1,100), chl_diff_cc= pred_sum_mimp[2,])
  
  stan_mix_imp <- ggplot(dat1, aes(chl1, chl_diff_cc)) + geom_point(alpha = 0.6, size = 2 ) +
    geom_point(data = feed_lit1, aes(chl,diff), color = "blue") +
    geom_line(data = lower_mimp, linetype = "dotdash", lwd = 1.25) +
    geom_line(data = upper_mimp, linetype = "dotdash", lwd = 1.25)+
    geom_line(data = med_mimp, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
    ylab(" ") + ggtitle("Stan: Imputation Mixed With Exc")
  
  print(stan_mix_imp)
  

  pred_exc_mimp <- apply(newdat_wide,1,lin3, m=er_pred_imp, t=fr_pred_imp )
  pred_exc_mimp <- apply(pred_exc_mimp, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))
  
  lower_mimp_x <- data.frame(chl1 = seq(1,100), nh4_diff_cc = pred_exc_mimp[1,], chl_diff_cc = seq(0.01,1,0.01))
  upper_mimp_x <- data.frame(chl1 = seq(1,100), nh4_diff_cc = pred_exc_mimp[3,], chl_diff_cc =  seq(0.01,1,0.01))
  med_mimp_x <- data.frame(chl1 = seq(1,100), nh4_diff_cc = pred_exc_mimp[2,], chl_diff_cc =  seq(0.01,1,0.01))
  
  stan_mimpex_g <- ggplot(dat1, aes(chl_diff_cc, nh4_diff_cc)) + geom_point(alpha = 0.6, size = 2 ) +
    geom_line(data = lower_mimp_x, linetype = "dotdash", lwd = 1.25) +
    geom_line(data = upper_mimp_x, linetype = "dotdash", lwd = 1.25)+
    geom_line(data = med_mimp_x, linetype = "solid", lwd =1.25) + xlab("Change in Chlorophyll a (ug/L)/ Daphnia*hr") +
    ylab("Nh4 (mg/L) perDaphnia*hr ") + ggtitle("Stan: Fit with Excretion Wide Priors")
  
  print(stan_mimpex_g) 
    
  
  
  

### add in excretion data....maybe as "hyperparam"? difficult because not reported with uptake or food....






