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

newdat_1 <- data.frame(chl1 = seq(0,70)
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

t_mixed <- rstan::extract(fit_mixed,permuted = FALSE)
fit_sum_mixed <- summary(fit_mixed)
fit_sum_param_mixed <- fit_sum_mixed$summary[c(1:3,8,9),]

slope_feeding_mixed <- rbind(t_mixed[,1,1],t_mixed[,2,1], t_mixed[,3,1], t_mixed[,4,1]) ## all rows, all chains 


pred_feed_mixed <- apply(newdat_wide,1,lin2,m=slope_feeding_mixed)
pred_feed_sum_mixed <- apply(pred_feed_mixed, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_mixed <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_feed_sum_mixed[1,])

upper_mixed <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_feed_sum_mixed[3,])
med_mixed <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_feed_sum_mixed[2,])

stan_mixed_g <- ggplot(dat1, aes(chl1, chl_diff_cc)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_mixed, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_mixed, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_mixed, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
  ylab("Chl (ug/L) per Daphnia*hr ") + ggtitle("Stan: Fit with Excretion With Lit vary Slope")

print(stan_mixed_g)

## graph excretion
slope_exc_mixed <- rbind(t_mixed[,1,4],t_mixed[,2,4], t_mixed[,3,4], t_mixed[,4,4])

newdat_m1 <- data.frame(chl1 = seq(0,70)
)

pred_exc_mix <- apply(newdat_wide,1,lin3, m=slope_exc_mixed, t=slope_feeding_mixed )
pred_exc_mix <- apply(pred_exc_mix, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_mix_x <- data.frame(chl1 = seq(1,100), nh4_diff_cc = pred_exc_wide[1,], chl_diff_cc = seq(0.01,1,0.01))
upper_mix_x <- data.frame(chl1 = seq(1,100), nh4_diff_cc = pred_exc_wide[3,], chl_diff_cc =  seq(0.01,1,0.01))
med_mix_x <- data.frame(chl1 = seq(1,100), nh4_diff_cc = pred_exc_wide[2,], chl_diff_cc =  seq(0.01,1,0.01))

stan_mixex_g <- ggplot(dat1, aes(chl_diff_cc, nh4_diff_cc)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_mix_x, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_mix_x, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_mix_x, linetype = "solid", lwd =1.25) + xlab("Change in Chlorophyll a (ug/L)/ Daphnia*hr") +
  ylab("Nh4 (mg/L) perDaphnia*hr ") + ggtitle("Stan: Fit with Excretion Wide Priors")

print(stan_mixex_g)



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
    ylab("Nh4 (mg/L) per Daphnia*hr ") + ggtitle("Stan: Excretion Imp Priors")
  
print(stan_mimpex_g) 
    
  
if(!file.exists("RDS_Files/feed.fit.lit.imp.RDS")){
    
    fit_lit <- stan(file = "bmb_imp.stan",
                    data = daph_imp_list, verbose = F, chains = 4, iter = 5000,thin = 2,
                    control = list(adapt_delta = 0.99, max_treedepth=13))
    saveRDS(fit_lit, file = "RDS_Files/feed.fit.lit.imp.RDS")
  } else {
    fit_lit <- readRDS("RDS_Files/feed.fit.lit.imp.RDS")
  }
  
  
launch_shinystan(fit_lit)
  
  
fit_sum_lit <- summary(fit_lit)
(fit_sum_param_lit <- fit_sum_lit$summary[c(1:4),])
t_lit <- rstan::extract(fit_lit,permuted = FALSE)
m_pred_lit <- rbind(t_lit[,1,1],t_lit[,2,1],t_lit[,3,1],t_lit[,4,1]) 
  
  
newdat_lit <- data.frame(chl = seq(0,1.3, by = 0.002))
  
pred_out_lit <- apply(newdat_lit,1,lin2,m= m_pred_lit)
pred_sum_lit <- apply(pred_out_lit, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))
  
lower_lit <- data.frame(chl = seq(0,1.3, by = 0.002), diff = pred_sum_lit[1,])
upper_lit <- data.frame(chl = seq(0,1.3, by = 0.002), diff = pred_sum_lit[3,])
med_lit <- data.frame(chl = seq(0,1.3, by = 0.002), diff = pred_sum_lit[2,])
  
lit_g <- ggplot(feed_lit1, aes(chl, diff)) + geom_point(alpha = 0.6, size = 2 ) +
    geom_line(data = lower_lit, linetype = "dotdash", lwd = 1.25) +
    geom_line(data = upper_lit, linetype = "dotdash", lwd = 1.25)+
    geom_line(data = med_lit, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
    ylab(" ") + ggtitle("Stan: Literature Only- Imputed")
  
  
  
print(lit_g)
  
  
## literature only varying slope no imputation-- so uses many fewer studies (only studies with reported sds)
  
  
lit_list_sd <- list(
    L = as.numeric(nrow(feed_lit2)),
    lit_chl = as.numeric(feed_lit2$chl),
    diff_lit = as.numeric(feed_lit2$diff),
    sd_lit = feed_lit2$sd_feed,
    M = as.numeric(length(unique(feed_lit2$Title)))
    
  )
  
  
  
if(!file.exists("RDS_Files/feed.fit.lit.vs.RDS")){
    
    lit_vslope <- stan(file = "lit_varyslope.stan", data = lit_list_sd,
                       control = list(adapt_delta = 0.9))
    
    saveRDS(lit_vslope, file = "RDS_Files/feed.fit.lit.vs.RDS")
  } else {
    lit_vslope <- readRDS("RDS_Files/feed.fit.lit.vs.RDS")
  }  
  
  
  
  
  
launch_shinystan(lit_vslope)
  
  
  
fit_sum_lit_s <- summary(lit_vslope)
(fit_sum_param_lit_s <- fit_sum_lit_s$summary[c(1:2),])
t_lit_s <- rstan::extract(lit_vslope,permuted = FALSE)
m_pred_lit_s <- rbind(t_lit_s[,1,1],t_lit_s[,2,1],t_lit_s[,3,1],t_lit_s[,4,1]) 
  
  
newdat_lit_s <- data.frame(chl = seq(0,1.3, by = 0.002))
  
pred_out_lit_s <- apply(newdat_lit_s,1,lin2,m= m_pred_lit_s)
pred_sum_lit_s <- apply(pred_out_lit_s, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))
  
lower_lit_s <- data.frame(chl = seq(0,1.3, by = 0.002), diff = pred_sum_lit_s[1,])
upper_lit_s <- data.frame(chl = seq(0,1.3, by = 0.002), diff = pred_sum_lit_s[3,])
med_lit_s <- data.frame(chl = seq(0,1.3, by = 0.002), diff = pred_sum_lit_s[2,])
  
lit_g_s <- ggplot(feed_lit2, aes(chl, diff)) + geom_point(alpha = 0.6, size = 2 ) +
    geom_line(data = lower_lit_s, linetype = "dotdash", lwd = 1.25) +
    geom_line(data = upper_lit_s, linetype = "dotdash", lwd = 1.25)+
    geom_line(data = med_lit_s, linetype = "solid", lwd =1.25) + xlab("Chl a (ug/L)") +
    ylab("Chl a change/ Daphnia*hour") + ggtitle("Stan: Literature Only- Vary Slopes")
  
print(lit_g_s)    
  
  

## feeding graphs
frg <- grid.arrange(stan_wide_g, stan_mixed_g, stan_mix_imp, lit_g, lit_g_s)

## excretion graph(s?) 
stan_wideex_g


## extract parameters  
feed_exc_est <- list(
  "mixed_model_vs" = fit_sum_mixed,
  "wide_prior"=fit_sum_wide,
  "lit_imp" = fit_sum_lit,
  "lit_vs" = fit_sum_lit_s,
  "mixed_imp" = fit_sum_mimp
)

  
### add in excretion data....maybe as "hyperparam"? difficult because not reported with uptake or food....






