source("../transfer_functions.R")
source("../chl_adj.R")
source("../Graphing_Set_Up.R")
library(tidyverse)
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
## add mean control avg back to main dataframe
dat <- left_join(rdat,cont)
dim(dat)
## account for controls

dat <- dat %>%
  filter(control == 1) %>%
  mutate(Chl_Time_Diff_day = (Chl_Time_Diff/60)/24,Nh4_Time_Diff_day = (Nh4_Time_Diff/60)/24) %>%
  mutate(chl_diff = (chl1-chl2)/Chl_Time_Diff_day, nh4_diff = (nh41-nh42)/Nh4_Time_Diff_day) %>%
  mutate(chl_diff_cc = (chl_diff-mean_chl)/Num_Daphnia, nh4_diff_cc = (nh4_diff-mean_nh4)/Num_Daphnia)
dim(dat)

dat1 <- dat %>% filter(control == 1) %>% filter(!is.na(chl_diff_cc)) %>% ## 1 entry is NA
  dplyr::select(Rep, Treatment,chl1,nh41, chl_diff_cc,nh4_diff_cc, Num_Daphnia)
dim(dat1)


##mod_sig <- nlxb(data=dat1, chl_diff_cc ~ (a*chl1^b)/(c+chl1^b), start = list(a=1,b=0.1,c=1))
mod_sat <- nlxb(data = dat1, chl_diff_cc ~ (chl1*a)/(chl1+b), start = list(a=1,b=1))
mod_lm <- lm(data = dat1, chl_diff_cc ~ chl1)
mod_lm2 <- lm(data = dat1, chl_diff_cc ~ chl1-1)
#newpred <- sig_fun(k= seq(1,100,1), a = 0.154319 , b = 0.861941, c = 83.8371)
#plot(seq(1,100,1), newpred)
#points(dat1$chl1,dat1$chl_diff_cc)

newdata = data.frame(chl1 = seq(1,100,1))

mod_obj <- summary(mod_sat)
sat_pred <- sat_fun(k= seq(1,100,1), a=mod_obj$coeff[1], b=mod_obj$coeff[2])
sat_pred_lwr <- sat_fun(k= seq(1,100,1), a=mod_obj$coeff[1]-mod_obj$SEs[1], b=mod_obj$coeff[2]-mod_obj$SEs[2]) 
sat_pred_upr <- sat_fun(k= seq(1,100,1), a=mod_obj$coeff[1]+mod_obj$SEs[1], b=mod_obj$coeff[2]+mod_obj$SEs[2])
newdat1 = data.frame(chl1 = newdata$chl1,
                     chl_diff_cc = sat_pred,
                     upr = sat_pred_upr,
                     lwr = sat_pred_lwr)



feed_nls_sat <- ggplot(data = dat1, aes(chl1, chl_diff_cc)) + geom_point() +
  geom_line(data = newdat1) + 
  geom_ribbon(data = newdat1, aes(ymin = lwr, ymax= upr)) +
  xlab("Chlorphyll a (ug/L)") + 
  ylab("Change in Chlorophyll a/Daphnia/Day") +
  ggtitle("NLS:Saturating Curve")

## straight line
newpred1 <- data.frame(predict(mod_lm, newdata = newdata, interval="confidence" ))
newpred1$chl_diff_cc <- newpred1$fit
newpred1$chl1 <- seq(1,100)
feed_ls_g <- ggplot(data = dat1, aes(chl1, chl_diff_cc)) + geom_point() +
  geom_line(data = newpred1) +
  geom_ribbon(data = newpred1, aes(ymin=lwr, ymax=upr),alpha = 0.3)+
  xlab("Chlorphyll a (ug/L") + 
  ylab("Change in Chlorophyll a/Daphnia/Day") +
  ggtitle("LS")


## linear line exactly the same as saturating, looks like good fit, range of chl1 close to final experiment -- get rid of intercept still good
## so keep linear ie type I

## fit linear in Stan with wide priors


daph_feed_list <- 
  list(
    N = nrow(dat1),
    chl = dat1$chl1,
    daily_fec = dat1$chl_diff_cc ## using lm from fec- exactly the same just label is weird
    
  )

if(!file.exists("RDS_Files/feed.fit.wide.RDS")){
  
  fit_wide <- stan(file = "fec_linear_wideprior.stan", 
                   data = daph_feed_list, verbose = F, chains = 4) 
  saveRDS(fit_wide, file = "RDS_Files/feed.fit.wide.RDS")
} else {
  fit_wide <- readRDS("RDS_Files/feed.fit.wide.RDS")
}


launch_shinystan(fit_wide)


t_wide <- rstan::extract(fit_wide,permuted = FALSE)
fit_sum_wide <- summary(fit_wide)
fit_sum_param_wide <- fit_sum_wide$summary[c(1:2),]

slope_pred_wide <- rbind(t_wide[,1,1],t_wide[,2,1], t_wide[,3,1], t_wide[,4,1]) ## all rows, all chains 


newdat_wide <- data.frame(chl = seq(1,100))

pred_out_wide <- apply(newdat_wide,1,lin2,m=slope_pred_wide)
pred_sum_wide <- apply(pred_out_wide, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_wide <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_sum_wide[1,])
upper_wide <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_sum_wide[3,])
med_wide <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_sum_wide[2,])

stan_wide_g <- ggplot(dat1, aes(chl1, chl_diff_cc)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_wide, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_wide, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_wide, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
  ylab("Chl a change/ Daphnia*Day") + ggtitle("Stan: Wide Priors")

## fit in stan with literature
### data from literature

feed_lit <- read.csv("feeding.csv")
feed_lit <- feed_lit %>% filter(!is.na(point_est_cell_indiv_day)) %>% 
  filter(!is.na(algal_conc_cellperml)) %>% 
  dplyr::select(Title, replicates,point_est_cell_indiv_day,point_error,algal_conc_cellperml,sd)

index_sd <- with(feed_lit, which(is.na(sd))) ## index of which rows have missing SD 
missing_n <- length(index_sd)

##copy dataframe
feed_lit1 <- feed_lit

## convert cells to chl in lit
feed_lit1$chl <- cell_adj(feed_lit1$algal_conc_cellperml)
feed_lit1$sd_feed <- cell_adj(feed_lit1$sd)
feed_lit1$diff <- cell_adj(feed_lit1$point_est_cell_indiv_day)

## replace NAs with dummy
feed_lit1[is.na(feed_lit1)] <- 100

## convert chl to cells in my data
dat1$cells <- chl_adj(dat1$chl1)
dat1$cell_diff <-chl_adj(dat1$chl_diff_cc) 



#######  fit in stan


daph_grow_list <- list(
  N = as.numeric(nrow(dat1)),
  L = as.numeric(nrow(feed_lit)),
  miss = missing_n,
  chl = dat1$cells,
  diff = dat1$cell_diff,
  lit_chl = as.numeric(feed_lit$algal_conc_cellperml),
  diff_lit = as.numeric(feed_lit$point_est_cell_indiv_day),
  sd_lit = log(feed_lit1$sd),
  sd_index = index_sd 
)


daph_imp_list <- list(
  L = as.numeric(nrow(feed_lit1)),
  miss = missing_n,
  lit_chl = as.numeric(feed_lit1$chl),
  diff_lit = as.numeric(feed_lit1$diff),
  sd_lit = feed_lit1$sd_feed,
  sd_index = index_sd 
)


daph_imp_list1 <- list(
  L = as.numeric(nrow(feed_lit1)),
  miss = missing_n,
  lit_chl = as.numeric(feed_lit1$algal_conc_cellperml),
  diff_lit = as.numeric(feed_lit1$point_est_cell_indiv_day),
  sd_lit = feed_lit1$sd,
  sd_index = index_sd 
)

##### this doesn't work
fit_mixed <- stan(file = "adult_feeding.stan", 
                  data = daph_grow_list, verbose = F, chains = 1, iter = 5000,
                  control = list(adapt_delta = 0.99,
                                 max_treedepth = 15))

launch_shinystan(fit_mixed)

fit_sum_mixed <- summary(fit_mixed)
(fit_sum_param_mixed <- fit_sum_mixed$summary[c(1:4),])
t_mix <- rstan::extract(fit_mixed,permuted = FALSE)
m_pred_mix <- rbind(t_mix[,1,1],t_mix[,2,1],t_mix[,3,1],t_mix[,4,1]) 


newdat_mix <- data.frame(chl1 = seq(0,100))

pred_out_mix <- apply(newdat_mix,1,lin2,m= m_pred_mix)
pred_sum_mix <- apply(pred_out_mix, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_mix <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_sum_mix[1,])
upper_mix <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_sum_mix[3,])
med_mix <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_sum_mix[2,])

stan_mix_g <- ggplot(dat1, aes(chl1, chl_diff_cc)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_mix, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_mix, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_mix, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
  ylab("Chl a change/ Daphnia*Day") + ggtitle("Stan: Mixed model")




## literature only
## some neff samples too low-- i think over estimating things-- not exactly sure if imputation is actually working
library(fitdistrplus)
sd <- feed_lit1 %>% dplyr::select(sd_feed) %>% filter(sd_feed > 100)

fitdist(sd$sd, "lnorm")

fit_lit <- stan(file = "lit_imputation.stan",  
                data = daph_imp_list, verbose = F, chains = 4, iter = 5000,thin = 2,
                control = list(adapt_delta = 0.99, max_treedepth=13))

launch_shinystan(fit_lit)

fit_sum_lit <- summary(fit_lit)
(fit_sum_param_lit <- fit_sum_lit$summary[c(1:4),])
t_lit <- rstan::extract(fit_lit,permuted = FALSE)
m_pred_lit <- rbind(t_lit[,1,1],t_lit[,2,1],t_lit[,3,1],t_lit[,4,1]) 


newdat_lit <- data.frame(chl1 = seq(0,100))

pred_out_lit <- apply(newdat_lit,1,lin2,m= m_pred_lit)
pred_sum_lit <- apply(pred_out_lit, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_lit <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_sum_lit[1,])
upper_lit <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_sum_lit[3,])
med_lit <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_sum_lit[2,])

lit_g <- ggplot(feed_lit1, aes(chl1, chl_diff_cc)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_lit, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_lit, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_lit, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
  ylab("Chl a change/ Daphnia*Day") + ggtitle("Stan: Literature Only")





feed_lit$cells <- feed_lit$algal_conc_cellperml
feed_lit$cell_diff <- feed_lit$point_est_cell_indiv_day

ggplot(data = dat1, aes(cells,cell_diff)) + geom_point() +
  geom_point(data = feed_lit, color = "blue", shape=4) + scale_x_log10()



## hyperparameter stan
feed_lit[is.na(feed_lit$replicates)] <- 1
feed_lit<-droplevels(feed_lit)
d <- fitdist(feed_lit$point_est_cell_indiv_day, "lnorm", weights = as.numeric(as.character(feed_lit$replicates)))
d1 <- fitdist(feed_lit$point_est_cell_indiv_day, "norm")
## check the weights warning...are the weights doing anything??
set.seed(100)
h <- hist(feed_lit$point_est_cell_indiv_day) 
xfit <- seq(0,1000000000,500)
yfit <- rlnorm(xfit,(d$estimate[1]), (d$estimate[2]) )
lines(xfit,yfit, col="blue")

yfit <- rnorm(xfit, d1$estimate[1], d1$estimate[2])
ggplot(data = feed_lit, aes(x=point_est_cell_indiv_day)) + geom_histogram(aes(y=..density..),bins = 10)+ 
  geom_density(data = data.frame(point_est_cell_indiv_day=yfit))

###hmmmmmmmmmmmmmm this is confusing... why doesnt this work?


## parameter just from literature values
## two real outlier for food concentration- drop
feed_lit1 <- feed_lit %>% filter(algal_conc_cellperml < 1000000)
feed_lit1$replicates <- as.numeric(as.character(feed_lit1$replicates))
lit_feed_mod <- lm(point_est_cell_indiv_day ~ -1+algal_conc_cellperml, data = feed_lit1, weights = 1/replicates)
newdat <- data.frame(
  algal_conc_cellperml = seq(min(feed_lit1$algal_conc_cellperml), max(feed_lit1$algal_conc_cellperml), length =1000),
  point_est_cell_indiv_day =0,
  upr =0,
  lwr = 0
)
pred <- as.data.frame(predict(lit_feed_mod, newdata = newdat, interval = "confidence"))

newdat$point_est_cell_indiv_day <- pred$fit
newdat$upr <- pred$upr
newdat$lwr <- pred$lwr

(lit_g <- ggplot(feed_lit1, aes(algal_conc_cellperml, point_est_cell_indiv_day)) + geom_point()+
    geom_line(data = newdat) + geom_ribbon(data = newdat, aes(ymin=lwr,ymax=upr), alpha = 0.3))



## models: ls, wideprior, mixed, lit only, hyper param?