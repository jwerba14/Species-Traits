## adult daphnia feeding and excretion

source("../transfer_functions.R")
source("../chl_adj.R")
source("../Graphing_Set_Up.R")
library(tidyverse)
library(nlmrt)

rdat <- read.csv("Daphnia_large_Feeding_Nov11.csv")

## get average change in controls by treatment


cont <- rdat %>% 
  mutate(Chl_Time_Diff_day = (Chl_Time_Diff/60)/24,Nh4_Time_Diff_day = (Nh4_Time_Diff/60)/24) %>%
  filter(control == 2) %>% 
  mutate(chl_diff =(chl1-chl2)/Chl_Time_Diff_day, nh4_diff= (nh42-nh41)/Nh4_Time_Diff_day) %>% # subtract 1 from 2 for change over time
  group_by(Treatment) %>%
  summarize(mean_chl = mean(chl_diff, na.rm = T), mean_nh4 = mean(nh4_diff,na.rm = T)) ## onr row in treatment 3 is all NAs..??


## add mean control avg back to main dataframe
dat <- left_join(rdat,cont)

## account for controls

dat <- dat %>%
  filter(control == 1) %>%
  mutate(Chl_Time_Diff_day = (Chl_Time_Diff/60)/24,Nh4_Time_Diff_day = (Nh4_Time_Diff/60)/24) %>%
  mutate(chl_diff = (chl1-chl2)/Chl_Time_Diff_day, nh4_diff = (nh41-nh42)/Nh4_Time_Diff_day) %>%
  mutate(chl_diff_cc = (chl_diff-mean_chl)/Num_Daphnia, nh4_diff_cc = (nh4_diff-mean_nh4)/Num_Daphnia)

dat1 <- dat %>% filter(control == 1) %>% filter(!is.na(chl_diff_cc)) %>% ## 1 entry is NA
  dplyr::select(Rep, Treatment,chl1,nh41, chl_diff_cc,nh4_diff_cc, Num_Daphnia)

ggplot(dat1, aes(chl1,chl_diff_cc)) + geom_point()



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



ggplot(data = dat1, aes(chl1, chl_diff_cc)) + geom_point() +
  geom_line(data = newdat1) + 
  geom_ribbon(data = newdat1, aes(ymin = lwr, ymax= upr)) +
  xlab("Chlorphyll a (ug/L)") + 
  ylab("Change in Chlorophyll a/Daphnia/Day") +
  ggtitle("NLS:Saturating Curve")

## straight line
newpred1 <- data.frame(predict(mod_lm, newdata = newdata, interval="confidence" ))
newpred1$chl_diff_cc <- newpred1$fit
newpred1$chl1 <- seq(1,100)
ggplot(data = dat1, aes(chl1, chl_diff_cc)) + geom_point() +
  geom_line(data = newpred1) +
  geom_ribbon(data = newpred1, aes(ymin=lwr, ymax=upr),alpha = 0.3)+
  xlab("Chlorphyll a (ug/L") + 
  ylab("Change in Chlorophyll a/Daphnia/Day") +
  ggtitle("LS")


## linear line exactly the same as saturating, looks like good fit, range of chl1 close to final experiment -- get rid of intercept still good
## so keep linear ie type I

## fit linear in Stan with wide priors
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(shinystan)

daph_feed_list <- 
  list(
    N = nrow(dat1),
    chl = dat1$chl1,
    daily_fec = dat1$chl_diff_cc ## using lm from fec- exactly the same just label is weird
    
  )

fit1 <- stan(file = "fec_linear_wideprior.stan", 
            data = daph_feed_list, verbose = F, chains = 4) 
launch_shinystan(fit)


t <- rstan::extract(fit,permuted = FALSE)
fit_sum <- summary(fit)
print(names(fit_sum))
print(fit_sum$summary)
fit_sum_param <- fit_sum$summary[c(1:2),]

slope_pred <- rbind(t[,1,1],t[,2,1], t[,3,1], t[,4,1]) ## all rows, all chains 


newdat <- data.frame(chl = seq(1,100))

pred_out <- apply(newdat,1,lin2,m=slope_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_sum[1,])
upper <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_sum[3,])
med <- data.frame(chl1 = seq(1,100), chl_diff_cc = pred_sum[2,])

ggplot(dat1, aes(chl1, chl_diff_cc)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower, linetype = "dotdash", lwd = 1.25) + geom_line(data = upper, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
  ylab("Chl a change/ Daphnia*Day") + ggtitle("Stan: Wide Priors")

## fit in stan with literature
### data from literature

feed_lit <- read.csv("feeding.csv")
feed_lit <- feed_lit %>% filter(!is.na(point_est_cell_indiv_day)) %>% 
  filter(!is.na(algal_conc_cellperml)) %>% 
  dplyr::select(Title, replicates,point_est_cell_indiv_day,point_error,algal_conc_cellperml,sd)

## convert chl to cells in my data
dat1$cells <- chl_adj(dat1$chl1)
dat1$cell_diff <-chl_adj(dat1$chl_diff_cc) 
  

#######  fit in stan


## for now to see if I can get it run put in NA sds as 1 in lit sd ## hmm ok not the issue
for (i  in 1:nrow(feed_lit)){
  if(is.na(feed_lit$sd[i]) ) {
    feed_lit$sd[i] <- 10
  }
}

## simplest thing to do is to assume sd is same as in my dataset, a little conservative- but
## could model sd as mixed model among studies, could be lognormal, need if statement in stan (if sd is NA than likelihood)

daph_grow_list <- list(
  N = as.numeric(nrow(dat1)),
  chl = dat1$cells,
  diff = dat1$cell_diff,
  L = as.numeric(nrow(feed_lit)),
  lit_chl = as.numeric(feed_lit$algal_conc_cellperml),
  diff_lit = as.numeric(feed_lit$point_est_cell_indiv_day),
  sd_lit = feed_lit$sd
)

daph_grow_list <- 
  list(
    N = 10,
    chl = c(10,15,20,25,30,35,40,45,50,55),
    diff = c(2,5,12,15,15,26,29,35,42,50),
    L = 7,
    lit_chl = c(8,15,25,27,35,50,80),
    diff_lit = c(2,2,15,17,19,35,62),
    sd_lit = c(0.5,0.75,1,2.5,2.2,3,5)
  )

fit <- stan(file = "adult_feeding.stan", 
            data = daph_grow_list, verbose = F, chains = 1, control = list(adapt_delta = 0.95, max_treedepth = 12) ) 
launch_shinystan(fit)

fit_sum <- summary(fit)
(fit_sum_param <- fit_sum$summary[c(1:4),])
t <- rstan::extract(fit,permuted = FALSE)
m_pred <- rbind(t[,1,1],t[,2,1],t[,3,1],t[,4,1]) 


newdat <- data.frame(chl1 = seq(0,100))

pred_out <- apply(newdat,1,lin2,m= m_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))


feed_lit$cells <- feed_lit$algal_conc_cellperml
feed_lit$cell_diff <- feed_lit$point_est_cell_indiv_day

ggplot(data = dat1, aes(cells,cell_diff)) + geom_point() +
  geom_point(data = feed_lit, color = "blue", shape=4) + scale_x_log10()


##ok clearly my cell conversion is wrong, or i just used so much food-- which doesn't really seem true

