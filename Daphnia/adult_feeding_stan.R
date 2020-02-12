## adult daphnia feeding and excretion

source("../transfer_functions.R")
source("../chl_adj.R")
source("../Graphing_Set_Up.R")
library(tidyverse)
library(nlmrt)
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
#ggplot(dat1, aes(chl1,chl_diff_cc)) + geom_point()



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

fit1 <- stan(file = "fec_linear_wideprior.stan", 
            data = daph_feed_list, verbose = F, chains = 4) 
launch_shinystan(fit)


t <- rstan::extract(fit1,permuted = FALSE)
fit_sum <- summary(fit1)
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

stan_wide_g <- ggplot(dat1, aes(chl1, chl_diff_cc)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower, linetype = "dotdash", lwd = 1.25) + geom_line(data = upper, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
  ylab("Chl a change/ Daphnia*Day") + ggtitle("Stan: Wide Priors")

## fit in stan with literature
### data from literature

feed_lit <- read.csv("feeding.csv")
feed_lit <- feed_lit %>% filter(!is.na(point_est_cell_indiv_day)) %>% 
  filter(!is.na(algal_conc_cellperml)) %>% 
  dplyr::select(Title, replicates,point_est_cell_indiv_day,point_error,algal_conc_cellperml,sd)

index_sd <- with(feed_lit, which(is.na(sd),)) ## index of which rows have missing SD 
missing_n <- length(index_sd)

##copy dataframe
feed_lit1 <- feed_lit
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
  sd_lit = feed_lit1$sd,
  sd_index = index_sd 
)


daph_imp_list <- list(
  L = as.numeric(nrow(feed_lit)),
  miss = missing_n,
  lit_chl = as.numeric(feed_lit$algal_conc_cellperml),
  diff_lit = as.numeric(feed_lit$point_est_cell_indiv_day),
  sd_lit = feed_lit1$sd,
  sd_index = index_sd 
)


fit <- stan(file = "adult_feeding.stan", init=list(list(shape = 5, scale = 5, slope_bar = 1)), 
            data = daph_grow_list, verbose = F, chains = 1) #, control = list(adapt_delta = 0.95, max_treedepth = 12) ) 
launch_shinystan(fit)

## something about shape/scale is wrong -- on the wrong scale cant get past initial value
fit <- stan(file = "lit_imputation.stan",  
            data = daph_grow_list, verbose = F, chains = 1)


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

## hyperparameter stan
feed_lit[is.na(feed_lit$replicates)] <- 1
feed_lit<-droplevels(feed_lit)
d <- fitdist(feed_lit$point_est_cell_indiv_day, "lnorm", weights = as.numeric(as.character(feed_lit$replicates)))

## check the weights warning...are the weights doing anything??
set.seed(100)
h <- hist(feed_lit$point_est_cell_indiv_day) 
xfit <- seq(0,1000000000,500)
yfit <- rnorm(xfit,d$estimate[1], d$sd[1] )
lines(xfit,yfit, col="blue")
###hmmmmmmmmmmmmmm


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

##doesn't at all look linear....
d_feed <- nls(point_est_cell_indiv_day ~ sat_fun(z,w,algal_conc_cellperml),
             data = feed_lit1, start = list(z=1000000,w=10000))
coef_nls_lit <- data.frame(coef(d_feed))
newdat_nls_lit <- data.frame(algal_conc_cellperml = seq(min(feed_lit1$algal_conc_cellperml),
                                                        max(feed_lit1$algal_conc_cellperml),
                                                        length = 1000),
                             point_est_cell_indiv_day = numeric(length = 1000),
                             upper = 0,
                             lower = 0)

algal_conc_cellperml <- data.frame(algal_conc_cellperml = seq(min(feed_lit1$algal_conc_cellperml),
                                                              max(feed_lit1$algal_conc_cellperml),
                                                              length = 1000))
confidence <- confint2(d_feed)
newdat_nls_lit$point_est_cell_indiv_day<- apply(algal_conc_cellperml,1,sat_fun,a=coef_nls_lit[1,1],b=coef_nls_lit[2,1])
newdat_nls_lit$upper<- apply(algal_conc_cellperml,1,sat_fun,a=confidence[1,2],b=confidence[2,2])
newdat_nls_lit$lower<- apply(algal_conc_cellperml,1,sat_fun,a=confidence[1,1],b=confidence[2,1])



(nls_lit_g <- ggplot(data =feed_lit1, aes(algal_conc_cellperml, point_est_cell_indiv_day)) + geom_point() + 
    geom_ribbon(data = newdat_nls_lit, aes(ymax = upper, ymin=lower), linetype = "dotdash" ) + 
    geom_line(data = newdat_nls_lit) +
    ggtitle("Saturating Fit Literature (NLS)") + xlab("Chlorophyll a (ug/L)") + ylab("Feeding"))
## nope negative problem again....

daph_feed_list_lit <- list(
  "N" = as.numeric(nrow(feed_lit1)),
  "chl" = feed_lit1$algal_conc_cellperml,
  "daily_fec" = feed_lit1$point_est_cell_indiv_day
)


fit_lit <- stan(file = "fec_stan.stan", 
                data = daph_feed_list_lit,
                verbose = TRUE, control = list(adapt_delta = 0.99, stepsize = 1, max_treedepth =13))

launch_shinystan(fit_lit)
## many divergent transitions

