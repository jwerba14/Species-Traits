##daily fecundity

library(tidyverse)
library(nlstools)
library(gridExtra)
library(fitdistrplus)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(shinystan)

source("../transfer_functions.R")
source("../chl_adj.R")
source("../Graphing_Set_Up.R")

daph <- read.csv("daphnia_lifetime.csv")
daph <- daph %>% 
  filter(adult_only=="N")

## to get fecundity parameter fit saturating curve (params z and w in full ode)
## need to make per day so need to divide total fecundity by # of days that individual was an adult
daph_fec <- daph %>% group_by(rep, treatment) %>%
  filter(size_class == "A") %>%
  summarize(
    time_adult = n()
    , life_fec   = sum(clutch_size, na.rm = TRUE)
    , chl        = mean(chl_avg)
    , chl_sd_rep = sd(chl_avg)) %>%
  mutate(daily_fec = life_fec / time_adult)


## make chl in cells per ml

daph_fec_adj <- daph_fec %>% 
  mutate(cell = chl_adj(chl = chl))

daph_fec_adj %>% dplyr::select(cell, chl)

daph_fec_adj$sd <- 0
## remove 0s because those are dead individuals

daph_fec_adj <- daph_fec_adj %>% 
  filter(!(daily_fec == 0 & chl > 5) )


## data from literature
fec_lit <- read.csv("fec_lit.csv")

fec_lit$cell <- c(NA,1e+09, NA, NA, 1e+08,5e+05, 166666.7, NA, 5e+05, NA,NA, NA)
fec_lit$sd <- fec_lit$sd_repro
fec_lit$daily_fec <- fec_lit$daphnia_reproduction
fec_lit$rep <- as.factor(rep("A", nrow(fec_lit)))


fec_lit1 <- fec_lit %>% filter(!is.na(cell))
fec_lit1<- fec_lit1 %>% 
  mutate(chl = cell_adj(cell = cell)) 

fec_lit1 <- fec_lit1 %>% mutate(cell1 = chl_adj(chl))
which(fec_lit1$cell != fec_lit1$cell1)

## this fit is when we include literature, it is a mixed model (estimating parameters for each study)
## gets a weird fit because conversion of chla to cells is not precise and dependent on many environmental and growth conditions

daph_fec_list <- list(
  "N" = 64,
  "chl" = daph_fec_adj$cell,
  "daily_fec" = daph_fec_adj$daily_fec,
  "L" = 5,
  "daily_fec_lit" = fec_lit1$daily_fec,
  "sd_lit" = fec_lit1$sd_repro,
  "chl_lit" = fec_lit1$cell
)

#daph_fec_list <- list(
#  "N" = 64,
 # "chl" = daph_fec_adj$chl,
  #"daily_fec" = daph_fec_adj$daily_fec,
  #"L" = 5,
#  "daily_fec_lit" = fec_lit1$daily_fec,
 # "sd_lit" = fec_lit1$sd_repro,
  #"chl_lit" = fec_lit1$chl
#)

##


#fit_chl <- stan(file = "fec_prior.stan", 
 #            data = daph_fec_list, chains = 4,
  #           control = list(adapt_delta = 0.99, max_treedepth = 17)) 

if(!file.exists("RDS_Files/fec.fit.mixed.RDS")){

fit2 <- stan(file = "fec_prior.stan", 
             data = daph_fec_list, chains = 4,
             control = list(adapt_delta = 0.99, max_treedepth = 17)) 

saveRDS(fit2, file ="RDS_Files/fec.fit.mixed.RDS" )
} else {
  fit2 <- readRDS("RDS_Files/fec.fit.mixed.RDS")
}

+##working finally!

launch_shinystan(fit2)

## sigma beta high then all studies dif, if measure on log10 scale then can have std of 1- dont allow to take impossible values

t1 <- rstan::extract(fit2,permuted = FALSE)
fit_sum_mix <- summary(fit2)
print(fit_sum_mix$summary[c(1:5),])
fit_sum_param_mix <- fit_sum_mix$summary[c(1:5),]

a_mix <- rbind(t1[,1,2],t1[,2,2], t1[,3,2], t1[,4,2]) ## all rows, all chains log_alpha
b_mix <- rbind(t1[,1,4], t1[,2,4], t1[,3,4], t1[,4,4]) ## all rows, all chains log_beta

newdat_mix <- data.frame(cell = seq(26000000,1141832222, 10000000))

pred_out_mix <- apply(newdat_mix,1,sat_fun,a=(a_mix),b=(b_mix))
pred_sum_mix <- apply(pred_out_mix, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_mixed <- data.frame(cell = seq(26000000,1141832222, 10000000), daily_fec = pred_sum_mix[1,])
upper_mixed <- data.frame(cell = seq(26000000,1141832222, 10000000), daily_fec = pred_sum_mix[3,])
med_mixed <- data.frame(cell = seq(26000000,1141832222, 10000000), daily_fec = pred_sum_mix[2,])

(stan_lit_sat_g <- ggplot(daph_fec_adj, aes((cell), daily_fec)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_mixed, linetype = "dotdash", lwd = 1.25) + geom_line(data = upper_mixed, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_mixed, linetype = "solid", lwd =1.25) +
  geom_point(data = fec_lit, color = "blue", size = 3, shape = 4)+ 
  geom_errorbar(data=fec_lit, aes(ymin = daily_fec-sd_repro, ymax=daily_fec+sd_repro), color = "blue")+
  xlab("Algal Cell Count") +
  ylab("Daily Fecundity") + 
  ggtitle("Stan: heirarchical model"))





##fit with wide priors (not incorporating literature data)
daph_fec_adj$cell2 <- chl_adj2(daph_fec_adj$chl)

daph_fec_list_1 <- list(
  "N" = 64,
  "chl" = daph_fec_adj$cell2,
  "daily_fec" = daph_fec_adj$daily_fec
  )

##

if(!file.exists("RDS_Files/fec.fit.wide.RDS")){
  
  fit_wide <- stan(file = "fec_stan.stan", 
                   data = daph_fec_list_1,
                   control = list(adapt_delta = 0.99))
  
  saveRDS(fit_wide, file ="RDS_Files/fec.fit.wide.RDS" )
} else {
  fit_wide <- readRDS("RDS_Files/fec.fit.wide.RDS")
}


launch_shinystan(fit_wide)


t2 <- rstan::extract(fit_wide,permuted = FALSE)
fit_sum_wide <- summary(fit_wide)
fit_sum_param_wide <- fit_sum_wide$summary[c(1:4),]

ftscell <- summary(fit_wide)
ftscell_p <- ftscell$summary[c(1:4),]

ftscell2 <- summary(fit_wide)
ftscell_p2 <- ftscell$summary[c(1:4),]


a_wide <- rbind(t2[,1,1],t2[,2,1], t2[,3,1], t2[,4,1]) ## all rows, all chains alpha?
b_wide <- rbind(t2[,1,2], t2[,2,2], t2[,3,2], t2[,4,2])

newdat_wide <- data.frame(cell = seq(26000000,1141832222, 10000000))

newdat_wide <- data.frame(chl = seq(0,100))

pred_out_wide <- apply(newdat_wide,1,sat_fun,a=a_wide,b=b_wide)
pred_sum_wide <- apply(pred_out_wide, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_wide <- data.frame(cell = seq(26000000,1141832222, 10000000), daily_fec = pred_sum_wide[1,])
upper_wide <- data.frame(cell = seq(26000000,1141832222, 10000000), daily_fec = pred_sum_wide[3,])
med_wide <- data.frame(cell = seq(26000000,1141832222, 10000000), daily_fec = pred_sum_wide[2,])

lower_wide <- data.frame( chl = seq(0,100), daily_fec = pred_sum_wide[1,])
upper_wide <- data.frame( chl = seq(0,100), daily_fec = pred_sum_wide[3,])
med_wide <- data.frame(chl = seq(0,100), daily_fec = pred_sum_wide[2,])

stan_wide_g <- ggplot(daph_fec_adj, aes(cell, daily_fec)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_wide, linetype = "dotdash", lwd = 1.25) + geom_line(data = upper_wide, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_wide, linetype = "solid", lwd =1.25) + xlab("Algal Cell Count") +
  ylab("Daily Fecundity") + ggtitle("Stan: Wide Priors")
  
ggplot(data= daph_fec_adj, aes(cell, chl)) + geom_point() #+ scale_x_log10()
## fit with literature only as constraining upper limit of curve 

fec_lit2 <- fec_lit %>% filter(daphnia_reproduction > 4) %>% mutate(cell = 1000000000)


daph_fec_list2 <- list(
  "N" = 64,
  "chl" = daph_fec_adj$cell,
  "daily_fec" = daph_fec_adj$daily_fec,
  "L" = 5,
  "daily_fec_lit" = fec_lit2$daily_fec,
  "sd_lit" = fec_lit2$sd_repro,
  "chl_lit" = fec_lit2$cell
)

if(!file.exists("RDS_Files/fec.fit.constraineda.RDS")){
  
  fit3 <- stan(file = "fec_a_constrained1.stan", 
               data = daph_fec_list2,chains = 4,
               control = list(adapt_delta = 0.99, max_treedepth = 13) )
  
  saveRDS(fit3, file ="RDS_Files/fec.fit.constraineda.RDS" )
} else {
  fit3 <- readRDS("RDS_Files/fec.fit.constraineda.RDS")
}

          
## no divergent transitions! I have never been happier in my whole life.
launch_shinystan(fit3)

t3 <- rstan::extract(fit3,permuted = FALSE)
fit_sum_cona <- summary(fit3)
print(names(fit_sum_cona))
fit_sum_param_cona <- fit_sum_cona$summary[c(1:4),]



a_cona <- rbind(t3[,1,1],t3[,2,1], t3[,3,1], t3[,4,1]) ## all rows, all chains alpha?
b_cona <- rbind(t3[,1,2], t3[,2,2], t3[,3,2], t3[,4,2])

newdat_cona <- data.frame(cell = seq(26000000,1141832222, 10000000))

pred_out_cona <- apply(newdat_cona,1,sat_fun,a=exp(a_cona),b=exp(b_cona))
pred_sum_cona <- apply(pred_out_cona, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_cona <- data.frame(cell = seq(26000000,1141832222, 10000000), daily_fec = pred_sum_cona[1,])
upper_cona <- data.frame(cell = seq(26000000,1141832222, 10000000), daily_fec = pred_sum_cona[3,])
med_cona <- data.frame(cell = seq(26000000,1141832222, 10000000), daily_fec = pred_sum_cona[2,])

stan_con_g <- ggplot(daph_fec_adj, aes(cell, daily_fec)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_cona, linetype = "dotdash", lwd = 1.25) + geom_line(data = upper_cona, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_cona, linetype = "solid", lwd =1.25) + xlab("Cell count") +
  ylab("Daily Fecundity") + ggtitle("Stan: Constrain a")

## makes me feel that priors are too tight

## fit with nls
fec_param_nls <- nls(daily_fec ~ sat_fun(z,w,chl), data = daph_fec_adj,
                 start = list(z=1,w=1))

pred_sum_nls <- summary(fec_param_nls)
coef_nls <- as.data.frame(pred_sum_nls$coefficients)
## graph
newdat_nls <- data.frame(chl = seq(0,100),
                         daily_fec = numeric(length = 101),
                         upper = 0,
                         lower = 0)

chl <- data.frame(chl = seq(0,100))
confidence <- confint2(fec_param_nls)
newdat_nls$daily_fec<- apply(chl,1,sat_fun,a=coef_nls[1,1],b=coef_nls[2,1])
newdat_nls$upper<- apply(chl,1,sat_fun,a=confidence[1,2],b=confidence[2,2])
newdat_nls$lower<- apply(chl,1,sat_fun,a=confidence[1,1],b=confidence[2,1])



(nls_fec_g <- ggplot(data = daph_fec_adj, aes(chl, daily_fec)) + geom_point() + 
  geom_ribbon(data = newdat_nls, aes(ymax = upper, ymin=lower), linetype = "dotdash", alpha = 0.2) + 
  geom_line(data = newdat_nls) +
  ggtitle("Saturating Fit (NLS)") + xlab("Chlorophyll a (ug/L)") + ylab("Daily Fecundity"))
 


## fit with literature as overarching distribution on daily-fecundity

lit <- fec_lit %>% 
  dplyr::select(daphnia_reproduction, sd_repro, Replicates) %>%
  filter(sd_repro != "NA")
  
##look at other function options to fit with non-integer weights
d <- fitdist(lit$daphnia_reproduction, "lnorm", weights = lit$Replicates)


set.seed(100)
h <- hist(lit$daphnia_reproduction) 
xfit <- seq(0,100)
yfit <- rnorm(xfit,d$estimate[1], d$sd[1] )
lines(xfit,yfit, col="blue")

if(!file.exists("RDS_Files/fec.fit.hyper.RDS")){
  
  fit4 <- stan(file = "hyper.stan", 
               data = daph_fec_list_1, iter = 5000) 
  
  saveRDS(fit4, file ="RDS_Files/fec.fit.hyper.RDS" )
} else {
  fit4<- readRDS("RDS_Files/fec.fit.hyper.RDS")
}



launch_shinystan(fit4)


t4 <- rstan::extract(fit4,permuted = FALSE)
fit_sum_hyper <- summary(fit4)
print(names(fit_sum_hyper))
fit_sum_param_hyper <- fit_sum_hyper$summary[c(1:4),] ##8.03, 11.35



a_hyper <- rbind(t4[,1,1],t4[,2,1], t4[,3,1], t4[,4,1]) ## all rows, all chains alpha?
b_hyper <- rbind(t4[,1,2], t4[,2,2], t4[,3,2], t4[,4,2])

newdat_hyper <- data.frame(chl = seq(1,100))

pred_out_hyper <- apply(newdat_hyper,1,sat_fun,a=a_hyper,b=b_hyper)
pred_sum_hyper <- apply(pred_out_hyper, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_hyper <- data.frame(chl = seq(1,100), daily_fec = pred_sum_hyper[1,])
upper_hyper <- data.frame(chl = seq(1,100), daily_fec = pred_sum_hyper[3,])
med_hyper <- data.frame(chl = seq(1,100), daily_fec = pred_sum_hyper[2,])

(stan_hyper_g <- ggplot(daph_fec_adj, aes(chl, daily_fec)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_hyper, linetype = "dotdash", lwd = 1.25) + geom_line(data = upper_hyper, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_hyper, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
  ylab("Daily Fecundity") + ggtitle("Stan: Hyper Priors"))



### get parameter just from literature (NLS) ## this gives impossible values so use Stan

d_fec <- nls(daily_fec ~ sat_fun(z,w,chl), data = fec_lit1, start = list(z=1,w=1))
#d_fec <- nlxb(daily_fec ~ hollings2(a,h,chl), data = fec_lit1, start = list(a=2,h=100))
coef_nls_lit <- data.frame(coef(d_fec))
newdat_nls_lit <- data.frame(chl = seq(0,80),
                         daily_fec = numeric(length = 81),
                         upper = 0,
                         lower = 0)

chl <- data.frame(chl = seq(0,80))
confidence <- confint2(d_fec)
newdat_nls_lit$daily_fec<- apply(chl,1,sat_fun,a=coef_nls_lit[1,1],b=coef_nls_lit[2,1])
newdat_nls_lit$upper<- apply(chl,1,sat_fun,a=confidence[1,2],b=confidence[2,2])
newdat_nls_lit$lower<- apply(chl,1,sat_fun,a=confidence[1,1],b=confidence[2,1])



(nls_lit_g <- ggplot(data =fec_lit1, aes(chl, daily_fec)) + geom_point() + 
    geom_ribbon(data = newdat_nls_lit, aes(ymax = upper, ymin=lower), linetype = "dotdash" ) + 
    geom_line(data = newdat_nls_lit) +
    ggtitle("Saturating Fit Literature (NLS)") + xlab("Chlorophyll a (ug/L)") + ylab("Daily Fecundity"))


### parameter from literature only Stan

daph_fec_list_lit1 <- list(
  "L" = 5,
  "chl_lit" = fec_lit1$cell,
  "daily_fec_lit" = fec_lit1$daily_fec,
  "sd_lit" = fec_lit1$sd_repro
)

if(!file.exists("RDS_Files/fec.fit.lit.RDS")){
  fit_lit <- stan(file = "lit_mixed.stan", 
                  data = daph_fec_list_lit1,
                  control = list(adapt_delta = 0.99, max_treedepth =13))
   
  
  saveRDS(fit_lit, file ="RDS_Files/fec.fit.lit.RDS" )
} else {
  fit_lit<- readRDS("RDS_Files/fec.fit.lit.RDS")
}



launch_shinystan(fit_lit)


t5 <- rstan::extract(fit_lit,permuted = FALSE)
fit_sum_lit <- summary(fit_lit)
fit_sum_param_lit <- fit_sum_lit$summary[c(1:4),]



a_lit<- rbind(t5[,1,2], t5[,2,2], t5[,3,2], t5[,4,2]) ## all rows, all chains alpha?
b_lit <- rbind(t5[,1,4], t5[,2,4], t5[,3,4], t5[,4,4])

newdat_lit <- data.frame(chl_lit = seq(100000,1141832222, 10000000))

pred_out_lit <- apply(newdat_lit,1,sat_fun,a=exp(a_lit),b=exp(b_lit))

pred_sum_lit <- apply(pred_out_lit, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_lit <- data.frame(chl_lit = seq(100000,1141832222, 10000000), daily_fec_lit = pred_sum_lit[1,])
upper_lit <- data.frame(chl_lit = seq(100000,1141832222, 10000000), daily_fec_lit = pred_sum_lit[3,])
med_lit <- data.frame(chl_lit = seq(100000,1141832222, 10000000), daily_fec_lit = pred_sum_lit[2,])

fec_lit1$chl_lit <- fec_lit1$cell
fec_lit1$daily_fec_lit <- fec_lit1$daily_fec
stan_lit_g <- ggplot(fec_lit1, aes(chl_lit, daily_fec_lit)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_lit, linetype = "dotdash", lwd = 1.25) + geom_line(data = upper_lit, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_lit, linetype = "solid", lwd =1.25) + xlab("Cell count") +
  ylab("Daily Fecundity") + ggtitle("Stan:Literature Only- Mixed")


## all estimates
fecundity_est <- list(
  "mixed_model" = fit_sum_param_mix,
  "wide_prior"=fit_sum_param_wide,
  "constrain_a" = fit_sum_param_cona,
  "nls" = pred_sum_nls,
  "hyper" = pred_sum_hyper,
  "lit" = fit_sum_param_lit
)

## final graphic
grid.arrange(nls_fec_g,stan_lit_g,stan_lit_sat_g,stan_wide_g,stan_hyper_g,stan_con_g, nrow=3)


##2/14/2020 -- clear my study isn't being weighted appropriately (see let only vs heirarchical predictions,
## bascially the same-- likely because for alpha_bar pull from distribution of 6 alphas one of which is my study and then
## indiv ones per each study-- which isn't what I want- because it is over weighting essentially single points)





