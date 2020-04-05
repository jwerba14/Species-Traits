##daily fecundity



## this fit is when we include literature, it is a mixed model (estimating parameters for each study)
## gets a weird fit because conversion of chla to cells is not precise and dependent on many environmental and growth conditions







##fit with wide priors (not incorporating literature data)
daph_fec_adj$cell2 <- chl_adj2(daph_fec_adj$chl)

#daph_fec_list_1 <- list(
#  "N" = 64,
#  "chl" = daph_fec_adj$cell2,
#  "daily_fec" = daph_fec_adj$daily_fec
#  )

daph_fec_list_1a <- list(
  "N" = 64,
  "chl" = daph_fec_adj$chl,
  "daily_fec" = daph_fec_adj$daily_fec
)


##

if(!file.exists("RDS_Files/fec.fit.wide.RDS")){
  
  fit_wide <- stan(file = "fec_wide.stan", 
                   data = daph_fec_list_1a,
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

#newdat_wide <- data.frame(cell = seq(26000000,1141832222, 10000000))

newdat_wide <- data.frame(chl = seq(0,100))

pred_out_wide <- apply(newdat_wide,1,sat_fun,a=a_wide,b=b_wide)
pred_sum_wide <- apply(pred_out_wide, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

#lower_wide <- data.frame(cell = seq(26000000,1141832222, 10000000), daily_fec = pred_sum_wide[1,])
#upper_wide <- data.frame(cell = seq(26000000,1141832222, 10000000), daily_fec = pred_sum_wide[3,])
#med_wide <- data.frame(cell = seq(26000000,1141832222, 10000000), daily_fec = pred_sum_wide[2,])

lower_wide <- data.frame( chl = seq(0,100), daily_fec = pred_sum_wide[1,])
upper_wide <- data.frame( chl = seq(0,100), daily_fec = pred_sum_wide[3,])
med_wide <- data.frame(chl = seq(0,100), daily_fec = pred_sum_wide[2,])

stan_wide_g <- ggplot(daph_fec_adj, aes(chl, daily_fec)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_wide, linetype = "dotdash", lwd = 1.25) + geom_line(data = upper_wide, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_wide, linetype = "solid", lwd =1.25) + xlab("Algal Cell Count") +
  ylab("Daily Fecundity") + ggtitle("Stan: Wide Priors")
 
print(stan_wide_g)
 
#ggplot(data= daph_fec_adj, aes(cell, chl)) + geom_point() #+ scale_x_log10()
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
yfit <- rlnorm(xfit,d$estimate[1], d$estimate[2] )
lines(xfit,yfit, col="blue")

if(!file.exists("RDS_Files/fec.fit.hyper.RDS")){
  
  fit4 <- stan(file = "hyper.stan", 
               data = daph_fec_list_1, iter = 5000, control = list(adapt_delta=0.9)) 
  
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

newdat_hyper <- data.frame(chl = seq(1,100, by = 0.5))

pred_out_hyper <- apply(newdat_hyper,1,sat_fun,a=a_hyper,b=b_hyper)
pred_sum_hyper <- apply(pred_out_hyper, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_hyper <- data.frame(chl = seq(1,100, by = 0.5), daily_fec = pred_sum_hyper[1,])
upper_hyper <- data.frame(chl = seq(1,100, by = 0.5), daily_fec = pred_sum_hyper[3,])
med_hyper <- data.frame(chl = seq(1,100, by = 0.5), daily_fec = pred_sum_hyper[2,])

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





