##fit with wide priors (not incorporating literature data)
daph_fec_adj$cell2 <- chl_adj2(daph_fec_adj$chl)


daph_fec_list_1a <- list(
  "N" = 64,
  "chl" = daph_fec_adj$chl,
  "daily_fec" = daph_fec_adj$daily_fec
)


##

if(!file.exists("../RDS_Files/fec.fit.wide.RDS")){
  
  fit_wide <- stan(file = "fec_wide.stan", 
                   data = daph_fec_list_1a,
                   control = list(adapt_delta = 0.99))
  
  saveRDS(fit_wide, file ="../RDS_Files/fec.fit.wide.RDS" )
} else {
  fit_wide <- readRDS("../RDS_Files/fec.fit.wide.RDS")
}


#launch_shinystan(fit_wide)


t2 <- rstan::extract(fit_wide,permuted = FALSE)
fit_sum_wide <- summary(fit_wide)
fit_sum_param_wide <- fit_sum_wide$summary[c(1:4),]

ftscell <- summary(fit_wide)
ftscell_p <- ftscell$summary[c(1:4),]

ftscell2 <- summary(fit_wide)
ftscell_p2 <- ftscell$summary[c(1:4),]


a_wide <- rbind(t2[,1,1],t2[,2,1], t2[,3,1], t2[,4,1]) 
b_wide <- rbind(t2[,1,2], t2[,2,2], t2[,3,2], t2[,4,2])



newdat_wide <- data.frame(chl = seq(0,100))

pred_out_wide <- apply(newdat_wide,1,sat_fun,a=a_wide,b=b_wide)
pred_sum_wide <- apply(pred_out_wide, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))



lower_wide <- data.frame( chl = seq(0,100), daily_fec = pred_sum_wide[1,])
upper_wide <- data.frame( chl = seq(0,100), daily_fec = pred_sum_wide[3,])
med_wide <- data.frame(chl = seq(0,100), daily_fec = pred_sum_wide[2,])

stan_wide_g <- ggplot(daph_fec_adj, aes(chl, daily_fec)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_wide, linetype = "dotdash", lwd = 1.25) + geom_line(data = upper_wide, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_wide, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
  ylab("Daily Fecundity") + ggtitle("Stan: Wide Priors")


