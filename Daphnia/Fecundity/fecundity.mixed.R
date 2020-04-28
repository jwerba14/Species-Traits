daph_fec_list <- list(
  "N" = 64,
  "chl" = daph_fec_adj$cell,
  "daily_fec" = daph_fec_adj$daily_fec,
  "L" = 5,
  "daily_fec_lit" = fec_lit1$daily_fec,
  "sd_lit" = fec_lit1$sd_repro,
  "chl_lit" = fec_lit1$cell
)


if(!file.exists("../RDS_Files/fec.fit.mixed.RDS")){
  
  fit2 <- stan(file = "fec_prior.stan", 
               data = daph_fec_list, chains = 4,
               control = list(adapt_delta = 0.99, max_treedepth = 17)) 
  
  saveRDS(fit2, file ="../RDS_Files/fec.fit.mixed.RDS" )
} else {
  fit2 <- readRDS("../RDS_Files/fec.fit.mixed.RDS")
}

##working finally!
  
#launch_shinystan(fit2)

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

stan_lit_sat_g <- ggplot(daph_fec_adj, aes((cell), daily_fec)) + geom_point(alpha = 0.6, size = 2 ) +
    geom_line(data = lower_mixed, linetype = "dotdash", lwd = 1.25) + geom_line(data = upper_mixed, linetype = "dotdash", lwd = 1.25)+
    geom_line(data = med_mixed, linetype = "solid", lwd =1.25) +
    geom_point(data = fec_lit, color = "blue", size = 3, shape = 4)+ 
    geom_errorbar(data=fec_lit, aes(ymin = daily_fec-sd_repro, ymax=daily_fec+sd_repro), color = "blue")+
    xlab("Algal Cell Count") +
    ylab("Daily Fecundity") + 
    ggtitle("Stan: heirarchical model")



## graph in chl a
lower_mixed <- lower_mixed %>% mutate(chl = cell_adj(lower_mixed$cell))
upper_mixed <- upper_mixed %>% mutate(chl = cell_adj(upper_mixed$cell))
med_mixed <- med_mixed %>% mutate(chl = cell_adj(med_mixed$cell))
daph_fec_adj <- daph_fec_adj %>% ungroup() %>% mutate(chl = cell_adj(daph_fec_adj$cell))


stan_lit_sat_g1 <- ggplot(daph_fec_adj, aes(chl, daily_fec)) + geom_point(alpha = 0.6, size = 2 )
stan_lit_sat_g2 <- stan_lit_sat_g1 + geom_line(data = lower_mixed, linetype = "dotdash", lwd = 1.25) + 
  geom_line(data = upper_mixed, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_mixed, linetype = "solid", lwd =1.25) +
  geom_point(data = fec_lit1, color = "blue", size = 3, shape = 4)+ 
  geom_errorbar(data=fec_lit1, aes(ymin = daily_fec-sd_repro, ymax=daily_fec+sd_repro), color = "blue")+
  xlab("Chlorophyll a (ug/L)") +
  ylab("Daily Fecundity") + 
  ggtitle("Stan: heirarchical model")

