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



if(!file.exists( "../RDS_Files/feex.fit.mix.imp.RDS")){
  
  
  fit_mix_imp <- stan(file = "feed.mix.imp.stan", 
                      data = list_imp2)
  
  saveRDS(fit_mix_imp, file = "../RDS_Files/feex.fit.mix.imp.RDS")
} else {
  fit_mix_imp  <- readRDS( "../RDS_Files/feex.fit.mix.imp.RDS")
}



#launch_shinystan(fit_mix_imp)



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

#print(stan_mix_imp)


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

#print(stan_mimpex_g)

