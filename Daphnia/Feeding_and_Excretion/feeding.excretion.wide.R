## wide priors, fit excetion and feeding simultaneously with just lab data

daph_feex_list <- 
  list(
    N = nrow(dat1),
    chl = dat1$chl1,
    diff_chl = dat1$chl_diff_cc,
    diff_nh4 = dat1$nh4_diff_cc
  )

if(!file.exists("../RDS_Files/feed_Exc.fit.wide.RDS")){
  
  fit_1 <- stan(file = "feed_exc_wide.stan", 
                data = daph_feex_list, verbose = F, chains = 4)  
  saveRDS(fit_1, file = "RDS_Files/feed_Exc.fit.wide.RDS")
} else {
  fit_1 <- readRDS("../RDS_Files/feed_Exc.fit.wide.RDS")
}




#launch_shinystan(fit_1)

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

#print(stan_wide_g)

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

#print(stan_wideex_g)



stan_wideex_g2 <- ggplot(dat1, aes(chl1, nh4_diff_cc)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_wide_x, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_wide_x, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_wide_x, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
  ylab("Nh4 (mg/L) perDaphnia*hr ") + ggtitle("Stan: Fit with Excretion Wide Priors")

#print(stan_wideex_g2)

