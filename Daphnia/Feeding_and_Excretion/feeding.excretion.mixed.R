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



if(!file.exists("../RDS_Files/feed_exc.fit.mix.RDS")){
  
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
    saveRDS(fit_mixed, file = "../RDS_Files/feed_exc.fit.mix.RDS")
    
    test_div <- sum(attr(fit_mixed@sim$samples[[4]], "sampler_params")$divergent__[1001:2000]) +
      sum(attr(fit_mixed@sim$samples[[3]], "sampler_params")$divergent__[1001:2000]) +
      sum(attr(fit_mixed@sim$samples[[2]], "sampler_params")$divergent__[1001:2000]) +
      sum(attr(fit_mixed@sim$samples[[1]], "sampler_params")$divergent__[1001:2000])
    
  }
  
} else {
  fit_mixed <- readRDS("../RDS_Files/feed_exc.fit.mix.RDS")
} 

#launch_shinystan(fit_mixed)  

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

#print(stan_mixed_g)

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

#print(stan_mixex_g)

