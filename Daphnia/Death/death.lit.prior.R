
#d <- fitdistr(dat$daphnia_survival, "normal" )

#set.seed(100)
#h <- hist(dat$daphnia_survival) 
#xfit <- seq(0,100)
#yfit <- rnorm(xfit,d$estimate[1], d$estimate[2] )

#ggplot(data = dat, aes(x=daphnia_survival)) + geom_histogram(aes(y=..density..)) +
  #geom_density(data = data.frame(daphnia_survival=yfit))


daph_death_list <- list(
  "N" = 74,
  "days" = daph_surv_curves$day,
  "survival" = daph_surv_curves$frac_surv
)

if(!file.exists("../RDS_Files/death.fit.inform.RDS")){
  fit_inf <- stan(file = "death_informedprior.stan", 
                  data = daph_death_list)
  saveRDS(fit_inf, file = "../RDS_Files/death.fit.inform.RDS")
}else {
  fit_inf <- readRDS("../RDS_Files/death.fit.inform.RDS")
}



#launch_shinystan(fit_inf)

fit_sum_inf <- summary(fit_inf)
fit_sum_param_inf <- fit_sum_inf$summary[c(1:4),]


t_inf <- rstan::extract(fit_inf,permuted = FALSE)
b_pred_inf <- rbind(t_inf[,1,1],t_inf[,2,1],t_inf[,3,1],t_inf[,4,1]) 

newdat <- data.frame(days = seq(0,60))

pred_out_inf <- apply(newdat,1,expon,b=b_pred_inf)
pred_sum_inf <- apply(pred_out_inf, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))



lower_inf <- data.frame(day = seq(0,60), frac_surv = pred_sum_inf[1,])
upper_inf <- data.frame(day = seq(0,60), frac_surv = pred_sum_inf[3,])
med_inf <- data.frame(day = seq(0,60), frac_surv= pred_sum_inf[2,])


inf_g <- ggplot(daph_surv_curves, aes(day, frac_surv)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_inf, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_inf, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_inf, linetype = "solid", lwd =1.25) + 
  xlab("Day") + ylab(str_wrap("Proportion Surviving", width = 10))+ ggtitle("Informed Prior") 

## make parameter dataframe

informed <- data.frame(param = rep("death2", 3),
                               quant = c("median", "lwr", "upr"),
                               method = rep("informed", 3),
                               value = c(fit_sum_param_inf[1,6], fit_sum_param_inf[1,4],fit_sum_param_inf[1,8]))
