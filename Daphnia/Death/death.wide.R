## fit with wide priors

daph_death_list <- list(
  "N" = 74,
  "days" = daph_surv_curves$day,
  "survival" = daph_surv_curves$frac_surv
)

if(!file.exists("../RDS_Files/fit.death.wide.rds")) {
  fit <- stan(file = "adult_death.stan", 
              data = daph_death_list) #,
  
  saveRDS(fit, file = "../RDS_Files/fit.death.wide.rds")
} else {
  fit <- readRDS("../RDS_Files/fit.death.wide.rds")
}

#launch_shinystan(fit)
## for ODE 1/beta is the rate- beta is the number of days until 1/e are lost
fit_sum <- summary(fit)
#print(names(fit_sum))
#print(fit_sum$summary)
fit_sum_param_d <- fit_sum$summary[c(1:4),]


t <- rstan::extract(fit,permuted = FALSE)
b_pred <- rbind(t[,1,1],t[,2,1],t[,3,1],t[,4,1]) 

newdat <- data.frame(days = seq(0,60))

pred_out <- apply(newdat,1,expon,b=b_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))



lower <- data.frame(day = seq(0,60), frac_surv = pred_sum[1,])
upper <- data.frame(day = seq(0,60), frac_surv = pred_sum[3,])
med <- data.frame(day = seq(0,60), frac_surv= pred_sum[2,])

wide_g <- ggplot(daph_surv_curves, aes(day, frac_surv)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med, linetype = "solid", lwd =1.25) + 
  xlab("Day") + ylab(str_wrap("Proportion Surviving", width = 10))+ ggtitle("Wide Priors") 


print(wide_g)