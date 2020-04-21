#source("set_up_fex.R")

## fit in stan
daph_feex_listj <- 
  list(
    N = nrow(dat1),
    chl = dat1$Chl.1,
    diff_chl = dat1$chl_diff_cc,
    diff_nh4 = dat1$nh4_diff_cc
  )


## forces excretion greater than 0

if(!file.exists("../RDS_Files/feed_Exc0.fit.j.RDS")){
  
  fit_j0 <- stan(file = "feed_exc_j.stan",  
                 data = daph_feex_listj, verbose = F, chains = 4)  
  saveRDS(fit_j0, file = "../RDS_Files/feed_Exc0.fit.j.RDS")
} else {
  fit_j0 <- readRDS("../RDS_Files/feed_Exc0.fit.j.RDS")
}

t_j0 <- rstan::extract(fit_j0,permuted = FALSE)
fit_sum_fej0 <- summary(fit_j0)
fit_sum_param_fej0 <- fit_sum_fej0$summary[c(1:4),]

slope_feeding_j0 <- rbind(t_j0[,1,1],t_j0[,2,1], t_j0[,3,1], t_j0[,4,1]) ## all rows, all chains 


newdat_fej0<- data.frame(Chl.1 = seq(1,35))

pred_feed_fej0 <- apply(newdat_fej0,1,lin2,m=slope_feeding_j)
pred_feed_sum_fej0 <- apply(pred_feed_fej0, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_fej0 <- data.frame(Chl.1 = seq(1,35), chl_diff_cc = pred_feed_sum_fej0[1,])

upper_fej0 <- data.frame(Chl.1 = seq(1,35), chl_diff_cc = pred_feed_sum_fej0[3,])
med_fej0 <- data.frame(Chl.1 = seq(1,35), chl_diff_cc = pred_feed_sum_fej0[2,])

stan_fej_g1 <- ggplot(dat1, aes(Chl.1, chl_diff_cc)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_fej0, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_fej0, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_fej0, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
  ylab("Chl (ug/L) per Daphnia*hr ") + ggtitle("A. Juvenile Feeding Fit with Excretion ")

#print(stan_fej_g1)


## graph excretion
slope_exc_j <- rbind(t_j[,1,3],t_j[,2,3], t_j[,3,3], t_j[,4,3])

newdat_1 <- data.frame(Chl.1 = seq(1,50)
)

pred_exc_j <- apply(newdat_1,1,lin3, m=slope_exc_j, t=slope_feeding_j )
pred_exc_sum_j <- apply(pred_exc_j, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_wide_jx <- data.frame(Chl.1 = seq(1,50), nh4_diff_cc = pred_exc_sum_j[1,], chl_diff_cc = seq(0.01,.28,0.0055))
upper_wide_jx <- data.frame(Chl.1 = seq(1,50), nh4_diff_cc = pred_exc_sum_j[3,], chl_diff_cc =  seq(0.01,.28,0.0055))
med_wide_jx <- data.frame(Chl.1 = seq(1,50), nh4_diff_cc = pred_exc_sum_j[2,], chl_diff_cc =  seq(0.01,.28,0.0055))

stan_wideexj_g1 <- ggplot(dat1, aes(chl_diff_cc, nh4_diff_cc)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_wide_jx, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_wide_jx, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_wide_jx, linetype = "solid", lwd =1.25) + xlab("Change in Chlorophyll a (ug/L)/ Daphnia*hr") +
  ylab("Nh4 (mg/L) perDaphnia*hr ") + ggtitle("B. Juvenile Excretion Wide Priors")

#print(stan_wideexj_g1)