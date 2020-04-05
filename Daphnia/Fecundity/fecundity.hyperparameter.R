## hyper parameter for fecundity

lit <- fec_lit %>% 
  dplyr::select(daphnia_reproduction, sd_repro, Replicates) %>%
  filter(sd_repro != "NA")

##look at other function options to fit with non-integer weights
#d <- fitdist(lit$daphnia_reproduction, "lnorm", weights = lit$Replicates)


#set.seed(100)
#h <- hist(lit$daphnia_reproduction) 
#xfit <- seq(0,100)
#yfit <- rlnorm(xfit,d$estimate[1], d$estimate[2] )
#lines(xfit,yfit, col="blue")

daph_fec_list_1 <- list(
  "N" = 64,
  "chl" = daph_fec_adj$cell2,
  "daily_fec" = daph_fec_adj$daily_fec
  )



if(!file.exists("../RDS_Files/fec.fit.hyper.RDS")){
  
  fit4 <- stan(file = "hyper.stan", 
               data = daph_fec_list_1, iter = 5000, control = list(adapt_delta=0.9)) 
  
  saveRDS(fit4, file ="../RDS_Files/fec.fit.hyper.RDS" )
} else {
  fit4<- readRDS("../RDS_Files/fec.fit.hyper.RDS")
}



#launch_shinystan(fit4)


t4 <- rstan::extract(fit4,permuted = FALSE)
fit_sum_hyper <- summary(fit4)
#print(names(fit_sum_hyper))
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

