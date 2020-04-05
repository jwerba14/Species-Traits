## fit feeding literature data and impute unreported sd

daph_imp_list <- list(
  L = as.numeric(nrow(feed_lit1)),
  miss = missing_n,
  lit_chl = as.numeric(feed_lit1$chl),
  diff_lit = as.numeric(feed_lit1$diff),
  sd_lit = feed_lit1$sd_feed,
  sd_index = index_sd 
)




if(!file.exists("../RDS_Files/feed.fit.lit.imp.RDS")){
  
  fit_lit <- stan(file = "bmb_imp.stan",
                  data = daph_imp_list, verbose = F, chains = 4, iter = 5000,thin = 2,
                  control = list(adapt_delta = 0.99, max_treedepth=13))
  saveRDS(fit_lit, file = "../RDS_Files/feed.fit.lit.imp.RDS")
} else {
  fit_lit <- readRDS("../RDS_Files/feed.fit.lit.imp.RDS")
}


#launch_shinystan(fit_lit)


fit_sum_lit <- summary(fit_lit)
(fit_sum_param_lit <- fit_sum_lit$summary[c(1:4),])
t_lit <- rstan::extract(fit_lit,permuted = FALSE)
m_pred_lit <- rbind(t_lit[,1,1],t_lit[,2,1],t_lit[,3,1],t_lit[,4,1]) 


newdat_lit <- data.frame(chl = seq(0,1.3, by = 0.002))

pred_out_lit <- apply(newdat_lit,1,lin2,m= m_pred_lit)
pred_sum_lit <- apply(pred_out_lit, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_lit <- data.frame(chl = seq(0,1.3, by = 0.002), diff = pred_sum_lit[1,])
upper_lit <- data.frame(chl = seq(0,1.3, by = 0.002), diff = pred_sum_lit[3,])
med_lit <- data.frame(chl = seq(0,1.3, by = 0.002), diff = pred_sum_lit[2,])

lit_g <- ggplot(feed_lit1, aes(chl, diff)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_lit, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_lit, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_lit, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
  ylab(" ") + ggtitle("Stan: Literature Only- Imputed")


#print(lit_g)
