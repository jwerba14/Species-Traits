## literature only varying slope no imputation-- so uses many fewer studies (only studies with reported sds)


lit_list_sd <- list(
  L = as.numeric(nrow(feed_lit2)),
  lit_chl = as.numeric(feed_lit2$chl),
  diff_lit = as.numeric(feed_lit2$diff),
  sd_lit = feed_lit2$sd_feed,
  M = as.numeric(length(unique(feed_lit2$Title)))
  
)



if(!file.exists("../RDS_Files/feed.fit.lit.vs.RDS")){
  
  lit_vslope <- stan(file = "lit_varyslope.stan", data = lit_list_sd,
                     control = list(adapt_delta = 0.9))
  
  saveRDS(lit_vslope, file = "../RDS_Files/feed.fit.lit.vs.RDS")
} else {
  lit_vslope <- readRDS("../RDS_Files/feed.fit.lit.vs.RDS")
}  




#launch_shinystan(lit_vslope)



fit_sum_lit_s <- summary(lit_vslope)
(fit_sum_param_lit_s <- fit_sum_lit_s$summary[c(1:2),])
t_lit_s <- rstan::extract(lit_vslope,permuted = FALSE)
m_pred_lit_s <- rbind(t_lit_s[,1,1],t_lit_s[,2,1],t_lit_s[,3,1],t_lit_s[,4,1]) 


newdat_lit_s <- data.frame(chl = seq(0,1.3, by = 0.002))

pred_out_lit_s <- apply(newdat_lit_s,1,lin2,m= m_pred_lit_s)
pred_sum_lit_s <- apply(pred_out_lit_s, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_lit_s <- data.frame(chl = seq(0,1.3, by = 0.002), diff = pred_sum_lit_s[1,])
upper_lit_s <- data.frame(chl = seq(0,1.3, by = 0.002), diff = pred_sum_lit_s[3,])
med_lit_s <- data.frame(chl = seq(0,1.3, by = 0.002), diff = pred_sum_lit_s[2,])

lit_g_s <- ggplot(feed_lit2, aes(chl, diff)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_lit_s, linetype = "dotdash", lwd = 1.25) +
  geom_line(data = upper_lit_s, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_lit_s, linetype = "solid", lwd =1.25) + xlab("Chl a (ug/L)") +
  ylab("Chl a change/ Daphnia*hour") + ggtitle("Stan: Literature Only- Vary Slopes")

#print(lit_g_s)

