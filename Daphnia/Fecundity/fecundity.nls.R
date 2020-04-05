## fit with nls
fec_param_nls <- nls(daily_fec ~ sat_fun(z,w,chl), data = daph_fec_adj,
                     start = list(z=1,w=1))

pred_sum_nls <- summary(fec_param_nls)
coef_nls <- as.data.frame(pred_sum_nls$coefficients)
## graph
newdat_nls <- data.frame(chl = seq(0,100),
                         daily_fec = numeric(length = 101),
                         upper = 0,
                         lower = 0)

chl <- data.frame(chl = seq(0,100))
confidence <- confint2(fec_param_nls)
newdat_nls$daily_fec<- apply(chl,1,sat_fun,a=coef_nls[1,1],b=coef_nls[2,1])
newdat_nls$upper<- apply(chl,1,sat_fun,a=confidence[1,2],b=confidence[2,2])
newdat_nls$lower<- apply(chl,1,sat_fun,a=confidence[1,1],b=confidence[2,1])



(nls_fec_g <- ggplot(data = daph_fec_adj, aes(chl, daily_fec)) + geom_point() + 
    geom_ribbon(data = newdat_nls, aes(ymax = upper, ymin=lower), linetype = "dotdash", alpha = 0.2) + 
    geom_line(data = newdat_nls) +
    ggtitle("Saturating Fit (NLS)") + xlab("Chlorophyll a (ug/L)") + ylab("Daily Fecundity"))