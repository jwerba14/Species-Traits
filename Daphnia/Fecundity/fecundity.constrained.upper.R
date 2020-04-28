## fit with literature only as constraining upper limit of curve 

fec_lit2 <- fec_lit %>% filter(daphnia_reproduction > 4) %>% mutate(cell = 1000000000)


daph_fec_list2 <- list(
  "N" = 64,
  "chl" = daph_fec_adj$cell,
  "daily_fec" = daph_fec_adj$daily_fec,
  "L" = 5,
  "daily_fec_lit" = fec_lit2$daily_fec,
  "sd_lit" = fec_lit2$sd_repro,
  "chl_lit" = fec_lit2$cell
)

if(!file.exists("../RDS_Files/fec.fit.constraineda.RDS")){
  
  fit3 <- stan(file = "fec_a_constrained1.stan", 
               data = daph_fec_list2,chains = 4,
               control = list(adapt_delta = 0.99, max_treedepth = 13) )
  
  saveRDS(fit3, file ="../RDS_Files/fec.fit.constraineda.RDS" )
} else {
  fit3 <- readRDS("../RDS_Files/fec.fit.constraineda.RDS")
}


## no divergent transitions! I have never been happier in my whole life.
#launch_shinystan(fit3)

t3 <- rstan::extract(fit3,permuted = FALSE)
fit_sum_cona <- summary(fit3)
#print(names(fit_sum_cona))
fit_sum_param_cona <- fit_sum_cona$summary[c(1:5),]
fit_sum_param_cona_chl <- as.data.frame(fit_sum_param_cona) %>%
                          dplyr::select(-n_eff, -Rhat) %>%
                          mutate_each(cell_adj)
fit_sum_param_cona_chl <- cbind(fit_sum_param_cona_chl, fit_sum_param_cona)

## check to see if conversion makes sense
fit_sum_param_cona <- as.data.frame(fit_sum_param_cona)
chl = seq(0,111)
cell = seq(26000000,1141832222, 10000000)
nd <- data.frame(chl = chl,
                 cell = cell,
                 out = sat_fun(a= fit_sum_param_cona_chl$mean[2], b=fit_sum_param_cona_chl$mean[4], k=chl),
                 out2 = sat_fun(a= fit_sum_param_cona$mean[2], b=fit_sum_param_cona$mean[4], k=cell)
)

ggplot(nd, aes(chl, out)) + geom_point()
ggplot(nd, aes(cell,out2)) + geom_point()

a_cona <- rbind(t3[,1,1],t3[,2,1], t3[,3,1], t3[,4,1]) ## all rows, all chains alpha?
b_cona <- rbind(t3[,1,2], t3[,2,2], t3[,3,2], t3[,4,2])

newdat_cona <- data.frame(cell = seq(26000000,1141832222, 10000000))

pred_out_cona <- apply(newdat_cona,1,sat_fun,a=exp(a_cona),b=exp(b_cona))
pred_sum_cona <- apply(pred_out_cona, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_cona <- data.frame(cell = seq(26000000,1141832222, 10000000), daily_fec = pred_sum_cona[1,])
upper_cona <- data.frame(cell = seq(26000000,1141832222, 10000000), daily_fec = pred_sum_cona[3,])
med_cona <- data.frame(cell = seq(26000000,1141832222, 10000000), daily_fec = pred_sum_cona[2,])

stan_con_g <- ggplot(daph_fec_adj, aes(cell, daily_fec)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_cona, linetype = "dotdash", lwd = 1.25) + geom_line(data = upper_cona, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_cona, linetype = "solid", lwd =1.25) + xlab("Cell count") +
  ylab("Daily Fecundity") + ggtitle("Stan: Constrain a")

## makes me feel that priors are too tight


## graph on chl scale
lower_cona <- lower_cona %>% mutate(chl = cell_adj(cell))
upper_cona <- upper_cona %>% mutate(chl = cell_adj(cell))
med_cona <- med_cona %>% mutate(chl = cell_adj(cell))
daph_fec_adj <- daph_fec_adj %>% mutate(chl = cell_adj(cell))

stan_con_g1 <- ggplot(daph_fec_adj, aes(chl, daily_fec)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_cona, linetype = "dotdash", lwd = 1.25) + geom_line(data = upper_cona, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_cona, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
  ylab("Daily Fecundity") + ggtitle("Stan: Constrain a")
