## fecundity literature only estimates

daph_fec_list_lit1 <- list(
  "L" = 5,
  "chl_lit" = fec_lit1$cell,
  "daily_fec_lit" = fec_lit1$daily_fec,
  "sd_lit" = fec_lit1$sd_repro
)

if(!file.exists("../RDS_Files/fec.fit.lit.RDS")){
  fit_lit <- stan(file = "lit_mixed.stan", 
                  data = daph_fec_list_lit1,iter = 4400,
                  control = list(adapt_delta = 0.99, max_treedepth =13))
  
  
  saveRDS(fit_lit, file ="../RDS_Files/fec.fit.lit.RDS" )
} else {
  fit_lit<- readRDS("../RDS_Files/fec.fit.lit.RDS")
}



launch_shinystan(fit_lit)


## graphing

t5 <- rstan::extract(fit_lit,permuted = FALSE)
fit_sum_lit <- summary(fit_lit)
fit_sum_param_lit <- fit_sum_lit$summary[c(1:4),]
fit_sum_param_lit_chl <- fit_sum_param_lit %>% mutate()


a_lit<- rbind(t5[,1,2], t5[,2,2], t5[,3,2], t5[,4,2]) ## all rows, all chains alpha?
b_lit <- rbind(t5[,1,4], t5[,2,4], t5[,3,4], t5[,4,4])

newdat_lit <- data.frame(chl_lit = seq(100000,1141832222, 10000000))

pred_out_lit <- apply(newdat_lit,1,sat_fun,a=(a_lit),b=(b_lit))

pred_sum_lit <- apply(pred_out_lit, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

lower_lit <- data.frame(chl_lit = seq(100000,1141832222, 10000000), daily_fec_lit = pred_sum_lit[1,])
upper_lit <- data.frame(chl_lit = seq(100000,1141832222, 10000000), daily_fec_lit = pred_sum_lit[3,])
med_lit <- data.frame(chl_lit = seq(100000,1141832222, 10000000), daily_fec_lit = pred_sum_lit[2,])

fec_lit1$chl_lit <- fec_lit1$cell
fec_lit1$daily_fec_lit <- fec_lit1$daily_fec
stan_lit_g <- ggplot(fec_lit1, aes(chl_lit, daily_fec_lit)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_lit, linetype = "dotdash", lwd = 1.25) + geom_line(data = upper_lit, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_lit, linetype = "solid", lwd =1.25) + xlab("Cell count") +
  ylab("Daily Fecundity") + ggtitle("Stan:Literature Only- Mixed")


## graph on chl a scale
lower_lit <- lower_lit %>% mutate(chl = cell_adj(chl_lit))
upper_lit <- upper_lit %>% mutate(chl = cell_adj(chl_lit))
med_lit <- med_lit %>% mutate(chl = cell_adj(chl_lit))

stan_lit_g1 <- ggplot(fec_lit1, aes(chl, daily_fec_lit)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower_lit, linetype = "dotdash", lwd = 1.25) + geom_line(data = upper_lit, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med_lit, linetype = "solid", lwd =1.25) + xlab("Chlorophyll a (ug/L)") +
  ylab("Daily Fecundity") + ggtitle("Stan:Literature Only- Mixed")
