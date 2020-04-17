##lit only -- weighted average-- because literature values already parameter of interest

dat_lit <- dat %>% filter(sd != "NA")
b1 <- mean(dat$daphnia_survival)
b2 <- weighted.mean(dat_lit$daphnia_survival, dat_lit$sd)
mean(dat_lit$daphnia_survival)
b3 <- weighted.mean(dat$daphnia_survival, dat$Replicates)

predb1 <-apply(newdat,1,expon,b=b1)
predb2 <-apply(newdat,1,expon,b=b2)
predb3 <-apply(newdat,1,expon,b=b3)

dd <- data.frame(average = predb1,
                 weighted_sd = predb2,
                 weighted_replicate = predb3,
                 day = newdat$days)

dd1 <- dd %>% pivot_longer(c(average,weighted_sd,weighted_replicate), names_to = "type", values_to = "frac_surv")

lit_g <- ggplot(daph_surv_curves, aes(day, frac_surv)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = dd1, aes(color=type, linetype=type), size = 3)+
  xlab("Day") + ylab(str_wrap("Proportion Surviving", width =10))+ ggtitle("Literature Only: NLS") +
  theme(legend.position = c(0.15,0.25),
        legend.direction = "vertical",
        legend.background = element_blank()) 



## for now only unweighted
pp<-quantile(dat$daphnia_survival, c(0.025,0.50,0.975))

