dat2 <- read.csv()

ttt <- c(rep(seq(1,6),each = 5))

#nSamples = nrow(growthdata) - 1 #use time=0 as initial condition, take this as fixed

nSamples = nrow(dat2)-1 ## 

#y0 = filter(growthdata,time==0) %>% select(-time) %>% unlist #initial condition

y0 = filter(dat2, date1 == 1) %>% select(-date1) %>% unlist 
t0 = 0.0


#ts = filter(growthdata,time>0) %>% select(time) %>% unlist
ts = filter(dat2, date1 >1) %>% select(date1) %>% unlist

#z = filter(growthdata,time>0) %>% select(-time)

z = filter(dat2, date1 >1) %>% select(-date1)

#n_wells = 9 #running on all wells can be slow

rep = 30

logisticgrowth_stan <- stan_model(file = "logisticmix.stan", model_name = "logisticmix")

estimates <- sampling(object = logisticgrowth_stan,
                      data = list (
                        T  = nSamples,
                        rep = rep,
                        y0 = y0[1:rep],
                        z  = z[,1:rep],
                        t0 = t0,
                        ts = ts
                      ),
                      seed = 123,
                      chains = 4,
                      iter = 1000,
                      warmup = 500
)

parametersToPlot = c("theta","sigma","lp__")
print(estimates, pars = parametersToPlot)

library(bayesplot)

draws <- as.array(estimates, pars=parametersToPlot)
mcmc_trace(draws)


color_scheme_set("brightblue")
mcmc_scatter(draws,pars=c('theta[1]','theta[2]'))

xdata <- data.frame(chl = unlist(z[,1:rep]),repl = as.vector(matrix(rep(1:rep,nSamples),nrow=nSamples,byrow=TRUE)),date1 = rep(ts,rep))
pred <- as.data.frame(estimates, pars = "z_pred") %>%
  gather(factor_key = TRUE) %>%
  group_by(key) %>%
  summarize(lb = quantile(value, probs = 0.05),
            median = quantile(value, probs = 0.5),
            ub = quantile(value, probs = 0.95)) %>%
  bind_cols(xdata)

p1 <- ggplot(pred, aes(x = date1, y = chl))
p1 <- p1 + geom_point() +
  labs(x = "time (day)", y = "chl") +
  theme(text = element_text(size = 12), axis.text = element_text(size = 12),
        legend.position = "none", strip.text = element_text(size = 8))
p1 + geom_line(aes(x = date1, y = median)) +
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.25) +
  facet_wrap(~factor(repl))