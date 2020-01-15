##daily fecundity

library(tidyverse)
source("../transfer_functions.R")
source("../chl_adj.R")
source("../Graphing_Set_Up.R")
daph <- read.csv("daphnia_lifetime.csv")
daph <- daph %>%
  filter(adult_only=="N")

## to get fecundity parameter fit saturating curve (params z and w in full ode)
## need to make per day so need to divide total fecundity by # of days that individual was an adult
daph_fec <- daph %>% group_by(rep, treatment) %>%
  filter(size_class == "A") %>%
  summarize(
    time_adult = n()
    , life_fec   = sum(clutch_size, na.rm = TRUE)
    , chl        = mean(chl_avg)
    , chl_sd_rep = sd(chl_avg)) %>%
  mutate(daily_fec = life_fec / time_adult)


## make chl in cells per ml

daph_fec_adj <- daph_fec %>% 
  mutate(cell = chl_adj(chl = chl))

daph_fec_adj$sd <- 0
## remove 0s because those are dead individuals

daph_fec_adj <- daph_fec_adj %>% 
  filter(!(daily_fec == 0 & chl > 5) )


## data from literature
fec_lit <- read.csv("fec_lit.csv")

fec_lit$cell <- c(NA,1e+09, NA, NA, 1e+08,5e+05, 166666.7, NA, 5e+05, NA,NA, NA)
fec_lit$sd <- fec_lit$sd_repro
fec_lit$daily_fec <- fec_lit$daphnia_reproduction
fec_lit$rep <- as.factor(rep("A", nrow(fec_lit)))


fec_lit <- fec_lit %>% filter(!is.na(cell))
fec_lit<- fec_lit %>% 
  mutate(chl = cell_adj(cell = cell)) 

ggplot(daph_fec_adj, aes((cell), daily_fec)) + geom_point() + geom_point(data = fec_lit, color="blue") +
  geom_errorbar(data = fec_lit, aes(ymin=daily_fec-sd_repro, ymax=daily_fec+sd_repro)) +
  geom_errorbarh(data = fec_lit, aes(xmin=(cell-sd), xmax=(cell+sd))) +
  scale_x_log10()

ggplot(daph_fec_adj, aes((chl), daily_fec)) + geom_point() + geom_point(data = fec_lit, color="blue") +
  geom_errorbar(data = fec_lit, aes(ymin=daily_fec-sd_repro, ymax=daily_fec+sd_repro)) 
  


#ndat <- daph_fec_adj %>% select("cell","sd","daily_fec", "rep")
#ndat1 <- fec_lit %>% select("cell", "sd", "daily_fec", "rep")

#p <- as.data.frame(rbind(as.matrix(ndat), as.matrix(ndat1)))
#p1 <- p %>% filter(rep == 1)
# names(p1) <- c("rep","cell","se","dailyfec")


library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



daph_fec_list <- list(
  "N" = 64,
  "chl" = daph_fec_adj$cell,
  "daily_fec" = daph_fec_adj$daily_fec,
  "L" = 5,
  "daily_fec_lit" = fec_lit$daily_fec,
  "sd_lit" = fec_lit$sd_repro,
  "chl_lit" = fec_lit$cell
)

##
fit <- stan(file = "fec_prior.stan", 
            data = daph_fec_list,
            verbose = TRUE,iter = 5000, control = list(adapt_delta = 0.99, max_treedepth = 17)) 
 ## hmm no divergent transitions, runs pretty fast but mixing for sigma_beta not great, maybe we have no idea what beta is..
library(shinystan)
launch_shinystan(fit)


saveRDS(fit, file = "fec_mix_stan_prior2.rds")
rd <- readRDS(file = "RDS_Files/fec_mix_stan_prior2.rds") ## this rds is from previous iteration, b4 dropped 0s


t <- rstan::extract(fit,permuted = FALSE)
fit_sum <- summary(fit)
print(names(fit_sum))
print(fit_sum$summary)
fit_sum_param <- fit_sum$summary[c(1:4),]

#tidy_pred <- tidybayes::add_fitted_draws(fit, newdata = data.frame(chl = seq(1,75)))


a_pred <- rbind(t[,1,2],t[,2,2], t[,3,2], t[,4,2]) ## all rows, all chains alpha?
b_pred <- rbind(t[,1,4], t[,2,4], t[,3,4], t[,4,4])

newdat <- data.frame(cell = seq(26000000,1141832222, 10000000))

pred_out <- apply(newdat,1,sat_fun,a=a_pred,b=b_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

with(daph_fec_r, plot(chl, daily_fec))
lines(seq(0,100), pred_sum[1,])
lines(seq(0,100), pred_sum[2,])
lines(seq(0,100), pred_sum[3,])

hist(t[,1,4], breaks = 200)
lower <- data.frame(cell = seq(26000000,1141832222, 10000000), daily_fec = pred_sum[1,])
upper <- data.frame(cell = seq(26000000,1141832222, 10000000), daily_fec = pred_sum[3,])
med <- data.frame(cell = seq(26000000,1141832222, 10000000), daily_fec = pred_sum[2,])

ggplot(daph_fec_adj, aes((cell), daily_fec)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lower, linetype = "dotdash", lwd = 1.25) + geom_line(data = upper, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med, linetype = "solid", lwd =1.25) + xlab("Algal Cell Count") + ylab("Daily Fecundity")+
  theme_bw() + theme(axis.text.x = element_text(size = 30),
                     axis.text.y = element_text(size = 32),
                     axis.title.x = element_text(size = 30),
                     axis.title.y = element_text(size = 32),
                     strip.text = element_text(size = 0),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     strip.background = element_blank()) 


## wtf is going on with predictions


pracdat <- data.frame(chl=seq(0,25,0.5),
                      pred = 0)

pracdat$pred <- sat_fun(3.8,.198, pracdat$chl)
with(pracdat, plot(chl,pred))

with(daph_fec_adj, plot(chl, daily_fec))

## fit with nls
fec_param <- nls(daily_fec ~ sat_fun(z,w,chl), data = daph_fec_adj,
                 start = list(z=1,w=1))

pred_sum <- summary(fec_param)
coef_nls <- as.data.frame(pred_sum$coefficients)
## graph
newdat_nls <- data.frame(chl = seq(0,100),
                         daily_fec = numeric(length = 101),
                         upper = 0,
                         lower = 0)

chl <- data.frame(chl = seq(0,100))
newdat_nls$daily_fec<- apply(chl,1,sat_fun,a=coef_nls[1,1],b=coef_nls[2,1])
newdat_nls$upper<- apply(chl,1,sat_fun,a=coef_nls[1,1]+coef_nls[1,2],b=coef_nls[2,1]+coef_nls[2,2])
newdat_nls$lower<- apply(chl,1,sat_fun,a=coef_nls[1,1]-coef_nls[1,2],b=coef_nls[2,1]-coef_nls[2,2])



ggplot(data = daph_fec_adj, aes(chl, daily_fec)) + geom_point() + 
  geom_ribbon(data = newdat_nls, aes(ymax = upper, ymin=lower), alpha = 0.3) + 
  geom_line(data = newdat_nls) +
  ggtitle("Saturating Fit (NLS)") + xlab("Chlorophyll a (ug/L)") + ylab("Daily Fecundity")
 

## fit straight line (lm)
fec_param_lm <- lm(daily_fec~0 + chl, data = daph_fec_adj)

newdat_lm <- data.frame(chl = seq(0,100),
                         daily_fec = numeric(length = 101),
                         upper = 0,
                         lower = 0)

pred_lm <- as.data.frame(predict.lm(fec_param_lm, newdata = chl, interval = "confidence"))

newdat_lm$daily_fec<- pred_lm$fit
newdat_lm$upper<- pred_lm$upr
newdat_lm$lower <- pred_lm$lwr

ggplot(data = daph_fec_adj, aes(chl, daily_fec)) + geom_point() + 
  geom_line(data = newdat_lm) + geom_ribbon(data = newdat_lm, aes(ymax = upper, ymin = lower), alpha = 0.3)+ 
  ggtitle("Linear Fit (LS)") + xlab("Chlorophyll a (ug/L)") + ylab("Daily Fecundity")

## fit straight line (stan)
