##daily fecundity

library(tidyverse)
source("../transfer_functions.R")
source("../chl_adj.R")
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

## data from literature
fec_lit <- read.csv("fec_lit.csv")

fec_lit$cell <- c(NA,1e+09, NA, NA, 1e+08,5e+05, 166666.7, NA, 5e+05, NA,NA, NA)
fec_lit$sd <- fec_lit$sd_repro
fec_lit$daily_fec <- fec_lit$daphnia_reproduction
fec_lit$rep <- as.factor(rep("A", nrow(fec_lit)))

fec_lit <- fec_lit %>% filter(!is.na(cell))

ndat <- daph_fec_adj %>% select("cell","sd","daily_fec", "rep")
ndat1 <- fec_lit %>% select("cell", "sd", "daily_fec", "rep")

p <- as.data.frame(rbind(as.matrix(ndat), as.matrix(ndat1)))
#p1 <- p %>% filter(rep == 1)
# names(p1) <- c("rep","cell","se","dailyfec")


library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



daph_fec_list <- list(
  "N" = 70,
  "chl" = daph_fec_adj$cell,
  "daily_fec" = daph_fec_adj$daily_fec,
  "L" = 5,
  "daily_fec_lit" = fec_lit$daily_fec,
  "sd_lit" = fec_lit$sd_repro
)

##
fit <- stan(file = "fec_prior.stan", 
            data = daph_fec_list ) #,
            #control = list(adapt_delta = 0.99, max_treedepth = 17),
            #iter = 5000)  
library(shinystan)
launch_shinystan(fit)









## fit with weights by se
library(brms)
ff <- brm(bf(dailyfec|weights(se) ~  alpha * cell / (cell + beta), data = p1, family = lognormal(),
          control = list(adapt_delta = 0.95),  prior = c(
              prior(normal(0.0, 1000), nlpar = "alpha"),
              prior(normal(0.0, 1000), nlpar = "beta") 
              )
))



ff <- brm(bf(dailyfec ~  alpha * cell / (cell + beta), data = p1, family = lognormal(),
             control = list(adapt_delta = 0.95),  prior = c(
               prior(normal(0.0, 1000), nlpar = "alpha"),
               prior(normal(0.0, 1000), nlpar = "beta") 
             )
))

 ## fit without weights
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



daph_fec_r <- daph_fec %>% dplyr::select(rep, chl, daily_fec)
daph_fec_r$rep <- seq(1,70)
daph_fec_r <- data.frame(daph_fec_r)

daph_fec_list <- list(
   "N" = 70,
   "chl" = daph_fec_r$chl,
   "daily_fec" = daph_fec_r$daily_fec
)

fit <- stan(file = "fec_stan.stan", 
            data = daph_fec_list,
            control = list(adapt_delta = 0.95))

saveRDS(fit, file = "fec_stan_prior.rds")

t <- rstan::extract(fit,permuted = FALSE)
fit_sum <- summary(fit)
print(names(fit_sum))
print(fit_sum$summary)
fit_sum_param <- fit_sum$summary[c(1:4),]

#tidy_pred <- tidybayes::add_fitted_draws(fit, newdata = data.frame(chl = seq(1,75)))

## right now only 2 chain want all 4 eventually 
a_pred <- rbind(t[,1,1],t[,2,1], t[,3,1], t[,4,1]) ## all rows, first chain, alpha
b_pred <- rbind(t[,1,2], t[,2,2], t[,3,2], t[,4,2])

newdat <- data.frame(chl = seq(0,100))

pred_out <- apply(newdat,1,sat_fun,a=a_pred,b=b_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

with(daph_fec_r, plot(chl, daily_fec))
lines(seq(0,100), pred_sum[1,])
lines(seq(0,100), pred_sum[2,])
lines(seq(0,100), pred_sum[3,])

hist(t[,1,4], breaks = 200)
lower <- data.frame(chl = seq(0,100), daily_fec = pred_sum[1,])
upper <- data.frame(chl = seq(0,100), daily_fec = pred_sum[3,])
med <- data.frame(chl = seq(0,100), daily_fec = pred_sum[2,])

ggplot(daph_fec_r, aes(chl, daily_fec)) + geom_point(alpha = 0.6, size = 2 )+
  geom_line(data = lower, linetype = "dotdash", lwd = 1.25) + geom_line(data = upper, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = med, linetype = "solid", lwd =1.25) + xlab("Daily Chl a ug/L") + ylab("Daily Fecundity")+
  theme_bw() + theme(axis.text.x = element_text(size = 30),
                     axis.text.y = element_text(size = 32),
                     axis.title.x = element_text(size = 30),
                     axis.title.y = element_text(size = 32),
                     strip.text = element_text(size = 0),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     strip.background = element_blank()) 
