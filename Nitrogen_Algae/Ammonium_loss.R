library(tidyverse)

ammonium <- read.csv("Nh4_Air.csv")
#remove replicates that spilled (e.g NH4 == NA)

ammonium <- ammonium %>% drop_na()
#same early morning machine problem
ammonium <- ammonium %>%
  filter(Rep != 1 & Rep != 15 & Rep != 52) ## not sure about 52 but obvi outlier so check when measured
# create dataframe with change in NH4/day

ammonium <- ammonium %>% group_by(Treat) %>% mutate(med_val = median(NH4)) %>%
  mutate(weights = 1/(2/med_val)) ## 2 is 2 mg/L which is the obs error based on guide to YSI

amm  <- ammonium %>%
  arrange(Treat, Rep, Day) %>%
  group_by(Treat, Rep) %>%
  mutate(diff = NH4 / lag(NH4)) %>%
  ungroup %>%
  mutate(weights_scaled = weights/max(weights), center_NH4 = NH4 - mean(NH4)) 


with(amm, plot(Day, diff))

library(lme4)

## exploration of weights' size and intercept vs slope, would prefer just intercept for ODE 
lm(data = amm[amm$diff < 2 ,], diff ~ 1, weights = weights_scaled) 
lm(data = amm,weights = weights_scaled, diff ~ 1)
lmer(data = amm,weights = lag(NH4), diff ~ 1 + (1|Rep))
lmer(data = amm,weights = lag(NH4), diff ~ lag(center_NH4) + (1+lag(center_NH4)|Rep))
lmer(data = amm[amm$diff < 2 ,], diff ~ lag(center_NH4)+(1+lag(center_NH4)|Rep), weights = weights_scaled)


mod <- lm(data = amm, diff ~ lag(center_NH4), weights = weights_scaled) ## looked at cook's distance and found 1 rep day past 0.5 and 2 past 1
lmer(data = amm[-c(30,108,113),], diff ~ lag(center_NH4)+(1+lag(center_NH4)|Rep), weights = weights_scaled)

lmer(data = amm[-c(30,108,113),], diff ~ 1+(1|Rep), weights = weights_scaled) ## this seems like the model i want- intercept for constant prop loss



ggplot(data = amm, aes(lag(NH4), (diff))) + geom_point(aes(color= as.factor(Rep))) + geom_smooth(method = "lm") 

library(brms)
ff <- brm(diff|weights(weights_scaled) ~ 1+ (1|Rep), data = amm[-c(30,108,113),], family = gaussian(),
          control = list(adapt_delta = 0.95))
stancode(ff)
ff1 <- summary(ff)
launch_shinystan(ff)

saveRDS(ff, file = "ammonium.RDS")

samples <- posterior_samples(ff)

samplesb <- samples %>% select(b_Intercept)
mean(samplesb[,1])
samples_sigma <- samples %>% select(sigma)
mean(samples_sigma[,1])


with(amm[-c(30,108,113),], plot(NH4, diff))
lines(seq(0,25), rep(median(samplesb[,1]),26))
lines(seq(0,25), rep(quantile(samplesb[,1], c(0.025)),26))
lines(seq(0,25), rep(quantile(samplesb[,1], c(0.975)),26))





library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


amm <-amm[-c(30,108,113),]
amm <- amm %>% filter(!is.na(diff))

amm1 <- list(
  "N" = nrow(amm),
  "Y" = amm$diff,
  "weights" = amm$weights_scaled,
  "N_1" = length(unique(amm$Rep)),
  "M_1" = 1,
  "J_1" = amm$Rep,
  "Z_1_1" = rep(1,nrow(amm)),
  "prior_only" = 0
)


fit <- stan(file = "ammonium.stan", 
            data = amm1,iter = 2000)

mod <- stan_model(file = "ammonium.stan")
gg <- sampling(mod, data= amm1)

