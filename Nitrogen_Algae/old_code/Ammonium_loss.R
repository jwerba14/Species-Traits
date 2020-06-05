library(tidyverse)
library(lme4)
library(brms)
library(fitdistrplus)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
source("../Graphing_Set_Up.R")

ammonium <- read.csv("Data/Nh4_Air.csv")
#remove replicates that spilled (e.g NH4 == NA)

ammonium <- ammonium %>% drop_na()

#same early morning machine problem
ammonium <- ammonium %>%
  filter(Rep != 1 & Rep != 15 & Rep != 52) ## not sure about 52 but obvi outlier so check when measured

# create dataframe with change in NH4/day
ammonium <- ammonium %>% group_by(Treat) %>% mutate(med_val = median(NH4)) %>%
  mutate(weights = 1/(2/med_val)) ## 2 is 2 mg/L which is the obs error based on guide to YSI-- why did I think
## this was the best way to weight?? confused

amm  <- ammonium %>%
  arrange(Treat, Rep, Day) %>%
  group_by(Treat, Rep) %>%
  mutate(diff = NH4 / lag(NH4)) %>%  ## change from day before
  ungroup %>%
  mutate(weights_scaled = weights/max(weights), center_NH4 = NH4 - mean(NH4)) 


with(amm, plot(Day, diff))



## exploration of weights' size and intercept vs slope, would prefer just intercept for ODE 
#lm(data = amm[amm$diff < 2 ,], diff ~ 1, weights = weights_scaled) 
#lm(data = amm,weights = weights_scaled, diff ~ 1)
#lmer(data = amm,weights = lag(NH4), diff ~ 1 + (1|Rep))
#lmer(data = amm,weights = lag(NH4), diff ~ lag(center_NH4) + (1+lag(center_NH4)|Rep))
#lmer(data = amm[amm$diff < 2 ,], diff ~ lag(center_NH4)+(1+lag(center_NH4)|Rep), weights = weights_scaled)


#mod <- lm(data = amm, diff ~ lag(center_NH4), weights = weights_scaled) 
## looked at cook's distance and found 1 rep day past 0.5 and 2 past 1
#lmer(data = amm[-c(30,108,113),], diff ~ lag(center_NH4)+(1+lag(center_NH4)|Rep), weights = weights_scaled)

#amm_mod1 <- lmer(data = amm[-c(30,108,113),], diff ~ 1+(1|Rep), weights = weights_scaled) ## this seems like the model i want- intercept for constant prop loss, bc diff is already prop
 ## hmm singular fit.... getting 0 for variance Rep
#amm_mod2 <- lmer(data = amm[-c(30,108,113),], diff ~ 1+(1|Rep))
##hmmm

amm2 <- amm %>% group_by(Rep) %>% count()
min(amm2$n)
amm3 <- left_join(amm, amm2)

amm3 <- amm3 %>% filter(n >= 5)
## remove outliers
amm4 <- amm3 %>% filter(Rep != 30 & Rep != 108 & Rep != 113)
amm_mod3 <- lmer(data = amm4, diff ~ 1+(1|Rep), weights = weights_scaled) ## fixed singular fit

am1s <- summary(amm_mod3)

pp <- data.frame(confint(amm_mod3))

# ci 
dat_nls_g <- ggplot(amm4, aes(lag(NH4), diff)) + geom_point(aes(color =as.factor(Treat))) + 
  geom_hline(yintercept = am1s$coefficients[1] ) + 
  geom_hline(yintercept = pp$X2.5..[3],linetype = "dotdash")+
  geom_hline(yintercept = pp$X97.5..[3], linetype = "dotdash")+
  ggtitle("NLS:Lab Data Only")

print(dat_nls_g)  ## it is dropping the reps of day 1 I assume...so no diff/lag


amm5 <- amm4 %>% filter(!is.na(diff))



## literature values

#lit <- read.csv("../literature_extraction.csv")

#litamm <- lit %>% 
#filter(!is.na(nitrification)) %>%
# dplyr::select(Title, Authors, nitrification,convert_mg_N_day, units.6,sd.2 )

#names(litamm) <- c("Title", "Author", "nitrification", "nit_day", "units", "sd")

#write.csv(litamm, file = "lit_amm.csv")

lita <- read.csv("Data/lit_amm.csv")
lit <- lita %>% filter(!is.na(nit_day))

dd <- fitdist(lit$nit_day, distr = "lnorm")



## bayesian model



if(!file.exists("RDS/ammonium.RDS")){
  
  prs <- prior(lognormal(-1.971344,1.764047), class='Intercept')  # priors from literature
  ff <- brm(diff|weights(weights_scaled) ~ 1+ (1|Rep), data =amm5, family = gaussian(),
            control = list(adapt_delta = 0.95), prior=prs)
  
  saveRDS(ff, file ="RDS/ammonium.RDS" )
} else {
  ff<- readRDS("RDS/ammonium.RDS")
}



#stancode(ff)

ff1 <- summary(ff)
launch_shinystan(ff)


samples <- posterior_samples(ff)

samplesb <- samples %>% dplyr::select(b_Intercept)

samplesbs <- samplesb %>%
  summarize(med = median(b_Intercept), lwr = quantile(b_Intercept, 0.025), upp = quantile(b_Intercept,0.975))



ammpl <- ggplot(amm4, aes(lag(NH4), diff)) +  geom_point(aes(color =as.factor(Treat))) + 
  geom_hline( yintercept = samplesbs$med) + 
  geom_hline( yintercept = samplesbs$lwr, linetype = "dotdash")+
  geom_hline( yintercept = samplesbs$upp, linetype = "dotdash")+
  ggtitle("Stan: Priors built from literature")


print(ammpl)






