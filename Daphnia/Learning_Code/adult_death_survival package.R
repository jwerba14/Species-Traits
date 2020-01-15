library(tidyverse)
library(survminer)
library(survival)
daph <- read.csv("daphnia_lifetime.csv")
daph <- daph %>%
  filter(adult_only=="N")

dA <-  daph %>%
  filter(size_class == "A")

daph_adult_death <-  dA %>%
  group_by(rep,treatment) %>%
  summarize(days_adult = n(),
            chl = mean(chl_avg),
            chl_sd_rep = sd(chl_avg)) 
daph_adult_death$censor <- as.numeric(1)
left_join(daph_adult_death, dA)

## with surviminer
## kaplan meier-- only can take by treatment not actual measured chl
surv_obj <- Surv(time = daph_adult_death$days_adult, event = daph_adult_death$censor)
surv <- survfit(surv_obj ~ daph_adult_death$treatment)
ggsurvplot(surv, data = daph_adult_death, pval = TRUE)

ggsurvplot(surv, data = daph_adult_death,
           conf.int = TRUE,
           break.time.by = 20,
           risk.table = TRUE)

## cox proportional hazards- can take continuous variable gives out risk of death at time t
##in the case of a continuous covariate {\displaystyle x} x,
##it is typically assumed that the hazard responds exponentially; 
##each unit increase in x results in proportional scaling of the hazard.
surv_obj <- Surv(time = daph_adult_death$days_adult, event = daph_adult_death$censor)
res.cox <- coxph(surv_obj ~ daph_adult_death$chl, data = daph_adult_death)
summary(res.cox)
res.graph <- survfit(res.cox)

ggsurvplot(res.graph,data = daph_adult_death, color = "#2E9FDF",
           ggtheme = theme_minimal())

## with stan
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

daph_adult_death1 <- daph_adult_death %>% filter(treatment == 25)


if (!require(biostan))
  devtools::install_github('jburos/biostan')
library(biostan)




