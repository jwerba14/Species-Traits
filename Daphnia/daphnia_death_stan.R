## fit exponential death with stan 
library(tidyverse)

daph <- read.csv("daphnia_lifetime.csv")
# filter out individuals that were NOT born in the conditions (original daphnia)
daph <- daph %>%
  filter(adult_only=="N")

dA <-  daph %>%
  filter(size_class == "A")

daph_adult_death <-  dA %>%
  group_by(rep,treatment) %>%
  summarize(days_adult = n(),
            chl = mean(chl_avg),
            chl_sd_rep = sd(chl_avg)) 

survcurve <- function(x) {
  x <- c(0,sort(x))
  tibble(day=x,frac_surv=seq(1,0,length.out=length(x)))
}

daph_surv_curves <- daph_adult_death %>%
  group_by(treatment) %>%
  do(survcurve(.$days_adult))

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

daph_death_list <- list(
  "N" = 74,
  "days" = daph_surv_curves$day,
  "survival" = daph_surv_curves$frac_surv
)

fit <- stan(file = "adult_death.stan", 
            data = daph_death_list,
            control = list(adapt_delta = 0.95))
