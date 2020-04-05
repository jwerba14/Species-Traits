daph <- read.csv("daphnia_lifetime.csv")
dat <- read.csv("survival_literature.csv")

# filter out individuals that were NOT born in the conditions (original daphnia)
daph <- daph %>%
  filter(adult_only=="N")

dA <-  daph %>%
  filter(size_class == "A")

daph_adult_death <-  dA %>%
  group_by(rep, treatment) %>%
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
nrow(daph_surv_curves)


## add in chl
#ll <- daph_adult_death %>% dplyr::select(rep, chl)


#fdat <- left_join(daph_surv_curves,ll)
#nrow(fdat)