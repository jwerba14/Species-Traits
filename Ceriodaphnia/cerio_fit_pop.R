## transform cerio data to estimate difference per day

cerio <- read.csv("cerio_pop.csv")
source("../transfer_functions.R")
cerio$week <- as.numeric(0)
## create column of weeks
for (i in 1:nrow(cerio)) {
 if (cerio$Day[i] >=1 & cerio$Day[i] < 8) {
  cerio$week[i] <- 1 
} else if ( cerio$Day[i] >=8 & cerio$Day[i] < 15) {
  cerio$week[i] <- 2 
} else if ( cerio$Day[i] >=15 & cerio$Day[i] < 22) {
  cerio$week[i] <- 3 
} else if ( cerio$Day[i] >=22 & cerio$Day[i] < 29) {
  cerio$week[i] <- 4 
} else if ( cerio$Day[i] >=29 & cerio$Day[i] < 36) {
  cerio$week[i] <- 5 
} else if ( cerio$Day[i] >=36 & cerio$Day[i] < 43) {
  cerio$week[i] <- 6    
} else if ( cerio$Day[i] >=43 & cerio$Day[i] < 51) {
  cerio$week[i] <- 7    
}
}

## need avg chl by week by rep but first need to not include duplicated rows (due to multiple pop count)
avg <- cerio[!duplicated(cerio[, 11]),]
avg <- avg %>% 
  group_by(Treatment,Rep, week) %>%
  mutate(week_chl = mean(Avg_Chl), week_chl_sd = sd(Avg_Chl))
avg <- avg %>% select(Treatment,Rep, week, week_chl,week_chl_sd)

full <- left_join(cerio,avg)
full <- full %>% select(Treatment,Rep, week, week_chl, week_chl_sd, Population, Day)
## get rid of days where I didn't count 
full <- full %>% filter(Population != "NA")

full$percapita <- full$week_chl/ full$Population

## need dataframe with change in pop, 
new <- full %>% group_by(Treatment, Rep,week) %>%
  mutate(avg_pop = mean(Population)) 
## remove duplicates
new1 <- new[!duplicated(new[, c(1,2,3)]),]
## create r 
final <- new1 %>%
  arrange(Treatment, Rep, week) %>%
  group_by(Treatment, Rep) %>%
  mutate(diff = avg_pop / lag(avg_pop, default = first(avg_pop)), percapita = week_chl/avg_pop) %>%
  mutate(loglagper = log(lag(percapita))) %>%
  filter(week != 1)


ggplot(final, aes(loglagper, diff)) + geom_point(lwd = 3, aes(color = as.factor(Treatment))) + geom_smooth(method = "lm")

lm(data = final, diff ~ loglagper)

final <- final %>% filter(!is.na(diff)) %>% ungroup() %>% select(loglagper,diff) ## removes one row for which there isn't data

## fit in stan
ceriodat <- list(
  "N" = 143,
  "chl" = final$loglagper,
  "pop" = final$diff
  )

fit <- stan(file = "cerio_pop.stan", 
            data = ceriodat)

saveRDS(fit, file = "cerio_pop.rds")

library(shinystan)

launch_shinystan(fit)

t <- rstan::extract(fit,permuted = FALSE)
fit_sum <- summary(fit)
print(names(fit_sum))
(fit_sum_param <- fit_sum$summary[c(1:4),])

#tidy_pred <- tidybayes::add_fitted_draws(fit, newdata = data.frame(chl = seq(1,75)))

## right now only 2 chain want all 4 eventually 
m_pred <- rbind(t[,1,1],t[,2,1], t[,3,1], t[,4,1]) ## all rows, first chain, alpha
b_pred <- rbind(t[,1,2], t[,2,2], t[,3,2], t[,4,2])

newdat <- data.frame(chl = seq(-3,10))

pred_out <- apply(newdat,1,lin,m=m_pred,b=b_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

with(final, plot((loglagper), diff))
lines(seq(-3,10), pred_sum[1,])
lines(seq(-3,10), pred_sum[2,])
lines(seq(-3,10), pred_sum[3,])


