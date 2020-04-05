## juvenile daphnia feeding and excretion
source("../transfer_functions.R")
source("../Graphing_Set_Up.R")
library(tidyverse)

rdatj <- read.csv("Small_Daph_Feeding.csv")
dim(rdatj)

cont <- rdatj %>% 
  filter(Control.Y.N == "Y") %>% 
  mutate(chl_diff =((Chl.1-Chl.2)/Chl_Time_Diff)*1440, nh4_diff= ((Nh4.2-Nh4.1)/Nh4_Time_Dif)*1440) %>%  
  group_by(Treatment) %>%
  summarize(mean_chl = mean(chl_diff, na.rm = T), mean_nh4 = mean(nh4_diff,na.rm = T)) ## onr row in treatment 3 is all NAs..??
dim(cont)

## add mean control avg back to main dataframe
dat <- left_join(rdatj,cont)
dim(dat)
## account for controls

dat <- dat %>%
  mutate(chl_diff = ((Chl.1-Chl.2)/Chl_Time_Diff)*1440, nh4_diff = ((Nh4.2-Nh4.1)/Nh4_Time_Dif)*1440) %>%
  mutate(chl_diff_cc = (chl_diff-mean_chl)/Num_Daphnia, nh4_diff_cc = (nh4_diff-mean_nh4)/Num_Daphnia)  ## need to add in diff from control bc if positive algae grew so indiv actually ate more if neg indiv ate less

dat1 <- dat %>% filter(Control.Y.N == "N") %>% filter(!is.na(chl_diff_cc)) %>% 
  filter(!is.na(nh4_diff_cc)) %>% filter(nh4_diff_cc > -5) %>% ## remove one weird measurement
  select(Rep.., Treatment,Chl.1,Nh4.1, chl_diff_cc,nh4_diff_cc, Num_Daphnia)

#ggplot(dat1, aes(Chl.1,chl_diff_cc)) + geom_point()

mod_lm <- lm(data = dat1, chl_diff_cc ~ -1+Chl.1)


#newpred <- sat_fun(k= seq(1,100,1), a=960892 ,b =1339121459)

newdata = data.frame(Chl.1 = seq(1,25,0.1))
newpred1 <- as.data.frame(predict(mod_lm, newdata = newdata, interval = "confidence"))
newdata$chl_diff_cc <- newpred1$fit
newdata$lwr <- newpred1$lwr
newdata$upr <- newpred1$upr

j_feed_g <- ggplot(data = dat1, aes(Chl.1, chl_diff_cc)) + geom_point() +
  geom_line(data = newdata) + 
  geom_ribbon(data = newdata, aes(ymin = lwr, ymax= upr),alpha = 0.3) +
  xlab("Chlorphyll a (ug/L)") + 
  ylab(str_wrap("Change in Chlorophyll a/Juvenile Daphnia/Day", width = 25)) +
  ggtitle("LS: Linear Fit")

print(j_feed_g)

## fit in stan
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



daph_grow_list <- list(
  "N" = nrow(dat1),
  "chl" = dat1$Chl.1,
  "diff" = dat1$chl_diff_cc
)
fit <- stan(file = "adult_feeding.stan", 
            data = daph_grow_list)

library(shinystan)
launch_shinystan(fit)
saveRDS(fit, file = "juv_feeding.RDS")

fit_sum <- summary(fit)
(fit_sum_param <- fit_sum$summary[c(1:4),])

t <- rstan::extract(fit,permuted = FALSE)
m_pred <- rbind(t[,1,1],t[,2,1],t[,3,1],t[,4,1]) 
b_pred <- rbind(t[,1,2],t[,2,2],t[,3,2],t[,4,2]) 

newdat <- data.frame(chl1 = seq(0,25))

pred_out <- apply(newdat,1,lin,m= m_pred, b=b_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))



## ammonium excretion

lm(data=dat1[dat1$nh4_diff_cc < 0 ,], nh4_diff_cc/chl_diff_cc ~ 1)


ggplot(dat1, aes(Nh4.1,nh4_diff_cc/Chl.1)) + geom_point()+geom_smooth(method = "lm")
mod_nh4 <- lm(data = dat1, nh4_diff_cc ~ Nh4.1)
mod_int_only <- lm(data = dat1, nh4_diff_cc ~1)
pred <- predict(mod_int_only)
with(dat1, plot(Nh4.1,nh4_diff_cc))
lines(pred)

library(nlmrt)
mod_sat <- nlxb(data = dat1, nh4_diff_cc ~ (Nh4.1*a)/(Nh4.1+b), start = list(a=1,b=1))

##newpred <- sat_fun(k= seq(5,25,.1), a= -0.00161455 ,b =15.609)


newdata = data.frame(Nh4.1 = seq(5,25,0.1))
newpred2 <- predict(mod_nh4, newdata = newdata)
newpred3 <- predict(mod_int_only, newdata = newdata)
plot(dat1$Nh4.1,dat1$nh4_diff_cc)
points(seq(5,25,0.1), newpred2)

points(seq(5,25,0.1), newpred3)
points(seq(5,25,0.1), newpred)

### fit in stan

daph_excretion_list <- list(
  "N" = nrow(dat1),
  ##"nh4" = dat1$Nh4.1,
  "diff" = dat1$nh4_diff_cc
)


fit <- stan(file = "intercept_only_excretion.stan", 
            data = daph_excretion_list, chains = 4, control = list(adapt_delta=0.9), iter = 5000 )

launch_shinystan(fit)

saveRDS(fit, file = "juv_exec.RDS")

fit_sum <- summary(fit)
(fit_sum_param <- fit_sum$summary[c(1:4),])

t <- rstan::extract(fit,permuted = FALSE)
b_pred <- rbind(t[,1,1],t[,2,1],t[,3,1],t[,4,1]) 
 

newdat <- data.frame(Nh4.1 = seq(0,25))

pred_out <- apply(newdat,1,lin, b=b_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))


with(dat1, plot(Nh4.1, nh4_diff_cc))
lines(seq(0,25), rep(median(b_pred[,1]), 26))
lines(seq(0,25), rep(quantile(b_pred[,1],c(0.025)),26))
lines(seq(0,25), rep(quantile(b_pred[,1],c(0.975)),26))



daph_excretion_list <- list(
  "N" = nrow(dat1),
  ##"nh4" = dat1$Nh4.1,
  "diff" = dat1$nh4_diff_cc
)


fit <- stan(file = "intercept_only_excretion.stan", 
            data = daph_excretion_list, chains = 4, control = list(adapt_delta=0.9), iter = 5000 )

launch_shinystan(fit)

saveRDS(fit, file = "juv_exec.RDS")


## excretion update
dat2 <- dat1 %>% filter(Treatment > 4)

daph_excretion_list <- list(
  "N" = nrow(dat2),
  "chl" = dat2$chl_diff_cc,
  "diff" = dat2$nh4_diff_cc
)


fit <- stan(file = "adult_excretion_update.stan", 
            data = daph_excretion_list, chains = 4, control = list(adapt_delta=0.9), iter = 5000 )

launch_shinystan(fit)
fit_sum <- summary(fit)
(fit_sum_param <- fit_sum$summary[c(1:4),])

t <- rstan::extract(fit,permuted = FALSE)
b_pred <- rbind(t[,1,1],t[,2,1],t[,3,1],t[,4,1]) 


saveRDS(fit, file = "juv_exec_update.RDS")

with(dat2, plot(chl_diff_cc, nh4_diff_cc))
lines(seq(0,25), rep(median(b_pred[,1]), 26))
lines(seq(0,25), rep(quantile(b_pred[,1],c(0.025)),26))
lines(seq(0,25), rep(quantile(b_pred[,1],c(0.975)),26))


