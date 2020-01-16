### ammonium excretion 
ggplot(dat1, aes(nh41,nh4_diff_cc)) + geom_point()+geom_smooth(method = "lm")
ggplot(dat1, aes(chl_diff_cc,nh4_diff_cc)) + geom_point()+ geom_smooth(method = "lm")

lm(data = dat1, nh4_diff_cc ~1/chl_diff_cc)



mod_nh4 <- lm(data = dat1, nh4_diff_cc ~ nh41)
mod_int_only <- lm(data = dat1, nh4_diff_cc ~1)
mod_sat <- nlxb(data = dat1, nh4_diff_cc ~ (nh41*a)/(nh41+b), start = list(a=1,b=1))
mod_lm <- lm(data = dat1, nh4_diff_cc ~1/chl_diff_cc)

newpred <- sat_fun(k= seq(5,22,.1), a=1667 ,b =9905303)

newdata = data.frame(nh41 = seq(5,22,0.1))
newpred2 <- predict(mod_nh4, newdata = newdata)
newpred3 <- predict(mod_int_only, newdata = newdata)
plot(seq(5,22,0.1), newpred2)
points(dat1$nh41,dat1$nh4_diff_cc)
points(seq(5,22,0.1), newpred3)
points(seq(5,22,0.1), newpred)

newdata1 = data.frame(chl_diff_cc = seq(0,15,0.1))
newpred5 <- predict(mod_lm, newdata = newdata1)
plot(seq(0,10,0.1), newpred5)
plot(dat1$chl_diff_cc,dat1$nh4_diff_cc)
points(seq(0,15,0.1), newpred5)

## ok so actually I think that excretion has to be a function of the ingestion parameter a_feed_m

a_feed_m = fit_sum_param[1,6]

newmod <- nls(data = dat1, nh4_diff_cc ~ a_feed_m*chl1*m, start = list(m=1))
newdata2 = data.frame(chl1 = seq(0,110,1))
newpred6 <- predict(newmod, newdata = newdata2)
plot(seq(0,110,1), newpred6)
plot(dat1$chl1,dat1$nh4_diff_cc)
points(seq(0,110,1), newpred6)

## i think has to be 1/a_feed_m to get correct units....
a_feed_m1 <- 1/a_feed_m
newmod <- nls(data = dat1, nh4_diff_cc ~ a_feed_m1*chl1*m, start = list(m=1))
newdata2 = data.frame(chl1 = seq(0,110,1))
newpred6 <- predict(newmod, newdata = newdata2)
#plot(seq(0,110,1), newpred6)
plot(dat1$chl1,dat1$nh4_diff_cc)
points(seq(0,110,1), newpred6)


### fit in stan

daph_excretion_list <- list(
  "N" = nrow(dat1),
  "nh4" = dat1$nh41,
  "diff" = dat1$nh4_diff_cc
)


fit <- stan(file = "adult_excretion.stan", 
            data = daph_excretion_list)

launch_shinystan(fit)

fit_sum <- summary(fit)
(fit_sum_param <- fit_sum$summary[c(1:4),])

t <- rstan::extract(fit,permuted = FALSE)
m_pred <- rbind(t[,1,1],t[,2,1],t[,3,1],t[,4,1]) 
b_pred <- rbind(t[,1,2],t[,2,2],t[,3,2],t[,4,2]) 

newdat <- data.frame(nh41 = seq(0,20))

pred_out <- apply(newdat,1,lin,m= m_pred, b=b_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

with(dat1, plot(nh41, nh4_diff_cc))
lines(seq(0,20), pred_sum[1,])
lines(seq(0,20), pred_sum[2,])
lines(seq(0,20), pred_sum[3,])

saveRDS(fit, file = "adult_exc.RDS")
fit2 <- readRDS("adult_exc.RDS")



daph_excretion_list <- list(
  "N" = nrow(dat1),
  "chl" = dat1$chl_diff_cc,
  "diff" = dat1$nh4_diff_cc
)


fit <- stan(file = "adult_excretion_update.stan", 
            data = daph_excretion_list)

launch_shinystan(fit)

fit_sum <- summary(fit)
(fit_sum_param <- fit_sum$summary[c(1:4),])

t <- rstan::extract(fit,permuted = FALSE)
m_pred <- rbind(t[,1,1],t[,2,1],t[,3,1],t[,4,1]) 
b_pred <- rbind(t[,1,2],t[,2,2],t[,3,2],t[,4,2]) 

newdat <- data.frame(chl1 = seq(0,100))

pred_out <- apply(newdat,1,lin,m= m_pred, b=b_pred)
pred_sum <- apply(pred_out, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))

with(dat1, plot(chl_diff_cc, nh4_diff_cc))
lines(seq(0,20), pred_sum[1,])
lines(seq(0,20), pred_sum[2,])
lines(seq(0,20), pred_sum[3,])

saveRDS(fit, file = "adult_exc_new.RDS")
fit2 <- readRDS("adult_exc_new.RDS")