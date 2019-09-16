## simulate data
#devtools::install_github("parksw3/fitode")
library(fitode)
cammonium = (1-9.4235e-01)
chl_nh4_mod <- odemodel(
  name = "algal_nit",
  model = list(
    pred_nh4 ~ -pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) + gamma *(death1*pred_chl + death2*(pred_chl^2))-cammonium*pred_nh4 ,
    pred_chl ~ beta * pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) - death1*pred_chl - death2*(pred_chl^2)
  ),
  ## consider using bbmle::dnorm_n ?
  observation = list(
    nh4 ~ dnorm2(mean = pred_nh4),
    chl ~ dnorm2(mean = pred_chl)
  ),
  initial = list(pred_nh4 ~ pred_nh40 , pred_chl ~ pred_chl0),
  par=c("alpha", "beta", "omega", "death1","death2", "pred_nh40", "pred_chl0", "gamma")
)

## maybe figure out initial values
start <- c(alpha = 0.03, 
           beta = 15,
           omega=2.3,
           death1=0.006,
           death2=0.001,
           pred_nh40 = 15 ,
           pred_chl0 = 40, 
           gamma=0.01
)

ss <- ode.solve(chl_nh4_mod, 1:100, start,
                solver.opts=list(method="rk4", hini=0.1))


plot(ss@solution$pred_nh4)
plot(ss@solution$pred_chl)

simdat <- data.frame( 
nh4 = rnorm(ss@solution$pred_nh4, 0.5),
chl = rnorm(ss@solution$pred_chl,1),
date = seq(1,100,1))

fit <- fitode(
  chl_nh4_mod,
  data = simdat, 
  start=start,
  tcol = "date",
  solver.opts=list(method="rk4", hini=0.1)
)
plot(fit, level=0.95)
## difficult to fit/ recover 

##### try simpler model
chl_nh4_mod2 <- odemodel(
  name = "algal_nit",
  model = list(
    pred_nh4 ~ -a*pred_chl*(pred_nh4/(k+pred_nh4))+r*death*pred_chl,
    pred_chl ~  a*pred_chl*(pred_nh4/(k+pred_nh4)) - death*pred_chl  
  ),
  
  observation = list(
    nh4 ~ dlnorm(meanlog =log(pred_nh4), sdlog = 0.2),
    chl ~ dlnorm(meanlog = log(pred_chl),sdlog = 0.1)
  ),
  initial = list(pred_nh4 ~ pred_nh40 , pred_chl ~ pred_chl0),
  par=c("a","k", "r","death", "pred_nh40", "pred_chl0")
)

start <- c(a = 0.03, 
          k = .03,
          r = 1,
          death = 0.02,
          pred_nh40= 15,
          pred_chl0 = 40
          )




s1 <- ode.solve(chl_nh4_mod2, 1:40, start,
                solver.opts=list(method="rk4", hini=0.1))

s1B <- ode.solve(chl_nh4_mod2, 1:40, start2,
                solver.opts=list(method="rk4", hini=0.1))

s2 <-  simulate(chl_nh4_mod2,nsim = 5, parms= start, times = seq(1,40),
                solver.opts=list(method="rk4", hini=0.1))


plot(s1@solution$time,s1@solution$pred_chl)
plot(s1@solution$time,s1@solution$pred_nh4)

with(s1@solution, plot(pred_chl, pred_nh4, type="b", xlim=c(40,60),
                       ylim=c(0,20)))
with(s1B@solution, plot(pred_chl, pred_nh4, type="b", col=2))
matplot(s1B@solution[,-1],log="y")
with(s1B@solution, lines(pred_chl, pred_nh4, type="b", col=2))


plot(s2$times, s2$chl)
plot(s2$times, s2$nh4)

simdat2 <- data.frame( 
  nh4 = rnorm(s1@solution$pred_nh4, 0.5),
  chl = rnorm(s1@solution$pred_chl,1),
  date = seq(1,40,1))



fit <- fitode(
  chl_nh4_mod2,
  data = s2, 
  start=start,
  tcol = "times",
  solver.opts=list(method="rk4", hini=0.1)
)
plot(fit, level=0.95)
coef(fit)



fit_try <-  simulate(chl_nh4_mod2,nsim = 5, parms= coef(fit), times = seq(1,40),
                solver.opts=list(method="rk4", hini=0.1))

with(fit_try, plot(times, pred_chl))
with(fit_try, plot(times, pred_nh4))
## good recovery of params try with fewer days

s2 <-  simulate(chl_nh4_mod2,nsim = 5, parms= start, times = seq(1,11),
                solver.opts=list(method="rk4", hini=0.1))



plot(s2$times, s2$chl)
plot(s2$times, s2$nh4)





fit <- fitode(
  chl_nh4_mod2,
  data = s2, 
  start=start,
  tcol = "times",
  solver.opts=list(method="rk4", hini=0.1)
)
plot(fit, level=0.95)
coef(fit)
confint(fit)

## good coverage

## try with off starting values.. does it converge

start2 <- c(a = 0.05, 
           k = .1,
           r = 1,
           death = 0.002,
           pred_nh40= 15,
           pred_chl0 = 40
) 

fit2 <- fitode(
  chl_nh4_mod2,
  data = s2, 
  start=start2,
  tcol = "times",
  solver.opts=list(method="rk4", hini=0.1)
)

plot(fit2, level=0.95)
coef(fit2)
confint(fit2)


## try to add ammonium loss due to nitrification

## try this parameterization
##' @param x concentration
##' @param a asymptote
##' @param i initial slope
micmen2 <- function(x,a,i) {
    a*x/((a/i) + x)
}

chl_nh4_mod3 <- odemodel(
  name = "algal_nit",
  model = list(
      pred_nh4 ~ -a*pred_chl*(pred_nh4/(k+pred_nh4))+r*death*pred_chl-cammonium,
      pred_chl ~  a*pred_chl*(pred_nh4/(k+pred_nh4)) - death*pred_chl
  ),
  observation = list(
    nh4 ~ dlnorm(meanlog = log(pred_nh4), sdlog = 0.05),
    chl ~ dlnorm(meanlog = log(pred_chl), sdlog = 0.01)
  ),
  initial = list(pred_nh4 ~ pred_nh40 , pred_chl ~ pred_chl0),
  par=c("a","k", "r","death", "pred_nh40", "pred_chl0")
)

start3 <- c(a = 0.03, 
            k = .03,
            r = 1,
            death = 0.02,
            pred_nh40= 15,
            pred_chl0 = 40
)
s3 <-  simulate(chl_nh4_mod3,nsim = 5, parms= start3, times = seq(1,40),             
                solver.opts=list(method="rk4", hini=0.1))



plot(s3$times, s3$chl)
plot(s3$times, s3$nh4)


fit3 <- fitode(
  chl_nh4_mod3,
  data = s3, 
  start=start3,
  tcol = "times",
  solver.opts=list(method="rk4", hini=0.1)
)

start4 <- c(a = 0.01, 
            k = .1,
            r = 1,
            death = 0.002,
            pred_nh40= 15,
            pred_chl0 = 40
)

fit4 <- fitode(
  chl_nh4_mod3,
  data = s3, 
  start=start4,
  tcol = "times",
  solver.opts=list(method="rk4", hini=0.1)
)



#### make dataframe of different start values see if converges on true parameters

library("lhs")
set.seed(100)
hc <- improvedLHS(300, 6)
hc <- data.frame(hc)

hc[,1] <- .01 +0.5*hc[,1]
hc[,2] <- .01 +0.5*hc[,2]
hc[,3] <- 0.1 + 1*hc[,3]
hc[,4] <- 0.001 + 0.05*hc[,4]
hc[,5] <- 5 + 25*hc[,5]
hc[,6] <- 30 + 25*hc[,6]

names(hc) <- c("a","k","r","death","pred_nh40","pred_chl0")


st <- expand.grid(
  a = seq(0.01,0.5,0.04),
  k = seq(0.01,0.5,0.04),
  r = seq(0.01,1,0.08),
  death = seq(0.001,0.05,0.004),
  pred_nh40 = 15,
  pred_chl0 = 40
)

st2 <- data.frame(
  a = seq(0.01,0.5,0.1),
  k = seq(0.01,0.5,0.1),
  r = seq(0.01,1,0.01),
  death = seq(0.001,0.05,0.01),
  pred_nh40 = 15,
  pred_chl0 = 40)

temp <- data.frame (
  a =  0,
  k = 0,
  r = 0,
  death = 0,
  pred_nh40 = 15,
  pred_chl0 = 40,
  lowa = 0,
  lowk = 0,
  lowr = 0,
  lowdeath = 0,
  lownh4 = 0,
  lowchl = 0,
  higha = 0,
  highk = 0,
  highr = 0,
  highdeath = 0,
  highnh4 = 0,
  highchl = 0,
  loglik = 0,
  ttime  = 0
)

temp_models <- list()

## stopped at 196
for(i in 196:nrow(st)) {
  newstart <- with(st[i,], 
                   list(
                     a =  a,
                     k = k,
                     r=r,
                     death = death,
                     pred_nh40 = pred_nh40,
                     pred_chl0 = pred_chl0
                    
                     
                   ))
  x <- system.time({
    tempm <- try((fitode(chl_nh4_mod3, data= s3, start = newstart, tcol = "times", 
                         solver.opts = list(method="rk4", hini=0.1))),silent = TRUE)
    temp_models <- c(temp_models,list(tempm))
    if (class(tempm) == "try-error") {
        temp[i,] <- "NA"
    } else {
      temp[i,1:6] <- coef(tempm)
      temp[i,7:12] <- confint(tempm)[,2]
      temp[i,13:18] <- confint(tempm)[,3]
      temp$loglik[i] <- logLik(tempm)
      
    }})
  temp$ttime[i] <- x[[3]]
  print(i/nrow(st))
}



dat <- temp %>% filter(a != "NA")
write.csv(dat, file = "filtered_sim.csv")
write.csv(temp, file = "unfiltered_sim.csv")
nrow(temp) ## 196
nrow(dat)  ## 11

dat2 <- read.csv("filtered_sim.csv")
dat2B <- dat2[1:10,] ## fit 11 is crazy
op <- par(mfrow=c(1,2))
library(corrplot)
corrplot.mixed(cov2cor(vcov(fit3)),lower="ellipse",upper="number")
dev.new()
pairs(dat2B[,2:7],gap=0)
par(op)
### repeat as above but have different parameters














### try with real data


chl_fit_27_dd <- fitode(
  chl_nh4_mod3,
  data = dat_nit_27, 
  start=start,
  tcol = "date1",
  solver.opts=list(method="rk4", hini=0.1)
)
plot(chl_fit_27_dd, level=0.95)
coef(fit)

newdat <- simulate(chl_nh4_mod3,nsim = 5, parms= coef(chl_fit_27_dd), times = seq(1,11),             
           solver.opts=list(method="rk4", hini=0.1))
newdat$date1 <- newdat$times

ggplot(dat_nit_27, aes(date1,chl)) + geom_point()+ geom_line(data = newdat)

ggplot(dat_nit_27, aes(date1,nh4)) + geom_point()+ geom_line(data = newdat) 




