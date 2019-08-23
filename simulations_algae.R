## simulate data
library(fitode)
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


##### try simpler model
chl_nh4_mod2 <- odemodel(
  name = "algal_nit",
  model = list(
    pred_nh4 ~ -a*pred_chl*(pred_nh4/(k+pred_nh4))+r*death*pred_chl,
    pred_chl ~  a*pred_chl*(pred_nh4/(k+pred_nh4)) - death*pred_chl  ## but recy < or = to death
  ),
  ## consider using bbmle::dnorm_n ?
  observation = list(
    nh4 ~ dnorm(mean =pred_nh4, sd = 0.5),
    chl ~ dnorm(mean = pred_chl,sd = 1)
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

## FIXME: 'transform' methods?
start2 <- start
start2[["pred_chl0"]] <- 60
start2[["pred_nh40"]] <- 0.01


s1 <- ode.solve(chl_nh4_mod2, 1:40, start,
                solver.opts=list(method="rk4", hini=0.1))

s1B <- ode.solve(chl_nh4_mod2, 1:40, start2,
                solver.opts=list(method="rk4", hini=0.1))

s2 <-  simulate(chl_nh4_mod2,nsim = 5, parms= start, times = seq(1,40),
                solver.opts=list(method="rk4", hini=0.1))


plot(s1@solution$time,s1@solution$pred_chl)
plot(s1@solution$time,s1@solution$pred_nh4,log="y")

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

## real data growth was much higher and had more noise

chl_nh4_mod2 <- odemodel(
  name = "algal_nit",
  model = list(
    pred_nh4 ~ -a*pred_chl*(pred_nh4/(k+pred_nh4))+r*death*pred_chl,
    pred_chl ~ a*pred_chl*(pred_nh4/(k+pred_nh4)) - death*pred_chl  ## but recy < or = to death
  ),
  ## consider using bbmle::dnorm_n ?
  observation = list(
    nh4 ~ dlnorm(meanlog = log(pred_nh4), sdlog = 0.05),
    chl ~ dlnorm(meanlog = log(pred_chl), sdlog = 0.01)
  ),
  initial = list(pred_nh4 ~ pred_nh40 , pred_chl ~ pred_chl0),
  par=c("a","k", "r","death", "pred_nh40", "pred_chl0")
)


start <- c(a = .05, 
           k = .09,
           r = .1,
           death = 0.02,
           pred_nh40= 15,
           pred_chl0 = 40
)
s3 <-  simulate(chl_nh4_mod2,nsim = 5, parms= start, times = seq(1,11),             
                solver.opts=list(method="rk4", hini=0.1))



plot(s3$times, s3$chl)
plot(s3$times, s3$nh4)


chl_fit_27_dd <- fitode(
  chl_nh4_mod2,
  data = dat_nit_27, 
  start=start,
  tcol = "date1",
  solver.opts=list(method="rk4", hini=0.1)
)
plot(chl_fit_27_dd, level=0.95)
coef(fit)


