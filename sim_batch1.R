library(fitode)
set.seed(101)
max_param_set <- 10 ## number of parameter sets to try
llist <- lme4:::namedList

## cammonium = (1-9.4235e-01) ## measured ammonium loss in abiotic system
cammonium = 0
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

## simplify/switch to Gaussian
chl_nh4_mod3_G <- update(
    chl_nh4_mod3,
    observation = list(
        nh4 ~ dnorm(mean = pred_nh4, sd = 1),
        chl ~ dnorm(mean = pred_chl, sd = 1)
    ))
    
start3 <- c(a = 0.03, 
            k = .03,
            r = 1,
            death = 0.02,
            pred_nh40= 15,
            pred_chl0 = 40
            )
s3 <-  simulate(chl_nh4_mod3,nsim = 5, parms= start3, times = seq(1,40),             
                solver.opts=list(method="rk4", hini=0.1))


st <- expand.grid(
  a = seq(0.01,0.5,0.04),
  k = seq(0.01,0.5,0.04),
  r = seq(0.01,1,0.08),
  death = seq(0.001,0.05,0.004),
  pred_nh40 = 15,
  pred_chl0 = 40
)

m <- min(max_param_set,nrow(st))

params <- c("a","k","r","death","pred_nh40","pred_chl0")
## construct column names; t transposes; c flattens
cnames <- c(t(outer(c("","low","high"),params,paste0)),"loglik","ttime")
## set up columns of NAs
temp <- as.data.frame(replicate(length(cnames),rep(NA,m)))
names(temp) <- cnames

fitfun <- function(model, start,
                   solver.opts = list(method="rk4", hini=0.1))  {
    tt <- system.time({
    tempm <- try(fitode(model, data= s3, start = start, tcol = "times", 
                        solver.opts = solver.opts),
                 silent = TRUE)
    })
    attr(tempm,"time") <- tt
    return(tempm)
}

sumfun <- function(fit) {
    res <- rep(NA,ncol(temp))
    names(res) <- colnames(temp)
    if (!inherits(fit, "try-error")) {
        res[1:6] <- coef(fit)
        res[7:12] <- confint(fit)[,2]
        res[13:18] <- confint(fit)[,3]
        res["loglik"] <- logLik(fit)
    }
    res["ttime"] <- attr(fit,"time")[[3]]
    return(res)
}

temp_models <- list()
for(i in 1:m) {
  newstart <- with(st[i,], 
                   llist(a, k, r, death, pred_nh40, pred_chl0))
  ff <- fitfun(chl_nh4_mod3_G, newstart)
  temp[i,] <- sumfun(ff)
  cat(i,"/",m,"\n")
}
save.image("sim_batch1.RData")


i <- 1
newstart <- with(st[1,], 
                 llist(a, k, r, death, pred_nh40, pred_chl0))
## previous values:
try(f0 <- fitfun(chl_nh4_mod3, newstart)) ## fails
## slow but works (because Normal doesn't require result >0)
sumfun(f1 <- fitfun(chl_nh4_mod3_G, newstart))
## LOTS of warnings from lsoda ... capture.output() intercepts
junk <- capture.output(sumfun(f2 <-
         fitfun(chl_nh4_mod3,
            newstart, solver.opts=list(method="lsoda"))))
sumfun(f3 <- fitfun(chl_nh4_mod3_G, newstart, solver.opts=list(method="rk4",
                                                               hini=0.001)))
## lowering step size looks like it works?
sumfun(f4 <- fitfun(chl_nh4_mod3, newstart, solver.opts=list(method="rk4",
                                                               hini=0.001)))

## maybe there's something faster and robust?
## try rk4 with smaller or adaptive step size?
## method="adams" ?

## solving ODE with these params gives crazy answers:
## ode.solve 
test1 <- ode.solve(chl_nh4_mod3, 1:40, unlist(newstart))
test1G <- ode.solve(chl_nh4_mod3_G, 1:40, unlist(newstart))
matplot(test1@solution[,"time"],test1@solution[,-1],type="l")
matlines(test1G@solution[,"time"],test1@solution[,-1],type="l",lwd=2)
matplot(test1@solution[,"time"],test1@solution[,-1],type="l",
        ylim=c(-300,300))
gradfun <- function(t,y,parms) {
    g <- with(as.list(c(y,parms)),
              c(pred_nh4=-a*pred_chl*(pred_nh4/(k+pred_nh4))+
                    r*death*pred_chl-cammonium,
                pred_chl=a*pred_chl*(pred_nh4/(k+pred_nh4)) -
                    death*pred_chl))
    return(list(g))
}
library(deSolve)
odesol2 <- ode(func=gradfun,
               y=c(pred_nh4=newstart$pred_nh40,
                   pred_chl=newstart$pred_chl0),
               times=1:40,
               parms=newstart)
odesol2_rk4 <- ode(func=gradfun,
               y=c(pred_nh4=newstart$pred_nh40,
                   pred_chl=newstart$pred_chl0),
               times=1:40,
               parms=newstart,
               method="rk4")
pfun <- function(x,fn=matlines,col="black") {
    fn(x[,1],x[,-1],type="l",col=col,ylab="density",xlab="time")
}
pfun(test1@solution)
pfun(odesol2,matlines,col="red")
pfun(odesol2_rk4,col="blue")
abline(h=0,lty=3)
