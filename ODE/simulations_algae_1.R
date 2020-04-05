library(fitode)
set.seed(101)
max_param_set <- 100 ## number of parameter sets to try
llist <- lme4:::namedList

## cammonium = (1-9.4235e-01) ## measured ammonium loss in abiotic system
#cammonium = 0

chl_nh4_mod3 <- odemodel(
  name = "algal_nit",
  model = list(
    pred_nh4 ~ -a*pred_chl*(pred_nh4/(k+pred_nh4))+r*death*pred_chl,
    pred_chl ~  e*a*pred_chl*(pred_nh4/(k+pred_nh4)) - death*pred_chl
  ),
  observation = list(
    nh4 ~ dlnorm(meanlog = log(pred_nh4), sdlog = 0.05),
    chl ~ dlnorm(meanlog = log(pred_chl), sdlog = 0.01)
  ),
  initial = list(pred_nh4 ~ pred_nh40 , pred_chl ~ pred_chl0),
  par=c("a","k", "r","death","e", "pred_nh40", "pred_chl0")
)

start <- c(a = 0.03, 
            k = .03,
            r = 1,
            death = 0.02,
            e = 1,  
            pred_nh40= 15,
            pred_chl0 = 40
)
s3 <-  simulate(chl_nh4_mod3,nsim = 1, parms= start, times = seq(1,40),             
                solver.opts=list(method="rk4", hini=0.1))
plot(s3$times, s3$nh4)
plot(s3$times, s3$chl)


start2 <- c(a = 0.03, 
            k = .03,
            r = 1,
            death = 0.01,
            e = 0.4,  
            pred_nh40= 15,
            pred_chl0 = 40)

s4 <-  simulate(chl_nh4_mod3,nsim = 5, parms= start2, times = seq(1,40),             
                solver.opts=list(method="rk4", hini=0.1))
plot(s4$times, s3$nh4)
plot(s4$times, s3$chl)

fitode(chl_nh4_mod3, data= s4, start = start2, tcol = "times",solver.opts = list(method = "lsoda")) ## good recovery

## try with different start values to see if converge at true parameter
params <- c("a","k","r","death","e", "pred_nh40","pred_chl0")


st <- expand.grid(
  a = seq(0.01,0.5,0.04),
  k = seq(0.01,0.5,0.04),
  r = seq(0.01,1,0.08),
  death = seq(0.001,0.05,0.004),
  e = seq(0.01,1,0.08),
  pred_nh40 = 15,
  pred_chl0 = 40
)

row_samp <- sample(1:nrow(st), 100, replace = F)
new_ls <- st[row_samp,]

m <- min(max_param_set,nrow(new_ls))


## construct column names; t transposes; c flattens
cnames <- c(t(outer(c("","low","high"),params,paste0)),"loglik","ttime")

## set up columns of NAs
temp <- as.data.frame(replicate(length(cnames),rep(NA,m)))
names(temp) <- cnames

fitfun <- function(model, start,
                   solver.opts = list(method="lsoda"))  {
  tt <- system.time({
    tempm <- try(fitode(model, data= s4, start = start2, tcol = "times", 
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
                   llist(a, k, r, death,e, pred_nh40, pred_chl0))
  ff <- fitfun(chl_nh4_mod3, newstart)
  temp[i,] <- sumfun(ff)
  cat(i,"/",m,"\n")
}










