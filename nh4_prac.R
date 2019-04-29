## parameters for algal growth at different NH4 over time
## still not fitting high nh4 in either parameterization and in alt param only fits half
## goal is to get range of phi and x for each treatment and then fit those by treatment for final param?
source("transfer_functions.R")
dat <- read.csv("Algae_Nutrient.csv")
str(dat)
with(dat,plot(date1,nh4))
with(dat,plot(chl,nh4))
library(tidyverse)
library(MASS)
library(nlstools)


## make a function to fit the logist-- if a rep won't fit just gives NA
fit_nls <- function(subdat, par_names=NULL, logscale=FALSE) {

    if (!logscale) {
        par_names <- c("r","k")
        form <- chl~logist(r=r,k=k,t=date1,a0=start_chl)
        startvals <- list(r=.1,k=150)
    } else {
        par_names <- c("logr","logk")
        form <- chl~logist(r=exp(logr),k=exp(logk),t=date1,a0=start_chl)
        ## FIXME:: DRY
        startvals <- list(logr=log(.1),logk=log(150))
    }
  nls_fit <- try(
    nls(form,data=subdat,start = startvals),
        silent = TRUE
     )
  
  p = 1
  while (class(nls_fit) == "try-error") {
    if (logscale) stop("we haven't fixed this part of the code yet")
    start_r <- rlnorm(1, -2, 1)
    start_k <- rlnorm(1, log(250), 3)
    
    nls_fit <- try(
      nls(chl~logist(r=r,k=k,t=date1,a0=start_chl),data=subdat,start = list(r=start_r,k=start_k)),
      silent = TRUE
    )
    
    p = p + 1
    
    if (p > 100) { break }
    
  }
  
  col_names <- c(outer(par_names,c("est","se"),paste,sep="_"))
  if(class(nls_fit) == "try-error") {
    pp <- rep(NA,4) ## FIXME: get the right number of NAs programmatically
    ## FIXME: will fail if first group is bad
  } else {
    coef_tab <- coef(summary(nls_fit))
    pp <- c(coef_tab[,c("Estimate","Std. Error")])
  }
  ## collapse results to a one-row matrix
  dd <- data.frame(matrix(pp,nrow=1))
  names(dd) <- col_names
  return(dd)
}

fit_nls2 <- function(subdat) {
  data.frame(matrix(fit_nls(subdat),nrow=1))
}

# add a unique name for each individual box
dat$urep <- dat$rep + dat$treat




fit_nls_alt <- function(subdat, par_names=c("phi","x")) {
  
  nls_fit <- try(
      nls(chl~log_alt(phi=phi,x=x,t=date1,a0=start_chl,n0=start_nh4),
          data=subdat,start = list(phi=1,x=1/200)),
    silent = TRUE
  )
  
  p = 1
  while (class(nls_fit) == "try-error") {
    if (!is.na(subdat$r_est[1])){
    
   
      start_phi <- rlnorm(1,log(subdat$r_est[1]/subdat$start_nh4[1]),1)
 
    start_x <- rlnorm(1, log(subdat$start_nh4[1]/subdat$k_est[1]), 1)
    } else {
      start_phi <- rlnorm(1, -2, 1)
      start_x <- 1/rlnorm(1, log(250), 1)
    }
    nls_fit <- try(
      nls(chl~log_alt(phi=phi,x=x,t=dat1$date1,a0=start_chl,n0=start_nh4),
          data=subdat,start = list(phi=start_phi,x=start_x)),
      silent = TRUE
    )
    
    p = p + 1
    
    if (p > 100) { break }
    
  }
  
  col_names <- c(outer(par_names,c("est","se"),paste,sep="_"), 
                 outer(par_names,c("conf.2.5","conf.97.5"),paste,sep="_"))
  if(class(nls_fit) == "try-error") {
    pp <- rep(NA,4) ## FIXME: get the right number of NAs programmatically
    ## FIXME: will fail if first group is bad
    con <- rep(NA, 4)
  } else {
    coef_tab <- coef(summary(nls_fit))
    pp <- c(coef_tab[,c("Estimate","Std. Error")])
    con <- nlstools::confint2(nls_fit)
  }
  
  ## collapse results to a one-row matrix
  dd <- data.frame(matrix(
    data = c(pp,con[1],con[2],con[3],con[4]),
    nrow=1))
  names(dd) <- col_names
  return(dd)
}






##  fit r and k for all reps at once
exdat <- dat %>%
  group_by(urep) %>%
  mutate(start_chl = chl[1])

res <- exdat %>% do(fit_nls(.))

## fit reps with alternate parameters (phi * n0 = r and phi/x = K), phi = r/no, x=k/phi)
mdat <- exdat %>%
  group_by(urep)%>%
  mutate(start_nh4 = nh4[1])

mdat <- left_join(mdat,res)

newfit <- mdat %>% do(fit_nls_alt(.))


### function of x and phi vs nh4
## need to add in nh4 start to newfit

newfit2 <- left_join(newfit,mdat)

ggplot(newfit2, aes(log(start_nh4),phi_est)) + geom_point()

ggplot(newfit2, aes(start_nh4,x_est)) + geom_point()



## BMB  ## JW I DID SOMETHING WEIRD AND MESSED THIS UP

## for debugging purposes
debug(fit_nls_alt)
d1 <- filter(mdat,urep==1.5)
n1 <- nls(chl~logist(r=r,k=k,t=date1,a0=start_chl),
          data=d1,start = list(r=.1,k=150))
stats:::confint.default(n1)  ## Wald intervals  (Gaussian)
nlstools::confint2(n1)       ## Wald intervals  (t statistic)
## these break when we try to do the profile
## nlstools::confint2(n1,method="profile")
## MASS:::confint.nls(n1)



## BMB

newfit_tidy <- (newfit
    %>% na.omit()
    %>% gather(param,value, -urep)
    ## split names of parameter and estimate/se
    %>% separate(param, c("param","w"), sep="_")
    %>% spread(w,value)
)

ggplot(newfit_tidy,aes(urep,est,
                       ymin=conf.2.5,
                       ymax=conf.97.5)) +
    geom_pointrange() + facet_wrap(~param,scale="free")

library(ggstance) ## for linerangeh
ggplot(na.omit(newfit), aes(phi_est,x_est,colour=as.factor(urep))) + geom_point()+
    geom_linerange(aes(ymin=x_est-2*x_se,ymax=x_est+2*x_se))+
    geom_linerangeh(aes(xmin=phi_est-2*phi_se,xmax=phi_est+2*phi_se))+
    geom_path(colour="red")+
    geom_label(aes(label=urep))

full <- left_join(newfit_tidy,mdat)
(ggplot(full[full$param=="phi",], aes(start_nh4,est,color=as.factor(treat)))
    + geom_point()
    + labs(y=expression(phi))
    + geom_smooth(aes(group=1))
    ## + scale_y_sqrt()
)

(full
    %>% filter(param=="x", start_nh4<40)
    %>% ggplot(aes(start_nh4,est,color=as.factor(treat)))
    + geom_point()
    +labs(y="x")
)

(ggplot(full, aes(start_nh4,est,color=as.factor(treat)))
    + geom_point()
    + facet_wrap(~param,scale="free")
)


### predict ODE 

dat2 <- exdat %>% filter(urep == 1.5)
ode_pred(r=0.278, k=125,t=dat2$date1,a0=dat2$start_chl[1])

## try to get ode predictions for all reps

df <- left_join(res,exdat)

ode_fit_df <- df %>%
  group_by(urep)%>%
  do(ode_pred(r=r_est[1], k=k_est[1], t=date1,a0=start_chl[1]))  ## this doesn't work cant find r-est etc



df1 <- df %>% filter (!is.na(r_est))


temp <- data.frame(
  urep = (df1$urep),
  date1 = df1$date1,
  chl_est = 0
  
)

for (i in 1:length(unique(df1$urep))) {
  ss <- df1 %>% filter(urep == unique(df1$urep)[i])
  
  sto <-  ode_pred (
   r=ss$r_est[1],
   k=ss$k_est[1],
   t=ss$date1,
   a0 = ss$start_chl[1])
  
  temp[temp$urep==unique(temp$urep)[i], ]$chl_est <- sto   
}



















## bad ones?
bad_u <- res %>% filter(is.na(k_est)) %>% pull(urep)

ggplot(filter(exdat,urep %in% bad_u),
       aes(date1,chl))+geom_point()+geom_smooth()+
    facet_wrap(~urep)+
    scale_y_log10()

res2 <- (res
    %>% filter(k_est-2*k_se>0) 
    %>% gather(var,val,-urep)
    %>% separate(var,into=c("param","element"))
    %>% spread(element,val)
)

ggplot(res2,aes(urep,est,ymin=est-2*se,ymax=est+2*se))+
    geom_pointrange()+facet_wrap(~param,scale="free")+
    geom_smooth(method="gam"
                ## ,formula=y~s(x,k=6)
                )

library(mgcv)
kpars <- filter(res2, param=="k")
gam(est~s(urep),weights=1/se^2,data=kpars)
## or whatever kind of model you want to fit: linear, quadratic, or ...
summary(lm(est~urep+I(urep^2),weights=1/se^2,data=kpars))

dd0 <- filter(exdat,urep==1.5)
plot(chl~date1,data=dd0)
lines(dd0$date1,ode_pred(r=0.1,k=150,t=dd0$date1,a0=dd0$chl[1]))


nlsfit0 <- nls(chl~logist(r=r,k=k,t=date1,a0=chl[1], debug=TRUE),
    data=dd0,
    start = list(r=.1,k=150))

## to get this working:
##  (1) pick 'better starting values' (not a general solution)
nls(chl~ode_pred(r=r,k=k,t=date1,a0=chl[1]),
    data=dd0,
    start = as.list(coef(nlsfit0))) ## (r=.1,k=150))

## (2) bound the parameters (hmm ...)
nls(chl~ode_pred(r=r,k=k,t=date1,a0=chl[1]),
    data=dd0,
    start = list(r=.1,k=150),
    lower=c(0.01,1),
    algorithm="port")

## (3) fit r, k on the log scale
## (make the parameters log_r, log_k, exponentiate them
##  inside the odepred function)

## (4) try fitode function, which might have built-in link
## functions for parameters

full_mod <- nls(chl~logist(r=r,k=k,t=date1,n=start_chl),data = exdat,start = list(r=.1,k=150))

pred_df <- data.frame(
  date1 <- exdat$date1,
  chl <- predict(full_mod)
)


g2 <- ggplot(aes(date1,chl), data = exdat) + geom_point() + geom_line(data = pred_df)

## ok that was fun but what I actually want is a table of r-- that will then be a function of nutrients




newdat <- exdat %>%
  select(c(urep,start_chl,chl,nh4,date1))

newdat$r <- 0
newdat$k <- 0
newdat$r_se <- 0
newdat$k_se <- 0

for (i in 1:length(unique(newdat$urep))) {
  temp_dat <- newdat[newdat$urep == unique(newdat$urep)[i],]
  newdat[i,c(6,7,8,9)] <- fit_nls(temp_dat)
}

#make loop to fit for each rep individually to get a dataframe with all r and k estimates
chl <- exdat %>%
  group_by(urep) %>%
  summarize(chl = mean(chl))

nh4 <- exdat %>%
  group_by(urep) %>%
  summarize(nh4 = mean(nh4))

fits_df <- data.frame(
  urep = unique(dat$urep),
  chl = chl[2],
  nh4 =nh4[2],
  r = 0,
  k = 0,
  r_se = 0,
  k_se =0
)

for (i in 1:length(unique(exdat$urep))) {
  temp_dat <- exdat[exdat$urep == unique(exdat$urep)[i],]
  fits_df[i,c(4,5,6,7)] <- fit_nls(temp_dat)
}



fits_df2 <- data.frame(
  treat = unique(dat$treat),
  r = 0,
  k = 0
)


for (i in 1:length(unique(exdat$treat))) {
  temp_dat <- exdat[exdat$treat == unique(exdat$treat)[i],]
  fits_df2[i,c(2,3)] <- fit_nls(temp_dat)
}


fit_nls(subdat = exdat)
















###### random other stuff
#try to do it in tidy but failed...

ggplot(aes(nh4+no3,chl),data = dat)+geom_point(aes(color=as.factor(treat)))

ggplot(aes(chl, nh4+no3),data = dat)+geom_point(aes(color=as.factor(treat)))
ggplot(aes(date1, chl),data = dat)+geom_point(aes(color=as.factor(treat))) +
 facet_wrap(~as.factor(rep))

ggplot(aes(date1,nh4+no3),data = dat)+geom_point(aes(color=as.factor(treat)))

ggplot(aes(date1,no3),data = dat)+geom_point(aes(color=as.factor(treat)))


con <- dat %>%
  group_by(control,Treatment) %>%
  select(nh41,nh42,chl1,chl2,Num_Daphnia, control,Treatment)%>%
  summarize(avg_nh41 = mean(nh41,na.rm = TRUE),
            avg_nh42 = mean(nh42, na.rm = TRUE),avg_chl1 = mean(chl1, na.rm = TRUE),
            avg_chl2 = mean(chl2,na.rm = TRUE))

con$diff_nh <- with(con,avg_nh41-avg_nh42)

with(con, plot(diff_nh))

new <- c(0.657-.48,.221-.634,.125-(-.265),.636-.264,1.17-(-.393),0.185-.0333,
         .877-1.12)
treat <- c(1,2,3,4,5,6,7)



ggplot(aes(treat,new))+geom_point()

