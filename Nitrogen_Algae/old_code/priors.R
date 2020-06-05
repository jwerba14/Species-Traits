## build priors
library(tidyverse)
library(fitdistrplus)
library(MASS)
library(bbmle)
library(broom)
library(dotwhisker)
library(ggplot2); theme_set(theme_bw())
library(ggstance)
## note: MASS::fitdistr vs. fitdistrplus::fitdist

## trying to do things 'tidy':
## use broom 'verbs':
##  tidy gets coefficient tables
##  glance gets model summary (AIC etc.)
##  augment gets predictions (abusing this slightly)

## tidy method for fitdistr actually works fine for 'fitdist' objects
tidy.fitdist <- broom:::tidy.fitdistr

##' @param x is a fitted object
##' @param dname name of the distribution fitted, without "d" (e.g. "gamma", "lnorm")
##' @param data either a numeric vector of the original data fitted, *or* a list or data frame with an "x" column (or the column specified by varname)
##' @param varname column from data to use
##' @param log (logical) return log-density instead of density?
augment.fitdistr <- function(x, dname, data, n=101, varname="x", log=FALSE) {
    if (varname %in% names(data)) data <- data[[varname]]
    tibble(xval=seq(min(data, na.rm=TRUE),
                    max(data, na.rm=TRUE),
                    length.out=n),
           density=do.call(paste0("d",dname),
                           c(list(x=xval, log=log), as.list(coef(x))))
           )
}

augment.fitdist <- function(x, data, n=101, log=FALSE, dname=x$distname) {
    augment.fitdistr(x, dname=dname, data=data, n=n, log=log)
}

## not generic: works for this problem 
augment.mle2 <- function(x, data, n=101, log=FALSE, dname=NULL) {
    if (is.null(dname))
        dname <- sub("^d","",deparse(as.formula(x@formula)[[3]][[1]]))
    augment.fitdistr(x, dname=dname, data=data, n=n, log=log)
}

## model summaries
glance.fitdist <- function(x) {
    with(x,tibble(n=n,logLik=loglik,AIC=aic,BIC=bic))
}

glance.mle2 <- function(x) {
    tibble(n=length(x@data[[1]]),  ## fragile? nobs() method??
           logLik=c(logLik(x)),AIC=AIC(x),BIC=BIC(x))
}

## helper function to use mle2 to fit a gamma, computing starting
## values automatically
mle2_fit_gamma <- function(x,...) {
    ## CV = 1/sqrt(shape)
    ## -> shape = 1/CV^2 = mean^2/var
    ## mean = shape/rate = (mean^2/var)/rate
    ## rate = mean/var
    m <- mean(x)
    v <- var(x)
    m <- mle2(
        x~dgamma(shape,rate),
        data=data.frame(x),
        start=list(shape=m^2/v,rate=m/v),
        ...)
    return(m)
}

## manage conflict
select <- dplyr::select
litdat <- read.csv("literature_extraction.csv")

## make all fits (all three packages x {gamma, lognormal}
## for a specified variable
make_fits <- function(x_name="half_sat_mg_N_L", data=litdat) {
    x <- c(na.omit(data[[x_name]]))
    ## c() drops attributes (fitdist doesn't like them)

    ## three ways to fit a distribution (lognormal)
    ## 1. MASS package: output class = "fitdistr"
    fit1 <- fitdistr(x, "lognormal")

    ## 2. fitdistrplus package: output class = "fitdist"
    fit2 <- fitdist(x, "lnorm")

    ## 3. bbmle package
    ## more control (can specify fitting scale, bounds, parscale, etc.)
    ## but need to specify starting values
    fit3 <- mle2(
        x~dlnorm(meanlog,sdlog),
        data=data.frame(x),
        start=list(meanlog=mean(log(x)),sdlog=sd(log(x)))
    )

    ## same for Gamma distribution
    fit4 <- fitdistr(x, "Gamma")
    fit5 <- fitdist(x, "gamma")
    ## use method of moments to compute starting values
    fit6 <- mle2_fit_gamma(x, method="L-BFGS-B", lower=c(0.002,0.002))

    ## list of lists: distributions/methods
    fitList <- list(lnorm=list(MASS=fit1,
                               fitdistrplus=fit2,
                               mle2=fit3),
                    gamma=list(MASS=fit4,
                               fitdistrplus=fit5,
                               mle2=fit6))

    ## attach the original data in case we want it later
    attr(fitList,"data") <- x
    return(fitList)
}

sum_fits <- function(fitList, y_max=NA, bins=30) {
    data <- attr(fitList,"data")
    ## parameters: map_dfr runs the function, then row-binds the results
    params <- map_dfr(fitList,.id="distribution",
                      ## run tidy for each package and combine
                      ~(map_dfr(.,tidy,.id="pkg")
                          %>% select(pkg,term,estimate,std.error)))

    ## don't really need this, but ...
    params_plot <- ggplot(params,aes(estimate,term,colour=pkg))+
        geom_pointrangeh(aes(xmin=estimate-2*std.error,
                             xmax=estimate+2*std.error),
                         position=position_dodgev(height=0.5))+
        facet_wrap(~distribution,ncol=1,scale="free")+
        scale_color_brewer(palette="Dark2")
    
    ## goodness-of-fit (log-likelihood, AIC)
    gof <- map_dfr(fitList,
                   .id="distribution",
                   ~(map_dfr(.,glance,.id="pkg")
                       %>% select(pkg,logLik,AIC)))

    ## predictions
    pred <- map2_dfr(fitList,names(fitList),.id="distribution",
                     ~(map_dfr(.x,augment,dname=.y,
                               data=data,
                               .id="pkg")))

    pred_plot <- ggplot(pred,aes(xval,density))+
        geom_histogram(data=data.frame(xval=data),
                       bins=bins,
                       aes(y=..density..))+
        geom_line(aes(colour=distribution,linetype=pkg),lwd=2)+
        scale_y_continuous(limits=c(0,y_max))+
        scale_colour_brewer(palette="Dark2")

    return(lme4:::namedList(params, params_plot,
                           gof, pred,
                           pred_plot))
}


half_sat_fits <- make_fits("half_sat_mg_N_L")
half_sat_sum <- sum_fits(half_sat_fits,y_max=0.4)
names(half_sat_sum)

half_sat_sum$gof
half_sat_sum$pred_plot

growth_day_fits <- make_fits("growth_day")
growth_day_sum <- sum_fits(growth_day_fits)
growth_day_sum$pred_plot
growth_day_sum$gof  ## gamma fit is considerably better

death_rate_day_fits <- make_fits("death_rate_day")
death_rate_day_sum <- sum_fits(death_rate_day_fits,bins=10)
death_rate_day_sum$pred_plot
death_rate_day_sum$gof 

## to do (?)
##  add confidence intervals?
##  we should probably be using AICc instead?

### end of BMB code

hist(half_sat,breaks = 10)
denscomp(ff2B)
xfit <- seq(min(half_sat), max(half_sat), length = 40)
yfit <- dlnorm(xfit,meanlog = .2665, sdlog = 2.9)
lines(xfit, yfit)


## growth rate
hist(litdat$growth_day)
growth_rate <- litdat %>% dplyr::select(growth_day) %>% filter(growth_day !="NA")
growth_rate <- as.vector(growth_rate$growth_day)
ff2 <- fitdist(growth_rate, "gamma")  ## gamma(0.968, 1.15)
denscomp(ff2)
?dgamma
hist(dgamma(growth_rate, shape = 1, rate = 9))



## death rate
hist(litdat$death_rate_day)
death_rate <- litdat %>% dplyr::select(death_rate_day) %>% filter(death_rate_day != "NA")
death_rate <- as.vector(death_rate$death_rate_day)
ff3 <- fitdist(death_rate, "gamma")  ## gamma(.79,9.77)
denscomp(ff3)
hist(death_rate)
yfit <- dgamma(death_rate, rate = 9, shape = .79)
lines(density(death_rate))


## nitrogen removal rate
hist(litdat$nit_rem_mg_day, breaks = 15)
nit_rem <- litdat %>% dplyr::select(nit_rem_mg_day) %>% filter(nit_rem_mg_day != "NA")
nit_rem <- as.vector(nit_rem$nit_rem_mg_day)
ff4<- fitdist(nit_rem, "exp")  # exp(0.18)
denscomp(ff4)


## max NH4 uptake (not much data)
hist(litdat$max_uptake_day)
max_up <- litdat %>% dplyr::select(max_uptake_day) %>% filter(max_uptake_day !="NA")
max_up <- as.vector(max_up$max_uptake_day)
ff5 <- fitdist(max_up, "gamma") #gamma(1.13,.12)
denscomp(ff5)


