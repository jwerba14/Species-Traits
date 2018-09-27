dat <- read.csv("Algae_Nutrient.csv")
str(dat)
with(dat,plot(date1,nh4))
with(dat,plot(chl,nh4))
library(ggplot2)
library(tidyverse)

dat$urep <- dat$rep + dat$treat
dat1 <- dat %>% filter(rep==3,treat==9)
mod <- nls(chl~logist(r=r,k=k,t=date1,n=44),data = dat1,start = list(r=.1,k=150))
plot(predict(mod),dat1$chl)
newdat <- data.frame (
  date1 = dat1$date1,
  chl = predict(mod)
)

g1 <- ggplot(aes(date1, chl), data = dat1) + geom_point()+geom_point(data = newdat, aes(color="red"))
with(dat1, plot(date1,chl))
mod2 <- nls(chl~log_alt(r=r,x=x,t=date1,n=44,n0=5.5),data = dat1,start = list(r=-0.5,x=40)) 

## try to do all reps at once
exdat <- dat %>%
  group_by(urep) %>%
  mutate(start_chl = chl[1])

full_mod <- nls(chl~logist(r=r,k=k,t=date1,n=start_chl),data = exdat,start = list(r=.1,k=150))

pred_df <- data.frame(
  date1 <- exdat$date1,
  chl <- predict(full_mod)
)


g2 <- ggplot(aes(date1,chl), data = exdat) + geom_point() + geom_line(data = pred_df)

## ok that was fun but what I actually want is a table of r-- that will then be a function of nutrients


## make a function to fit the logist-- if a rep won't fit just gives NA
fit_nls <- function(subdat) {
  
 nls_fit <- try(
    nls(chl~logist(r=r,k=k,t=date1,n=start_chl),data=subdat,start = list(r=.1,k=150)),
    silent = TRUE
    )
 
 p = 1
 while (class(nls_fit) == "try-error") {
 start_r <- rlnorm(1, -2, 1)
 start_k <- rlnorm(1, log(250), 3)
 
 nls_fit <- try(
   nls(chl~logist(r=r,k=k,t=date1,n=start_chl),data=subdat,start = list(r=start_r,k=start_k)),
   silent = TRUE
 )
 
 p = p + 1
 
 if (p > 100) { break }
 
 }
 
 if(class(nls_fit) == "try-error") {
   return(c(NA, NA))
 } else {
   return(coef(nls_fit)[c(1, 2)])
 }
  
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
  k = 0
)

for (i in 1:length(unique(exdat$urep))) {
  temp_dat <- exdat[exdat$urep == unique(exdat$urep)[i],]
  fits_df[i,c(4,5)] <- fit_nls(temp_dat)
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

#try to do it in tidy but failed...
rdf <- exdat %>% 
  group_by(rep, treatment) %>%
  summarize(fit_nls(subdat = .))


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

