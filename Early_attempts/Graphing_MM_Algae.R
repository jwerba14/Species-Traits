library(ggplot2)
library(nlmrt)

source("cleandat.R")
source("Mich_Death_functions.R")

ch2 <- data.frame(ch1)
#ch2$time <- seq(1,11)

#ggplot(ch2, aes(time,X1))+geom_point()
#ggplot(ch2, aes(time,X2))+geom_point()


nh2 <- data.frame(nh1)


### get proportional change for every replicate 
# loop over column each time rowbind so
#ch3 <- ch2[,-c(31)]
newdat <- data.frame(
  chlorp = numeric(0),
  ammonia = numeric(0),
  repn = numeric(0)
)
for (i in 1:ncol(ch2)) {
  newdat <- rbind(newdat, data.frame(
   
    chlorp = (ch2[-1, i] + death2(.00002,ch2[-length(ch2[ ,i]),i],0.5)*ch2[-length(ch2[,i]),i])/ 
    ch2[-length(ch2[ ,i]), i] - 1,
   
    #chlorp = (ch2[-1, i] + death(.25, 10,ch2[-length(ch2[ ,i]),i])*ch2[-length(ch2[,i]),i])/ 
     #ch2[-length(ch2[ ,i]), i] - 1,
   
    ammonia = nh2[-length(nh2[ ,i]), i],
    repn = rep(i, 10)
  ))
}

gg1 <- ggplot(newdat[newdat$chlorp < 3, ], aes(ammonia, chlorp)) + geom_point() + theme_bw() +
  geom_smooth()

ggplot(newdat[newdat$ammonia < 30 & newdat$chlorp < 2.5, ], aes(ammonia, chlorp)) + geom_point() + theme_bw() 

## quick MM parameter fit



newdat_mmfit <- newdat
newdat_mmfit$chlorp <- newdat_mmfit$chlorp 
newdat_mmfit1 <- newdat_mmfit[newdat_mmfit$ammonia < 30,]
newdat_mmfit2 <- newdat_mmfit[newdat_mmfit$ammonia < 40, ]

nls_fit <- nlxb(chlorp ~ a * ammonia / (s + ammonia),
     start = c(a = 2, s = 0.5),
     data = newdat_mmfit)

nls_fit1 <- nlxb(chlorp ~ a * ammonia / (s + ammonia),
                start = c(a = 2, s = 0.5),
                data = newdat_mmfit1)
nls_fit2<- nlxb(chlorp ~ a * ammonia / (s + ammonia),
                 start = c(a = 2, s = 0.5),
                 data = newdat_mmfit2)

nls_fit_dat <- data.frame(
  ammonia = seq(0.001, 60, by = 0.1),
  chlorp = mikmen(nls_fit$coefficients[1], nls_fit$coefficients[2], seq(0.001, 60, by = 0.1)))

nls_fit_dat1 <- data.frame(
  ammonia = seq(0.001, 60, by = 0.1),
  chlorp = mikmen(nls_fit1$coefficients[1], nls_fit1$coefficients[2], seq(0.001, 60, by = 0.1)))

nls_fit_dat2 <- data.frame(
  ammonia = seq(0.001, 60, by = 0.1),
  chlorp = mikmen(nls_fit2$coefficients[1], nls_fit2$coefficients[2], seq(0.001, 60, by = 0.1)))

ggplot(newdat_mmfit[newdat_mmfit$chlorp < 3, ], aes(ammonia, chlorp)) + geom_point() +
  theme_bw() + geom_line(data = nls_fit_dat, lwd = 2, col = "blue")

ggplot(newdat_mmfit1[newdat_mmfit1$chlorp < 3, ], aes(ammonia, chlorp)) + geom_point() +
  theme_bw() + geom_line(data = nls_fit_dat, lwd = 2, col = "blue")

ggplot(newdat_mmfit1[newdat_mmfit2$chlorp < 3, ], aes(ammonia, chlorp)) + geom_point() +
  theme_bw() + geom_line(data = nls_fit_dat, lwd = 2, col = "blue")

## What this tells me is that the death process can't be constant, because it doesn't
## allow for parameters for the MM function to fit the data that make biological
## sense. If death were constant but higher, then maybe the MM parameters make more
## sense but the death process doesn't make much sense



## Look at growth given daily proportional change
test_dat <- data.frame(day = seq(1, 20, by = 1),
           amount = c(40, rep(0, 19)))
for(i in 2:nrow(test_dat)) {
  test_dat$amount[i] = test_dat$amount[i - 1] * 1.5
}
plot(test_dat)
test_dat[11, ]


##
library(tibble)
library(dplyr)
library(ggplot2); theme_set(theme_bw())

cfun1 <- function(dd,valname="chl") {
    ## add day as a column and make sure it's numeric
    dd2 <- rownames_to_column(as.data.frame(dd),"day") %>%
        mutate(day=as.numeric(as.character(day)))
    ## convert to long form (except for day): put trial in the right order
    dd3 <- gather(dd2,key=trial,value=val,-day) %>%
        mutate(trial=factor(trial,levels=unique(trial)))
    names(dd3)[3] <- valname ## ugh
    return(dd3)
}

cdat <- full_join(cfun1(ch1),cfun1(nh1,"nh4"))

trt_tab <- data.frame(trial=factor(1:30,levels=1:30),
                      nutr=rep(c(0.5,3,9,27,54,108),each=5))
cdat <- full_join(cdat,trt_tab)

cdat <- cdat %>%
    group_by(trial) %>%
    mutate(chl_percap_change=log(chl/lag(chl)))

ggplot(cdat,aes(day,chl))+
    geom_line(aes(colour=factor(nutr),group=trial),alpha=0.6)+
    geom_smooth(aes(group=nutr,colour=factor(nutr)),alpha=0.2)

ggplot(cdat,aes(day,chl_percap_change))+
    geom_line(aes(group=trial,colour=factor(nutr)),alpha=0.3)+
    geom_smooth(aes(group=nutr,colour=factor(nutr),fill=factor(nutr)),alpha=0.1)+
    geom_hline(aes(yintercept=0),lty=2)


cdat <- cdat %>% mutate(high_nutr=nutr>=27)
ggplot(cdat,aes(chl,chl_percap_change))+
    geom_point(aes(colour=factor(nutr)))+
    geom_hline(aes(yintercept=0),lty=2)+
    geom_path(aes(colour=factor(nutr),group=trial),alpha=0.2)+
    ## geom_smooth(## method="gam",
                ## formula=y~s(x,k=10),
    ##   aes(group=nutr,colour=factor(nutr),fill=factor(nutr)),alpha=0.1)+
    facet_wrap(~high_nutr,nrow=1,scale="free_x")


ggplot(subset(cdat,chl<=300),
              aes(chl,chl_percap_change))+
    geom_point(aes(colour=factor(nutr)))+
    geom_hline(aes(yintercept=0),lty=2)+
    ## geom_path(aes(colour=factor(nutr),group=trial),alpha=0.2)+
    geom_smooth(## method="gam",
                ## formula=y~s(x,k=10),
       aes(group=nutr,colour=factor(nutr),fill=factor(nutr)),alpha=0.1)


str(cdat)
ggplot(cdat,aes(nh4,chl_percap_change))+
    geom_point(aes(colour=factor(nutr)))+
    scale_x_log10()+facet_wrap(~as.factor(nutr))


ggplot(subset(cdat,chl_percap_change>0),
       aes(nh4,chl))+
    geom_point(aes(size=chl_percap_change,colour=factor(nutr)),
               alpha=0.3)+
    geom_path(aes(group=trial,colour=factor(nutr)),alpha=0.2)+
    scale_x_log10()+
    scale_size(range=c(1,10))
