#source(nh4_prac)
source("../transfer_functions.R")
source("../Graphing_Set_Up.R")
library(nlstools)
library(nlmrt)
#library(boot)
library(tidyverse)

cerio <- read.csv("cerio_pop.csv")
cerio["Population"][is.na(cerio["Population"])] <- 1

#cerio1 <- cerio[cerio$Rep==1,]

#mod <- nls(Population~logist(r=r,k=k,t=Day,a0=3.35),data = cerio1,start = list(r=1,k=50))
#pp <-nlsBoot(mod)
#mod_xb <- nlxb(Population ~  k/ (1+((k/3.35)-1)*exp(-r*Day)), data = cerio1, start =list(r=1,k=50) )
#cerio2 <- cerio[cerio$Rep==12,]



#mod2 <- nls(Population~logist(r=r,k=k,t=Day,a0=3.35),data = cerio2,start = list(r=.1,k=5))



#cerio_25 <- cerio %>% filter(Treatment == 25)
#mod3 <- nls(Population~logist(r=r,k=k,t=Day,a0=3.35),data = cerio_25,start = list(r=1,k=8))

fit_nls <- function(subdat, par_names=c("r","k")) {
  
  nls_fit <- try(
    nls(Population ~  k/ (1+((k/3.35)-1)*exp(-r*Day)),data=subdat,start = list(r=1,k=50)),
    silent = TRUE
  )
  
  p = 1
  while (class(nls_fit) == "try-error") {
    start_r <- rlnorm(1, -2, 1)
    start_k <- rlnorm(1, 3, 3)
    nls_fit <- try(
      nlxb(Population ~  k/ (1+((k/3.35)-1)*exp(-r*Day)),data=subdat,start = list(r=start_r,k=start_k)),
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
    coef_tab <- summary(nls_fit)
    pp <- c(coef = coef_tab$coeff, SE = coef_tab$SEs)
  }
  ## collapse results to a one-row matrix
  dd <- data.frame(matrix(pp,nrow=1))
  names(dd) <- col_names
  return(dd)
}

c2 <- cerio %>%
  group_by(Rep) %>%
  mutate(start_chl = Avg_Chl[1])  ## this is a bit weird and not right bc should be constant throughout so maybe just have too many parameters



final <- c2 %>% do(fit_nls(.))
final <- final %>% dplyr::select(Rep, r_est, k_est, r_se, k_se)

c3 <- c2 %>% 
  dplyr::select(Rep, Treatment, Avg_Chl) %>%
  group_by(Rep) %>%
  slice(n())

f2 <- left_join(c3,final, by="Rep")
 
## set unkn uncertainty to max and use as weight 
## no uncertainity




## regression
## without error

r_reg <- lm(data = f2, r_est ~ -1 + Avg_Chl)
k_reg <- lm(data = f2, k_est ~ -1 + Avg_Chl)

newdat <- data.frame(Avg_Chl = seq(0,60, by= 0.1))

rg <- data.frame(predict(r_reg, newdata = newdat, interval = c("confidence")))
names(rg) <- c("r_est" ,"lwr" ,"upr")
rg$Avg_Chl <- newdat$Avg_Chl

(rr_g <- ggplot(data = f2, aes(Avg_Chl, r_est)) + geom_point() +
  geom_line(data = rg) + geom_ribbon(data = rg,aes(ymin = lwr, ymax = upr), alpha = 0.1))



kg <- data.frame(predict(k_reg, newdata = newdat, interval = c("confidence")))
names(kg) <- c("k_est" ,"lwr" ,"upr")
kg$Avg_Chl <- newdat$Avg_Chl

(kr_g <- ggplot(data = f2, aes(Avg_Chl, k_est)) + geom_point() +
    geom_line(data = kg) + geom_ribbon(data = kg,aes(ymin = lwr, ymax = upr), alpha = 0.1))



## graph without weights

f3 <- f2
# with error
for(i in 1:nrow(f3)){
  if(is.na(f3$r_se[i])) {
    f3$r_se[i] <- max(f3$r_se, na.rm = T)
  } else {
    f3$r_se[i] <- f3$r_se[i]
  }
}

for(i in 1:nrow(f3)){
  if(is.na(f3$k_se[i])) {
    f3$k_se[i] <- max(f3$k_se, na.rm = T)
  } else {
    f3$k_se[i] <- f3$k_se[i]
  }
}


new_reg <- lm(data = f3, r_est ~ -1 + Avg_Chl, weights = 1/r_se)
new_rK <- lm(data = f3
             , k_est ~ -1 + Avg_Chl, weights = 1/k_se)

##graph with weights

rgw <- data.frame(predict(new_reg, newdata = newdat, interval = c("confidence")))
names(rgw) <- c("r_est" ,"lwr" ,"upr")
rgw$Avg_Chl <- newdat$Avg_Chl

(rw_g <- ggplot(data = f3, aes(Avg_Chl, r_est)) + geom_point() +
    geom_line(data = rgw) + geom_ribbon(data = rgw,aes(ymin = lwr, ymax = upr), alpha = 0.1))



kgw <- data.frame(predict(new_rK, newdata = newdat, interval = c("confidence")))
names(kgw) <- c("k_est" ,"lwr" ,"upr")
kgw$Avg_Chl <- newdat$Avg_Chl

(kw_g <- ggplot(data = f3, aes(Avg_Chl, k_est)) + geom_point() +
    geom_line(data = kgw) + geom_ribbon(data = kgw,aes(ymin = lwr, ymax = upr), alpha = 0.1))



