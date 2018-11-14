#predictions from params for algae growth vs actual data
source("nh4_prac.R")

#just for one

str(dat1)

pred <- ode_pred(params = c(phi= 0.09883676, x=0.022566398, n0= 2.81), t=dat1$date1,a0=43.4)

dat1$pred <- pred

ggplot(dat1, aes(pred,chl))+geom_point() 

ggplot(dat1, aes(date1,chl))+geom_point() +geom_point(aes(date1,pred),color="red")

