#simulate data with parameters found from data
#load(dat_simulation.R)

pred <- data.frame(
  time = rep(seq(1,11),each=30),
  ind = rep(seq(1,30), 11),
  chl = c(dat$chl[1:30],rep(0,300)),
  nh4 = dat$nh4)

fun <- function (dat,a, s,d1) {
  for (i in 1:length(unique(pred$ind))) {  #to follow an individual replicate
    for (j in 2:length(unique(pred$time))) { #over each time point
      
      pred[pred$ind == unique(pred$ind)[i] & pred$time == unique(pred$time)[j], ]$chl <-
        pred[pred$ind == unique(pred$ind)[i] & pred$time == unique(pred$time)[j-1], ]$chl+
        (a*pred[pred$ind==unique(pred$ind)[i]&pred$time==unique(pred$time)[j-1],]$nh4)/
        (s+pred[pred$ind==unique(pred$ind)[i]&pred$time==unique(pred$time)[j-1],]$nh4)*
        pred[pred$ind == unique(pred$ind)[i] & pred$time == unique(pred$time)[j-1], ]$chl-
        death2(d1, pred[pred$ind == unique(pred$ind)[i] & pred$time == unique(pred$time)[j-1], ]$chl,0.5)*
        pred[pred$ind == unique(pred$ind)[i] & pred$time == unique(pred$time)[j-1], ]$chl
      
      
    }
  }
  
  return(pred)
}
pred_out <- fun(dat=pred,a=39,s=4.7,d1=0.00005)


# graph simulated data with parameters from model
pred_out$treat <- rep(rep(c(0.5,3,9,27,54,108), each = 5),11)

pg <- pred_out %>%
  group_by(time,treat)

pg <- pg %>%
  summarise_all(funs(mean(., na.rm=TRUE),sd(.,na.rm=TRUE))) #-c('date',"treat","rep","date1"))


#Graph the trends in chlorophyll


pg$treat <- as.factor(pg$treat)

pg1 <- ggplot(newdat, aes(time, chl_mean)) + geom_line(aes(colour = treat), size = 1)
#+ geom_errorbar(aes(ymin=chl_mean-chl_sd, ymax=chl_mean+chl_sd), width = 0.1)