load("faux_out.Rdata")
library(tidyr)

#need 2 matrices, with rows as time and individual as column. The cells are filled with first chl and in 2nd nh4

names <- c("no_error", "obs_chl","obs_nh4","proc_chl","proc_nh4","all_obs","all_proc","all_error")
ss <- seq(2, 16, by = 2)

for(i in ss) {
  
  nam <- names[i/2]

dat_chl <- faux_out[[i]][,-4] %>%   #this gets rid of Nh4
  spread(ind,chl)
ch1 <- as.matrix(dat_chl)
ch1 <- ch1[,-1]

assign(nam, ch1)

}

names1 <- c("no_error_n", "obs_chl_n","obs_nh4_n","proc_chl_n","proc_nh4_n","all_obs_n",
            "all_proc_n","all_error_n")

for(i in ss){
  
  nam1 <-names1[i/2]
  
  dat_nh4 <- faux_out[[i]][,-3] %>%   #this gets rid of chl
    spread(ind,nh4)
  nh1 <- as.matrix(dat_nh4)
  nh1 <- nh1[,-1]
  
  assign(nam1,nh1)
}

##need clean_sim.R

newparam <- fun(dat=faux,a=0.03,s=684,z=0.9,sd_chl_proc=1, sd_nh4_proc=1, 
                sd_chl_obs=1, sd_nh4_obs=1)
par(mfrow = c(2, 1)); with(newparam[[2]],
                           plot(time, nh4)); with(newparam[[2]], plot(time, log(chl)))

# so run 7 iterations of jags model and record (powerpoint) save densities and the chains rs


plot.new()
plot(proc_chl[,1],col="white")
lines(proc_chl[,1],col="black")
lines(proc_chl[,2],col="black")
lines(proc_chl[,3],col="black")
lines(proc_chl[,4],col="black")
lines(proc_chl[,5],col="black")
lines(no_error[, 1], col = "blue", lwd = 2)


#simulate data with parameters found from data
pred <- data.frame(
  time = rep(seq(1,11),each=30),
  ind = rep(seq(1,30), 11),
  chl = c(dat$chl[1:30],rep(0,300)),
  nh4 = dat$nh4)

fun <- function (dat,a, s) {
  for (i in 1:length(unique(pred$ind))) {  #to follow an individual replicate
    for (j in 2:length(unique(pred$time))) { #over each time point
      
      pred[pred$ind == unique(pred$ind)[i] & pred$time == unique(pred$time)[j], ]$chl <-
        pred[pred$ind == unique(pred$ind)[i] & pred$time == unique(pred$time)[j-1], ]$chl+
        (a*pred[pred$ind==unique(pred$ind)[i]&pred$time==unique(pred$time)[j-1],]$nh4)/
        (s+pred[pred$ind==unique(pred$ind)[i]&pred$time==unique(pred$time)[j-1],]$nh4)
      
      
    }
  }
  
  return(pred)
}
pred_out <- fun(dat=pred,a=39,s=4.7)


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
