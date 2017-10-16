#make data frame for simulated data: want to use NH4 to predict chl a
# 5 replicates for 5 treatments over 28 days
faux <- data.frame(
  time = rep(seq(1,27),each=25),
  ind = rep(seq(1,25), 27),
  chl = c(rep(40,25),rep(0,650)),
  nh4 = c(rep(c(3,9,18,36,54),each =5),rep(0,650)))

faux_out <- vector("list", 16)

## mich men growth
mikmen <- function(a, s, nh4) {
 # nh4 <- seq(0.01, 40, by = 0.1)
 a * nh4 / (s + nh4)
 # plot(nh4, out)
}
death <- function(a,b,chl){
  a*chl^(1/b)
}
par(mfrow = c(1,1));plot(death(0.25, 10, seq(40, 200, by = 10))) 
par(mfrow = c(1,1)); plot(mikmen(.31, 684, seq(1, 40, by = 1)))

#need nh4 to make sense??? need its own function some sort of decreasing function over time
fun <- function (dat,a, s,z, sd_chl_proc, sd_nh4_proc, sd_chl_obs, sd_nh4_obs) {
  for (i in 1:length(unique(faux$ind))) {  #to follow an individual replicate
    for (j in 2:length(unique(faux$time))) { #over each time point
      
      faux[faux$ind == unique(faux$ind)[i] & faux$time == unique(faux$time)[j], ]$nh4 <-   
        z *  subset(faux, ind == unique(faux$ind)[i] & time == unique(faux$time)[j-1])$nh4 
    
        #process level in z (nh4)
      faux[faux$ind == unique(faux$ind)[i] & faux$time == unique(faux$time)[j], ]$nh4 <-
      rlnorm(1,log(faux[faux$ind == unique(faux$ind)[i] & faux$time == unique(faux$time)[j], ]$nh4), 
             log(sd_nh4_proc))
      
      faux[faux$ind == unique(faux$ind)[i] & faux$time == unique(faux$time)[j], ]$chl <-
        # yesterdays chl +
        faux[faux$ind == unique(faux$ind)[i] & faux$time == unique(faux$time)[j-1], ]$chl+
        # a times yesterday nh4/(s +yesterday Nh4)*
         ((a*faux[faux$ind==unique(faux$ind)[i]&faux$time==unique(faux$time)[j-1],]$nh4)/
              (s+faux[faux$ind==unique(faux$ind)[i]&faux$time==unique(faux$time)[j-1],]$nh4)*
            # yesterdays chl-
        faux[faux$ind == unique(faux$ind)[i] & faux$time == unique(faux$time)[j-1], ]$chl) - 
        #(D * yesterdays chl)* yesterdays chl so that D is density dependent
        death(0.81,31, faux[faux$ind == unique(faux$ind)[i] & faux$time == unique(faux$time)[j-1], ]$chl)*
  faux[faux$ind == unique(faux$ind)[i] & faux$time == unique(faux$time)[j-1], ]$chl
            
     #process level in chl
      faux[faux$ind==unique(faux$ind)[i] & faux$time==unique(faux$time)[j], ]$chl <-
        rlnorm(1,log(faux[faux$ind == unique(faux$ind)[i] & faux$time == unique(faux$time)[j], ]$chl), 
               log(sd_chl_proc))
  }
  }

  #observation level error 
  faux$chl<- rlnorm(length(faux$chl),log(faux$chl),log(sd_chl_obs))
  faux$nh4 <- rlnorm(length(faux$nh4), log(faux$nh4),log(sd_nh4_obs))
  
  return(list(c(a=a,s=s,z=z,sd_chl_proc=sd_chl_proc, sd_nh4_proc=sd_nh4_proc, 
                sd_chl_obs=sd_chl_obs, sd_nh4_obs=sd_nh4_obs), faux))
}

faux_out[c(1,2)] <- fun(dat=faux,a=1,s=20,z=0.9,sd_chl_proc=1.0, sd_nh4_proc=1.0, 
                          sd_chl_obs=1.0, sd_nh4_obs=1.0)

par(mfrow = c(2, 1)); with(faux_out[[2]], plot(time, nh4)); with(faux_out[[2]], plot(time, log(chl)))

faux_out[c(3,4)] <- fun(dat=faux,a=1,s=20,z=0.9,sd_chl_proc=1.0, sd_nh4_proc=1.0, 
                          sd_chl_obs=1.2, sd_nh4_obs=1.0)

faux_out[c(5,6)] <- fun(dat=faux,a=1,s=20,z=0.9,sd_chl_proc=1.0, sd_nh4_proc=1.0, 
                          sd_chl_obs=1.0, sd_nh4_obs=1.2)

faux_out[c(7,8)] <- fun(dat=faux,a=1,s=20,z=0.9,sd_chl_proc=1.0, sd_nh4_proc=1.0, 
                          sd_chl_obs=1.2, sd_nh4_obs=1.2)

faux_out[c(9,10)] <- fun(dat=faux,a=1,s=20,z=0.9,sd_chl_proc=1.1, sd_nh4_proc=1.0, 
                          sd_chl_obs=1.0, sd_nh4_obs=1.0)

faux_out[c(11,12)] <- fun(dat=faux,a=1,s=20,z=0.9,sd_chl_proc=1.0, sd_nh4_proc=1.1, 
                          sd_chl_obs=1.0, sd_nh4_obs=1.0)

faux_out[c(13,14)] <- fun(dat=faux,a=1,s=20,z=0.9,sd_chl_proc=1.1, sd_nh4_proc=1.1, 
                          sd_chl_obs=1.0, sd_nh4_obs=1.0)

faux_out[c(15,16)] <- fun(dat=faux,a=1,s=20,z=0.9,sd_chl_proc=1.1, sd_nh4_proc=1.1, 
                        sd_chl_obs=1.2, sd_nh4_obs=1.2)

save(faux_out,file = "faux_out.Rdata")


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
