#make data frame for simulated data: want to use NH4 to predict chl a
# 5 replicates for 5 treatments over 28 days
faux <- data.frame(
  time = rep(seq(1,27),each=25),
  ind = rep(seq(1,25), 27),
  chl = c(rep(40,25),rep(0,650)),
  nh4 = c(rep(c(3,9,18,36,54),each =5),rep(0,650)))

faux_out <- vector("list", 16)


fun <- function (dat,a, s,z, sd_chl_proc, sd_nh4_proc, sd_chl_obs, sd_nh4_obs,d1,d2) {
  for (i in 1:length(unique(faux$ind))) {  #to follow an individual replicate
    for (j in 2:length(unique(faux$time))) { #over each time point
      
      #need nh4 to make sense??? need its own function - decreasing function over time
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
        #(D * yesterdays chl)* yesterdays chl 
        death2(d1, faux[faux$ind == unique(faux$ind)[i] & faux$time == unique(faux$time)[j-1], ]$chl,d2)*
  faux[faux$ind == unique(faux$ind)[i] & faux$time == unique(faux$time)[j-1], ]$chl
    
      #remove any replicate that hits 0 population
    if (faux[faux$ind == unique(faux$ind)[i] & faux$time == unique(faux$time)[j], ]$chl < 0) {
      faux[faux$ind == unique(faux$ind)[i] & faux$time == unique(faux$time)[j], ]$chl <- 0
    }
      
      
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
                sd_chl_obs=sd_chl_obs, sd_nh4_obs=sd_nh4_obs,d1,d2), faux))
}

faux_out[c(1,2)] <- fun(dat=faux,a=3,s=20,z=0.9,sd_chl_proc=1.0, sd_nh4_proc=1.0, 
                          sd_chl_obs=1.0, sd_nh4_obs=1.0, d1=0.00005,d2=0.5)

par(mfrow = c(2, 1)); with(faux_out[[2]], plot(time, nh4)); with(faux_out[[2]], plot(time, log(chl)))

faux_out[c(3,4)] <- fun(dat=faux,a=1,s=20,z=0.9,sd_chl_proc=1.0, sd_nh4_proc=1.0, 
                          sd_chl_obs=1.2, sd_nh4_obs=1.0,d1=0.00005,d2=0.5)

faux_out[c(5,6)] <- fun(dat=faux,a=1,s=20,z=0.9,sd_chl_proc=1.0, sd_nh4_proc=1.0, 
                          sd_chl_obs=1.0, sd_nh4_obs=1.2,d1=0.00005,d2=0.5)

faux_out[c(7,8)] <- fun(dat=faux,a=1,s=20,z=0.9,sd_chl_proc=1.0, sd_nh4_proc=1.0, 
                          sd_chl_obs=1.2, sd_nh4_obs=1.2,d1=0.00005,d2=0.5)

faux_out[c(9,10)] <- fun(dat=faux,a=1,s=20,z=0.9,sd_chl_proc=1.1, sd_nh4_proc=1.0, 
                          sd_chl_obs=1.0, sd_nh4_obs=1.0,d1=0.00005,d2=0.5)

faux_out[c(11,12)] <- fun(dat=faux,a=1,s=20,z=0.9,sd_chl_proc=1.0, sd_nh4_proc=1.1, 
                          sd_chl_obs=1.0, sd_nh4_obs=1.0,d1=0.00005,d2=0.5)

faux_out[c(13,14)] <- fun(dat=faux,a=1,s=20,z=0.9,sd_chl_proc=1.1, sd_nh4_proc=1.1, 
                          sd_chl_obs=1.0, sd_nh4_obs=1.0,d1=0.00005,d2=0.5)

faux_out[c(15,16)] <- fun(dat=faux,a=1,s=20,z=0.9,sd_chl_proc=1.1, sd_nh4_proc=1.1, 
                        sd_chl_obs=1.2, sd_nh4_obs=1.2,d1=0.00005,d2=0.5)

#save(faux_out,file = "faux_out.Rdata")



