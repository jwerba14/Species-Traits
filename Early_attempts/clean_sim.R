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


#plot to see how much error was generated
plot.new()
plot(proc_chl[,1],col="white")
lines(proc_chl[,1],col="black")
lines(proc_chl[,2],col="black")
lines(proc_chl[,3],col="black")
lines(proc_chl[,4],col="black")
lines(proc_chl[,5],col="black")
lines(no_error[, 1], col = "blue", lwd = 2)



