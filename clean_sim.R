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


