library(tidyverse)
library(pomp)
library(doParallel)
library(foreach)
library(doRNG)
library(deSolve)


source("../Graphing_Set_Up.R")

tt <- sobolDesign(lower = c(a =1, k= 1000, l = 0.01, death1 = 0.05, g = 0.01),
                  upper = c(a =50, k= 9000, l = 1, death1 = 2, g = 1),
                 500000)

usable.cores <- 3
registerDoParallel(cores = usable.cores)

dat <- read.csv("Data/Algae_Nutrient.csv")
dat$ammonium <- dat$nh4*1000  ## put nh4 in ug so that it is one the same scale as chl
dat$algae <- dat$chl 

d2 <- dat[dat$treat == 108, ]

source("fit_functions.R")

checktime <- system.time({
 # for (i in 1:length(unique(d2$treat))){
  ## Run mifs_local one time as a "burn-in" run (using MCMC terms...)
  registerDoRNG(610408799)
    
  mifs_local <- foreach(j = 1:length(unique(d2$rep)), .combine = rbind) %dopar%  {
    
    library(tidyverse)
    library(deSolve)
    library(pomp)
    
    gg <- data.frame(a = numeric(length = nrow(tt)),
                     k = numeric(length = nrow(tt)),
                     l =numeric(length = nrow(tt)),
                     death1 = numeric(length = nrow(tt)),
                     g = numeric(length = nrow(tt)),
                     loglik = numeric(length = nrow(tt)))
        
    for (k in 1:nrow(tt)){
      params <- unlist(tt[k,])
      gg[k, 1:5] <- params
      gg[k,6] <- loglik(params,d2[d2$rep == unique(d2$rep)[j], ])
    }
    
    gg <- gg %>% mutate(rep = unique(d2$rep)[j])
    
    gg
    
    
  }
  
  
} 
#}
) 

#saveRDS(mifs_local, file = "Treat_108_ll.RDS")




