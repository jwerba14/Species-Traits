## read in rds from nh4-alg
library(tidyverse)
library(data.table)
library(shinystan)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

path <- 'C:/Users/jower/OneDrive/Documents/GitHub/Species-Traits/Nitrogen_Algae/RDS'

files = list.files(path = path, pattern = '^nitalg_[0-9]{1}_[0-9]{1}\\.RDS$')




param_alg <- data.frame(
  param = numeric(0),
  value = numeric(0),
  quant = numeric(0),
  treat1 = numeric(0) ,
  rep = numeric(0) 
)


## pull out treat, rep, parameter name, 2.5-97.5 estimates
for(i in 1:length(files)){
  tempmod <- readRDS(paste(path,files[i], sep = "/"))
  ttt <- summary(tempmod)
  ttt1 <- ttt$summary[c(5:9), c(4:8)]
  ttt2 <- data.frame(ttt1)
  ttt2$param <- dimnames(ttt1)[[1]]
  ttt2 <- ttt2 %>% pivot_longer(-param, names_to = "quant", values_to = "value")
  pp <- unlist(strsplit(files[i], "_"))
  ttt2$treat <- pp[2]
  ttt2$rep <- pp[3]
  param_alg <- rbind(param_alg,ttt2)
  
}

param_alg$param2 <- rep("t", nrow(param_alg))

for(i in 1:nrow(param_alg)){
  if(param_alg$param[i] == "p[1]") {
    param_alg$param2[i] <- "a"
  } else if (param_alg$param[i] == "p[2]"){
    param_alg$param2[i] <- "k"
  }else if (param_alg$param[i] == "p[3]") {
    param_alg$param2[i] <- "l"
  }else if (param_alg$param[i] == "p[4]") {
    param_alg$param2[i] <- "death1"
  } else if (param_alg$param[i] == "p[5]") {
    param_alg$param2[i] <- "f"
  }
}

#write.csv(param_alg, file = "algal_parameters.csv")
