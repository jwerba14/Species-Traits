## CI for algal-Nit parameters
library(Hmisc)
library(tidyverse)

## for treatment 0.5
treat_0.5 <- readRDS("Fits_RDS/Treat_0.5_ll.RDS")


#Hmisc::wtd.quantile()
#for each column
#wtd.quantile() takes a vector of values and a vector of weights
#if you have negative log-likelihoods as nll_vec


nll2 <- treat_0.5$loglik-max(treat_0.5$loglik) ##positive number
#(1) adjust to be relative to best fit
likvec <- exp(nll2)  # tiny number close to zero
#(2) go from neg log-likelihood to likelihood
wts<- pmax(likvec, min(likvec[likvec>0])) ## same as above??
#(3) get rid of zero weights by setting them to the min non-zero value
#for each parameter p
ci_a_0.5 = wtd.quantile(treat_0.5$a, wts, c(0.025,0.5, 0.975))
ci_k_0.5 = wtd.quantile(treat_0.5$k, wts, c(0.025,0.5, 0.975))
ci_l_0.5 = wtd.quantile(treat_0.5$l, wts, c(0.025,0.5, 0.975))
ci_death1_0.5 = wtd.quantile(treat_0.5$death1, wts, c(0.025,0.5, 0.975))
ci_g_0.5 = wtd.quantile(treat_0.5$g, wts, c(0.025,0.5, 0.975))

#there *might* be refs to some similar approach in papers by Aaron King ...


## for treatment 3
treat_3 <- readRDS("Fits_RDS/Treat_3_ll.RDS")

nll2 <- treat_3$loglik-max(treat_3$loglik)
likvec <- exp(nll2)
wts<- pmax(likvec, min(likvec[likvec>0]))


ci_a_3 = wtd.quantile(treat_3$a, wts, c(0.025,0.5, 0.975))
ci_k_3 = wtd.quantile(treat_3$k, wts, c(0.025,0.5, 0.975))
ci_l_3 = wtd.quantile(treat_3$l, wts, c(0.025,0.5, 0.975))
ci_death1_3 = wtd.quantile(treat_3$death1, wts, c(0.025,0.5, 0.975))
ci_g_3 = wtd.quantile(treat_3$g, wts, c(0.025,0.5, 0.975))


## for treatment 9

treat_9 <- readRDS("Fits_RDS/Treat_9_ll.RDS")

nll2 <- treat_9$loglik-max(treat_9$loglik)
likvec <- exp(nll2)
wts<- pmax(likvec, min(likvec[likvec>0]))


ci_a_9 = wtd.quantile(treat_9$a, wts, c(0.025,0.5, 0.975))
ci_k_9 = wtd.quantile(treat_9$k, wts, c(0.025,0.5, 0.975))
ci_l_9 = wtd.quantile(treat_9$l, wts, c(0.025,0.5, 0.975))
ci_death1_9 = wtd.quantile(treat_9$death1, wts, c(0.025,0.5, 0.975))
ci_g_9 = wtd.quantile(treat_9$g, wts, c(0.025,0.5, 0.975))


## for treatment 27

treat_27 <- readRDS("Fits_RDS/Treat_27_ll.RDS")

nll2 <- treat_27$loglik-max(treat_27$loglik)
likvec <- exp(nll2)
wts<- pmax(likvec, min(likvec[likvec>0]))


ci_a_27 = wtd.quantile(treat_27$a, wts, c(0.025,0.5, 0.975))
ci_k_27 = wtd.quantile(treat_27$k, wts,c(0.025,0.5, 0.975))
ci_l_27 = wtd.quantile(treat_27$l, wts, c(0.025,0.5, 0.975))
ci_death1_27 = wtd.quantile(treat_27$death1, wts, c(0.025,0.5, 0.975))
ci_g_27 = wtd.quantile(treat_27$g, wts, c(0.025,0.5, 0.975))


## for treatment 54

treat_54 <- readRDS("Fits_RDS/Treat_54_ll.RDS")

nll2 <- treat_54$loglik-max(treat_54$loglik)
likvec <- exp(nll2)
wts<- pmax(likvec, min(likvec[likvec>0]))


ci_a_54 = wtd.quantile(treat_54$a, wts, c(0.025,0.5, 0.975))
ci_k_54 = wtd.quantile(treat_54$k, wts, c(0.025,0.5, 0.975))
ci_l_54 = wtd.quantile(treat_54$l, wts,c(0.025,0.5, 0.975))
ci_death1_54 = wtd.quantile(treat_54$death1, wts, c(0.025,0.5, 0.975))
ci_g_54 = wtd.quantile(treat_54$g, wts, c(0.025,0.5, 0.975))


## for treatment 108

treat_108 <- readRDS("Fits_RDS/Treat_108_ll.RDS")

nll2 <- treat_108$loglik-max(treat_108$loglik)
likvec <- exp(nll2)
wts<- pmax(likvec, min(likvec[likvec>0]))


ci_a_108 = wtd.quantile(treat_108$a, wts,c(0.025,0.5, 0.975))
ci_k_108 = wtd.quantile(treat_108$k, wts, c(0.025,0.5, 0.975))
ci_l_108 = wtd.quantile(treat_108$l, wts, c(0.025,0.5, 0.975))
ci_death1_108 = wtd.quantile(treat_108$death1, wts, c(0.025,0.5, 0.975))
ci_g_108 = wtd.quantile(treat_108$g, wts, c(0.025,0.5, 0.975))


##make full dataframe of parameters

param_1 <- rbind(ci_a_0.5,ci_k_0.5,ci_l_0.5,ci_death1_0.5,ci_g_0.5,
                 ci_a_3,ci_k_3,ci_l_3,ci_death1_3,ci_g_3,
                 ci_a_9,ci_k_9,ci_l_9,ci_death1_9,ci_g_9,
                 ci_a_27, ci_k_27, ci_l_27, ci_death1_27, ci_g_27,
                 ci_a_54, ci_k_54, ci_l_54, ci_death1_54, ci_g_54,
                 ci_a_108,ci_k_108, ci_l_108,ci_death1_108,ci_g_108 )


param_1 <- as.data.frame(param_1)
param_1$nn <- rownames(param_1)

param_1 <- param_1 %>% separate(col = nn,into =c("CI","param", "treatment") ,sep = "_") %>% dplyr::select(-CI)

## make dataframe for graphing med and CIs  
param2 <- param_1 %>% pivot_longer(-c(param, treatment), names_to = "quant", values_to = "value")

#write.csv(param2, "alg_nit_param_CI.csv")


## make dataframe to graph ODE output

param3 <- param2 %>% pivot_wider(everything(), names_from = "param", values_from = "value")

#write.csv(param3, "alg_nit_param_graph.csv")








