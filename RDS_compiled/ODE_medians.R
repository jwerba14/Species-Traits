source("../transfer_functions.R")
source("treatments.R")
library(deSolve)
# state variables given completely arbitary values but wanted to get names on them

param <- read.csv("parameters_no_cerio.csv")

state <- c(dammonium = 4.4,
           daph_j = 0,
           daph_a = 20,
           dalgae = 15
           
)

#parameters- will need parameters for each function
parameters_med <- c(
  alpha = 4.379716e-03, beta = 29,
  death1 = .007, death2 = 0.000082,
  gamma =  0.365 , omega = 10.5,
  growth_a = median(param$growth_a), growth_b = median(param$growth_b),
  death_b = median(param$death_b), a_feed_m = median(param$a_feed_m),
  a_exc_m = median(param$a_exc_m), 
  j_feed_m = median(param$j_feed_m),
  j_exc_m = median(param$j_exc_m), amm_param_b = median(param$amm_param_b),
  fec_a = median(param$fec_a), fec_b = median(param$fec_b)
) 


# write ODE- return has to be in same order as state variable equations are listed

d_equations <-function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    # rate of change
    # ammonium is added from release by both juvenile and adult daphnia   
    ddammonium <- daph_a*a_exc_m*a_feed_m*dalgae + daph_j*a_exc_m*j_feed_m*dalgae -  ## need to fix excretion- not right
      
       dalgae*dammonium*alpha*omega/(omega+dammonium) + 
      
     # ((dammonium*alpha*omega/(omega+dammonium)) / 10) * (death1*dalgae + death2*(dalgae^2)) -
      
       gamma/2 * (death1*dalgae + death2*(dalgae^2)) - 
      
      # lagvalue(t - 5, 1) -
      
      (1-amm_param_b)*dammonium
    
    
  
    ddaph_j <- sat_fun(fec_a,fec_b,dalgae)*daph_a - daph_j*(35/70) 
    
    #daphnia adults are added from growth by juveniles and are lost from death
    ddaph_a <- 1/(sat_fun(growth_a,growth_b,dalgae))*daph_j- daph_a*(1/death_b)
    
    
  
    ddalgae <- beta * dalgae*dammonium*alpha*omega/(omega+dammonium) - death1*dalgae - death2*(dalgae^2)-
      daph_a*a_feed_m*dalgae - daph_j*j_feed_m*dalgae
    
    
    
    # return the rate of change
    list(c(ddammonium, ddaph_j,ddaph_a,ddalgae))
  }) # end with(as.list ...
}


out_med <- ode(y = state, times = seq(0,42,0.1), func = d_equations, parms = parameters_med)
plot(out_med)


parameters_upp <- c(
  alpha = 4.379716e-03, beta = 29,
  death1 = .007, death2 = 0.000082,
  gamma =  0.365 , omega = 10.5,
  growth_a = quantile(param$growth_a, c(0.975), names = F), growth_b = quantile(param$growth_b,c(0.975), names = F),
  death_b = quantile(param$death_b,c(0.975), names = F), a_feed_m = quantile(param$a_feed_m,c(0.975), names = F),
  a_exc_m = quantile(param$a_exc_m,c(0.975), names = F), j_feed_m = quantile(param$j_feed_m,c(0.975), names = F),
  j_exc_m = quantile(param$j_exc_m,c(0.975), names = F), amm_param_b = quantile(param$amm_param_b,c(0.975), names = F),
  fec_a = quantile(param$fec_a,c(0.975), names = F), fec_b = quantile(param$fec_b,c(0.975), names = F)
) 

out_up <- ode(y = state, times = seq(0,42,0.1), func = d_equations, parms = parameters_upp)
plot(out_up)



parameters_low <- c(
  alpha = 4.379716e-03, beta = 29,
  death1 = .007, death2 = 0.000082,
  gamma =  0.365 , omega = 10.5,
  growth_a = quantile(param$growth_a, c(0.025), names = F), growth_b = quantile(param$growth_b,c(0.025), names = F),
  death_b = quantile(param$death_b,c(0.025), names = F), a_feed_m = quantile(param$a_feed_m,c(0.025), names = F),
  a_exc_m = quantile(param$a_exc_m,c(0.025), names = F), j_feed_m = quantile(param$j_feed_m,c(0.025), names = F),
  j_exc_m = quantile(param$j_exc_m,c(0.025), names = F), amm_param_b = quantile(param$amm_param_b,c(0.025), names = F),
  fec_a = quantile(param$fec_a,c(0.025), names = F), fec_b = quantile(param$fec_b,c(0.025), names = F)
) 

out_low <- ode(y = state, times = seq(0,42,0.1), func = d_equations, parms = parameters_low)
plot(out_low)

pred <- as.data.frame(out_low)
pred$quantile <- "low"
predup <- as.data.frame(out_up)
predup$quantile <- "upper"
predmed <- as.data.frame(out_med)
predmed$quantile <- "med"

library(tidyverse)
## daph only
dat_daph <- dat %>% filter(treatment == 3) %>% filter(NH4 < 20)%>% 
  filter(Chl < 100, Chl >= 0) %>% select(TankNum, NH4, Chl,Day)
names(dat_daph) <- c("TankNum", "dammonium","dalgae","time")
amm_g <- ggplot(predmed, aes(time,dammonium)) + geom_line(linetype = "solid")+ 
  geom_line(data= predup, linetype = "dotdash") + geom_line(data=pred, linetype ="dotdash")+
  theme_bw()+ geom_point(data = dat_daph,aes(time,dammonium) )+
  ylab("Ammonium mg/L")+xlab("Days")+
  theme(axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 32),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 32),
        strip.text = element_text(size = 0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank()) 


(alg_g <- ggplot(predmed, aes(time,dalgae)) + geom_line(linetype = "solid")+ 
  geom_line(data= predup, linetype = "dotdash") + geom_line(data=pred, linetype ="dotdash")+
  theme_bw()+ geom_point(data = dat_daph,aes(time,dalgae) )+
  ylab("Chl a ug/L")+xlab("Days")+
  theme(axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 32),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 32),
        strip.text = element_text(size = 0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank()) )

source("pop_curves.R")

dj <- popA %>% filter(treatment == 3) %>% group_by(TankNum,ExptDay) %>% summarize (daph_a = mean(count, na.rm = T)*100)
names(dj) = c("TankNum", "time","daph_a")

(dapha_g <- ggplot(predmed, aes(time,daph_a)) + geom_line(linetype = "solid")+ 
    geom_line(data= predup, linetype = "dotdash") + geom_line(data=pred, linetype ="dotdash")+
    theme_bw()+ geom_point(data = dj,aes(time,daph_a) )+
    ylab("Daphnia/L")+xlab("Days")+
    theme(axis.text.x = element_text(size = 30),
          axis.text.y = element_text(size = 32),
          axis.title.x = element_text(size = 30),
          axis.title.y = element_text(size = 32),
          strip.text = element_text(size = 0),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank()) )


djuv <- popJ %>% filter(treatment == 3) %>% group_by(TankNum,ExptDay) %>% summarize (daph_j = mean(count, na.rm = T)*100)
names(djuv) = c("TankNum", "time","daph_j")

(daphj_g <- ggplot(predmed, aes(time,daph_j)) + geom_line(linetype = "solid")+ 
    geom_line(data= predup, linetype = "dotdash") + geom_line(data=pred, linetype ="dotdash")+
    theme_bw()+ geom_point(data = djuv,aes(time,daph_j) )+
    ylab("Juvenile Daphnia/L")+xlab("Days")+
    theme(axis.text.x = element_text(size = 30),
          axis.text.y = element_text(size = 32),
          axis.title.x = element_text(size = 30),
          axis.title.y = element_text(size = 32),
          strip.text = element_text(size = 0),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank()) )

