 param <- read.csv("parameters_no_cerio.csv")

state <- c(dammonium = 4.4,
           dalgae = 15
           
)
library(deSolve)
#parameters- will need parameters for each function
parameters_med <- c(
  alpha = 4.379716e-03, beta = 29,
  death1 = .007, death2 = 0.000082,
  gamma =  0.365 , omega = 10.5,
  amm_param_b = median(param$amm_param_b)
) 


# write ODE- return has to be in same order as state variable equations are listed

d_equations <-function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    # rate of change
    
    ddammonium <-  -dalgae*dammonium*alpha*omega/(omega+dammonium) +
      
      gamma/2 * (death1*dalgae + death2*(dalgae^2)) - 
      
      # lagvalue(t - 5, 1) -
      
      (1-amm_param_b)*dammonium
    
    
    
    ddalgae <- beta * dalgae*dammonium*alpha*omega/(omega+dammonium) - death1*dalgae - death2*(dalgae^2)
    
    
    
    # return the rate of change
    list(c(ddammonium,ddalgae))
  }) # end with(as.list ...
}


out_med <- ode(y = state, times = seq(0,30,0.1), func = d_equations, parms = parameters_med)
plot(out_med)


parameters_upp <- c(
  alpha = 4.379716e-03, beta = 29,
  death1 = .007, death2 = 0.000082,
  gamma =  0.365 , omega = 10.5,
  amm_param_b = quantile(param$amm_param_b,c(0.975), names = F)
) 

out_up <- ode(y = state, times = seq(0,30,0.1), func = d_equations, parms = parameters_upp)
plot(out_up)



parameters_low <- c(
  alpha = 4.379716e-03, beta = 29,
  death1 = .007, death2 = 0.000082,
  gamma =  0.365 , omega = 10.5,
  amm_param_b = quantile(param$amm_param_b,c(0.025), names = F)
) 

out_low <- ode(y = state, times = seq(0,30,0.1), func = d_equations, parms = parameters_low)
plot(out_low)

pred <- as.data.frame(out_low)
pred$quantile <- "low"
predup <- as.data.frame(out_up)
predup$quantile <- "upper"
predmed <- as.data.frame(out_med)
predmed$quantile <- "med"



library(tidyverse)
## alg only
dat_alg <- dat %>% filter(treatment == 1) %>% filter(NH4 < 20)%>% 
  filter(Chl < 100, Chl >= 0) %>% select(TankNum, NH4, Chl,Day)
names(dat_alg) <- c("TankNum", "dammonium","dalgae","time")
amm_g <- ggplot(predmed, aes(time,log(dammonium))) + geom_line(linetype = "solid")+ 
  geom_line(data= predup,aes(time, log(dammonium)), linetype = "dotdash") + geom_line(data=pred,aes(time, log(dammonium)), linetype ="dotdash")+
  theme_bw()+ geom_point(data = dat_alg,aes(time,log(dammonium) ))+
  ylab("Log (Ammonium mg/L)")+xlab("Days")+
  theme(axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 32),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 32),
        strip.text = element_text(size = 0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank()) 


(alg_g <- ggplot(predmed, aes(time,log(dalgae))) + geom_line(linetype = "solid")+ 
    geom_line(data= predup, aes(time, log(dalgae)), linetype = "dotdash") + geom_line(data=pred,aes(time, log(dalgae)), linetype ="dotdash")+
    theme_bw()+ geom_point(data = dat_alg,aes(time,log(dalgae)) )+
    ylab("Log (Chl a ug/L)")+xlab("Days")+
    theme(axis.text.x = element_text(size = 30),
          axis.text.y = element_text(size = 32),
          axis.title.x = element_text(size = 30),
          axis.title.y = element_text(size = 32),
          strip.text = element_text(size = 0),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank()) )
