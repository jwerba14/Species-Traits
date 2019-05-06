exp_model <- new("model.ode",
                 name="exponential",
                 model=list(
                   A ~ - m * A
                 ),
                 observation=list(
                   nitrogen ~ dnorm(mean=A, sd=sd.nitrogen)
                 ),
                 initial=list(
                   A ~ A0
                 ),
                 par=c("m", "A0", "sd.nitrogen")
)

ff1 <- fitode(
  exp_model,
  data=data,
  start=c(m=0.4, A0=4500, sd.nitrogen=100), ## naming has to be consistent with parameter names
  tcol="day"
  

  
dat <- read.csv("Algae_Nutrient.csv")    
  
chl_nh4_mod <- new("model.ode",
                   name = "algal_nit",
                   model = list(
                     pred_nh4 ~ chl*((v*nh4)/(nh4+s))-.0001-000001,
                     
                     # chl is gained through uptake of nh4 and lost through density dependent death
                     # death is not directly measured
                     pred_chl ~ chl*((j*nh4)/(nh4+h))-chl*death
                     
                   ),
                   observation = list(
                     nh4 ~ dnorm(mean = pred_nh4, sd=sd1),
                     chl ~ dnorm(mean = pred_chl, sd=sd2)
                     
                   ),
                   initial = list(pred_nh4 ~ pred_nh40 , pred_chl ~ pred_chl0),
                   par=c("v","s","j","h","pred_nh40","pred_chl0", "sd1","sd2")
                   )


chl_fit <- fitode(
  chl_nh4_mod,
  data = dat,
  start=c(v = 0.01, 
          s = .01,
          j = 10,
          h = 10,
          pred_nh40 = 3 ,
          pred_chl0 = 15, 
          sd1 = 100 ,
          sd2 = 100 ),
  tcol = "date1"
)
