dat_nit_9 <- read.csv("dat_nit_9.csv")

library(fitode)


cammonium = 0.04085 # proportional ammonium lost to env-- calc in nutrient_air.R

chl_nh4_mod <- new("model.ode",
                   name = "algal_nit",
                   model = list(
                     pred_nh4 ~ -pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) + gamma *(death1*pred_chl + death2*(pred_chl^2))-cammonium*pred_nh4 ,
                     pred_chl ~ beta * pred_chl*pred_nh4*alpha*omega/(omega+pred_nh4) - death1*pred_chl - death2*(pred_chl^2)
                   ),
                   ## consider using bbmle::dnorm_n ?
                   observation = list(
                     nh4 ~ dnorm2(mean = pred_nh4),
                     chl ~ dnorm2(mean = pred_chl)
                   ),
                   initial = list(pred_nh4 ~ pred_nh40 , pred_chl ~ pred_chl0),
                   par=c("alpha", "beta", "omega", "death1","death2", "pred_nh40", "pred_chl0", "gamma")
)


start <- c(alpha = 5.577709e-03, 
          beta = 1.626124e+01,
          omega=1.401032e+01,
          death1=2.954388e-03,
          death2=6.663002e-04,
          pred_nh40 = 6 ,
          pred_chl0 = 50, 
          gamma=9.698845e-02 
)



chl_fit_9_dd <- fitode(
  chl_nh4_mod,
  data = dat_nit_9,
  start=start2,
  tcol = "date1",
  solver.opts=list(method="rk4", hini=0.1)
  #method="Nelder-Mead"
)
coef(chl_fit_9_dd)
plot(chl_fit_9_dd, level=0.95)