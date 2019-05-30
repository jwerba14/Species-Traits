state <- c(pred_nh4=5,
           pred_chl = 15)


parameters <- c(v = .1,
                s = 100,
                g=10,
                death =.0001,
                d1 = 1 
                )

chl_nh4_fun <- function(t,state, parameters){
  with(as.list(c(state, parameters)),{
                     pred_nh4 <- -pred_chl*((v)/(pred_nh4+s)) -0.000101
                     
                     ## chl is gained through uptake of nh4 and lost through density dependent death
                     ## death is not directly measured-- for evidence of dd death see nls feeding
                     pred_chl <- pred_chl*((v)/(pred_nh4+s))*g - pred_chl*(death*pred_chl)/(d1+pred_chl)
                     
                   list(c(pred_nh4, pred_chl)) })
}



out <- ode(y = state, times = seq(0,300,0.1), func = chl_nh4_fun, parms = parameters)
plot(out)


