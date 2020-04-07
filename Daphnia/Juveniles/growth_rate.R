## growth rate

library(tidyverse)
library(gridExtra)

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(shinystan)


source("../../transfer_functions.R")
source("../../Graphing_Set_Up.R")

daph <- read.csv("daphnia_lifetime.csv")

## remove initial daphnia that came from the lab population (not raised in the food conditions of interest)
daph <- daph %>%
  filter(adult_only=="N")

###growth to adult
daph_growth <- daph %>%
  group_by(rep,treatment) %>%
  filter(sum(size_class == "A") != 0)

daph_growth_j <- daph_growth %>%
  filter(size_class=="J") %>%
  group_by(treatment, rep) %>%
  summarize(days_to_adult = n(), chl = mean(chl_avg), chl_sd = sd(chl_avg) )


dg2 <- daph_growth_j %>% 
  group_by(treatment) %>%
  summarize(dd = median(days_to_adult), dsd = sd(days_to_adult))

grg <- ggplot(dg2, aes(as.factor(treatment), dd, color = as.factor(treatment))) + geom_point(size= 3) + 
  geom_errorbar(aes(ymin=dd-dsd, ymax=dd+dsd)) + xlab("Chl (ug/L) Treatment") + ylab("Days to Adult") + 
  ggtitle("B. Days to Adult by food availability")+
  theme_bw() + theme(axis.text.x = element_text(size = 30),
                     axis.text.y = element_text(size = 32),
                     axis.title.x = element_text(size = 30),
                     axis.title.y = element_text(size = 32),
                     strip.text = element_text(size = 0),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     strip.background = element_blank(),
                     legend.position = "none") 


##  lets do exponential bc does look pretty even across food groups and will be easier to interpret

survcurve <- function(x) {
  x <- c(0,sort(x))
  tibble(day=x,frac_surv=seq(1,0,length.out=length(x)))
}

daph_growth_curves <- daph_growth_j %>%
  group_by(treatment) %>%
  do(survcurve(.$days_to_adult)) %>%
  mutate(adults = 1-frac_surv)



daph_grow_list1 <- list(
  "N" = nrow(daph_growth_curves),
  "days" = daph_growth_curves$day,
  "survival" = (daph_growth_curves$frac_surv)
)


if(!file.exists("../RDS_Files/growth.fit.RDS")){
  
  fitg <- stan(file = "adult_growth.stan",  ## same as death model
               data = daph_grow_list1) 
  
  saveRDS(fitg, file ="../RDS_Files/growth.fit.RDS" )
} else {
  fitg <- readRDS("../RDS_Files/growth.fit.RDS")
}




#launch_shinystan(fitg)


fit_sumg <- summary(fitg)
fit_sum_paramg <- fit_sumg$summary[c(1:4),]

tg <- rstan::extract(fitg,permuted = FALSE)
g_pred <- (rbind(tg[,1,1],tg[,2,1], tg[,3,1], tg[,4,1])) ##


newdatg <- data.frame(day = seq(0,7, by = 0.1))

pred_outg <- apply(newdatg,1,expon,b=g_pred)
pred_sumg <- apply(pred_outg, 2, FUN = function (x) quantile(x, c(0.025,0.50,0.975)))


lowerg <- data.frame(day = seq(0,7, by = 0.1), adults = 1- pred_sumg[1,])
upperg <- data.frame(day = seq(0,7, by = 0.1), adults = 1- pred_sumg[3,])
medg <- data.frame(day = seq(0,7, by = 0.1), adults =1- pred_sumg[2,])

growth_g <- ggplot(daph_growth_curves, aes(day, adults)) + geom_point(alpha = 0.6, size = 2 ) +
  geom_line(data = lowerg, linetype = "dotdash", lwd = 1.25) + 
  geom_line(data = upperg, linetype = "dotdash", lwd = 1.25)+
  geom_line(data = medg, linetype = "solid", lwd =1.25) + xlab("Day") + 
  ylab("Proportion Adult")+ ggtitle("A. Juvenile Growth Predictions")+
  theme_bw() + theme(axis.text.x = element_text(size = 30),
                     axis.text.y = element_text(size = 32),
                     axis.title.x = element_text(size = 30),
                     axis.title.y = element_text(size = 32),
                     strip.text = element_text(size = 0),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     strip.background = element_blank()) 


grid.arrange(growth_g, grg)
