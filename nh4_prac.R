dat <- read.csv("Algae_Nutrient.csv")
str(dat)
with(dat,plot(date1,nh4))
with(dat,plot(chl,nh4))
library(ggplot2)

ggplot(aes(nh4+no3,chl),data = dat)+geom_point(aes(color=as.factor(treat)))

ggplot(aes(date1,nh4+no3),data = dat)+geom_point(aes(color=as.factor(treat)))

ggplot(aes(date1,no3),data = dat)+geom_point(aes(color=as.factor(treat)))


con <- dat %>%
  group_by(control,Treatment) %>%
  select(nh41,nh42,chl1,chl2,Num_Daphnia, control,Treatment)%>%
  summarize(avg_nh41 = mean(nh41,na.rm = TRUE),
            avg_nh42 = mean(nh42, na.rm = TRUE),avg_chl1 = mean(chl1, na.rm = TRUE),
            avg_chl2 = mean(chl2,na.rm = TRUE))

con$diff_nh <- with(con,avg_nh41-avg_nh42)

with(con, plot(diff_nh))

new <- c(0.657-.48,.221-.634,.125-(-.265),.636-.264,1.17-(-.393),0.185-.0333,
         .877-1.12)
treat <- c(1,2,3,4,5,6,7)



ggplot(aes(treat,new))+geom_point()

