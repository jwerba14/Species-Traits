library(tidyverse)
library(lme4)
library(coefplot)
library(glmmTMB)
library(broom)
library(dotwhisker)

env <- read.csv("pool_measures.csv", check.names = FALSE)
names(env)[1] <- "measure"

env1 <- env %>% pivot_longer(-c(measure,date), names_to = "pool_id")
env2 <- env1 %>% pivot_wider(names_from = measure)



ceri <- read.csv("cerio_jam_pop.csv")
names(ceri)[1] <- "pool_id"
ceri$pool_id <- as.character(ceri$pool_id)

fd <- left_join(ceri,env2, by= c("date","pool_id"))

mod1 <- glmer(Ceriodaphnia_rigaudi ~ -1 + 
                   Temp + 
                   Sal  +
                   Oxy  +
                   pH   +
                   (1 | date)  +
                   (1 | pool_id)
                 , data = fd, family = "poisson")

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(mod1)

fd1 <- fd[complete.cases(fd) == T, ]

## as expected very overdispersed
fit_zipoisson <- glmmTMB(Ceriodaphnia_rigaudi~Temp + 
                           Sal  +
                           Oxy  +
                           pH   +
                           (1 | date)  +
                           (1 | pool_id)
                         , data = fd,
                         ziformula=~1,
                         family="nbinom2")
 

plot(fitted(fit_zipoisson), 
     residuals(fit_zipoisson), main= "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")

plot(exp(predict(fit_zipoisson)), fd$Ceriodaphnia_rigaudi)
summary(fit_zipoisson)
plot(resid(fit_zipoisson))
plot(resid(mod1))


## hmm maybe want hurdle model bc says if here what drives
fit_hurdle <- glmmTMB(Ceriodaphnia_rigaudi~ Temp + 
                           Sal  +
                           Oxy  +
                           pH   +
                           (1 | date)  +
                           (1 | pool_id)
                         , data = fd1,
                         ziformula=~  Sal + Oxy + pH + Temp,
                         family= list(family = "truncated_nbinom1", link = "log"))

plot(exp(predict(fit_hurdle)), fd1$Ceriodaphnia_rigaudi)
summary(fit_hurdle)

##plot
library(ggstatsplot)

ggstatsplot::ggcoefstats(
  x = fit_hurdle,
  only.significant = TRUE,
  exclude.intercept = TRUE,
  conf.level = 0.99,
  title = str_wrap("Hurdle model for Ceriodaphnia rigaudii presence and abundance", width = 40),
  ggtheme = ggplot2::theme_bw()) + 
  scale_y_discrete(labels = c("Conditional: Temperature", "Conditional: Salinity","Conditional: Oxygen", 
                            "Conditional: pH", "Zero Inflated: Salinity", "Zero Inflated: Oxygen",'Zero Inflated: pH',  
                            'Zero Inflated: Temperature', 'NA'))


