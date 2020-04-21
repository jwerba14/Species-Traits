alg_param <- read.csv("algal_parameters.csv")


## graph a
a_parm <- alg_param %>% filter(param2 == "a")

a_parm_g <- a_parm %>% group_by(treat) %>% filter(quant == "X50.") %>%
  summarize(med = median(value), upr = quantile(value, 0.975), lwr = quantile(value, 0.025))

ggplot(a_parm_g, aes(treat, med)) + geom_point() + geom_errorbar(aes(ymin = lwr, ymax =upr)) + ylab("a estimate")


## graph k
k_parm <- alg_param %>% filter(param2 == "k")


k_parm_g <- k_parm %>% group_by(treat) %>% filter(quant == "X50.") %>%
  summarize(med = median(value), upr = quantile(value, 0.975), lwr = quantile(value, 0.025))

ggplot(k_parm_g, aes(treat, med)) + geom_point() + geom_errorbar(aes(ymin = lwr, ymax =upr)) + ylab("k estimate")

## graph l
l_parm <- alg_param %>% filter(param2 == "l")

l_parm_g <- l_parm %>% group_by(treat) %>% filter(quant == "X50.") %>%
  summarize(med = median(value), upr = quantile(value, 0.975), lwr = quantile(value, 0.025))

ggplot(l_parm_g, aes(treat, med)) + geom_point() + geom_errorbar(aes(ymin = lwr, ymax =upr)) + ylab("l estimate")


## graph f
f_parm <- alg_param %>% filter(param2 == "f")

f_parm_g <- f_parm %>% group_by(treat) %>% filter(quant == "X50.") %>%
  summarize(med = median(value), upr = quantile(value, 0.975), lwr = quantile(value, 0.025))

ggplot(f_parm_g, aes(treat, med)) + geom_point() + geom_errorbar(aes(ymin = lwr, ymax =upr)) + ylab("f estimate")


## graph death 1
death1_parm <- alg_param %>% filter(param2 == "death1")


death1_parm_g <- death1_parm %>% group_by(treat) %>% filter(quant == "X50.") %>%
  summarize(med = median(value), upr = quantile(value, 0.975), lwr = quantile(value, 0.025))

ggplot(a_parm_g, aes(treat, med)) + geom_point() + geom_errorbar(aes(ymin = lwr, ymax =upr)) + ylab("death1 estimate")
