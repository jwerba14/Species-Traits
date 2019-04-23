library(tidyverse)
library(ggplot2)

dat <- read.csv("daphnia_lifetime.csv")


theme_set(theme_bw()) 
theme_update(axis.text.x = element_text(size = 12),
             axis.text.y = element_text(size = 12),
             axis.title.x = element_text(size = 14),
             axis.title.y = element_text(size = 14),
             legend.title = element_text(size = 12),
             legend.text = element_text(size = 10),
             legend.spacing = unit(0.25, "cm"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             panel.spacing = unit(0, "lines"),
             legend.key = element_rect(fill = "white"),
             panel.spacing.y = unit(-0.25, "lines"),
             panel.border = element_rect(colour = "black", 
                                         fill = NA, size = 1),
             strip.text.x = element_text(size = 18, colour = "black", 
                                         face = "bold"))



dat1 <- dat %>%
  group_by(treatment) %>%
  summarize(average = mean(clutch_size, na.rm=TRUE), 
            standard_dev = sd(clutch_size, na.rm = TRUE))


ggplot(dat1, aes(treatment,average))+geom_point() + 
  geom_errorbar(aes(ymin = average-standard_dev, ymax = average+standard_dev))+
  ylab("Clutch Size") + xlab("Food Treatment")


dat2 <- dat %>%
  filter(size_class == "A" | size_class == "J")
  

dat2 <- dat2 %>% 
  group_by(treatment,Date) %>%
  count(size_class)


ggplot(dat2, aes(treatment,n))+geom_point(aes(color = size_class)) + #facet_grid(~Date)+
  ylab("individuals") + xlab("Food Treatment")


