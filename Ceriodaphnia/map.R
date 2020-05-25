## build map for Ceriodaphnia rigaudi distribution

library(tidyverse)
library(maps)


geo_dat <- read.csv("lit_data.csv")
geo_dat <- geo_dat %>% filter(lat != "NA")



world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white") +
  geom_point(data = geo_dat, aes(long, lat, color = exact, group =exact)) +
  xlab("Longitude") + ylab("Latitude") +
  scale_color_discrete(name = str_wrap("Exact Sampling Location", width = 10), labels = c("No", "Yes"))

             