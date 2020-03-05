## literature metadata

library(tidyverse)
lit <- read.csv("meta_lit.csv")

## downloaded per search
down <- lit %>% 
  group_by(Search) %>%
  summarize (num_cite = n_distinct(Title), download = n()) 
  

## extracted data per search
ext <- lit %>% 
  filter(Data.Extracted == "yes") %>% 
  group_by(Search) %>%
  summarize (num_cite = n_distinct(Title), download = n()) 


