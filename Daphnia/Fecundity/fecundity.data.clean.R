daph <- read.csv("daphnia_lifetime.csv")
daph <- daph %>% 
  filter(adult_only=="N")

## to get fecundity parameter fit saturating curve (params z and w in full ode)
## need to make per day so need to divide total fecundity by # of days that individual was an adult
daph_fec <- daph %>% group_by(rep, treatment) %>%
  filter(size_class == "A") %>%
  summarize(
    time_adult = n()
    , life_fec   = sum(clutch_size, na.rm = TRUE)
    , chl        = mean(chl_avg)
    , chl_sd_rep = sd(chl_avg)) %>%
  mutate(daily_fec = life_fec / time_adult)


## make chl in cells per ml

daph_fec_adj <- daph_fec %>% 
  mutate(cell = chl_adj(chl = chl))

daph_fec_adj %>% dplyr::select(cell, chl)

daph_fec_adj$sd <- 0
## remove 0s because those are dead individuals

daph_fec_adj <- daph_fec_adj %>% 
  filter(!(daily_fec == 0 & chl > 5) )


## data from literature
fec_lit <- read.csv("fec_lit.csv")

fec_lit$cell <- c(NA,1e+09, NA, NA, 1e+08,5e+05, 166666.7, NA, 5e+05, NA,NA, NA)
fec_lit$sd <- fec_lit$sd_repro
fec_lit$daily_fec <- fec_lit$daphnia_reproduction
fec_lit$rep <- as.factor(rep("A", nrow(fec_lit)))


fec_lit1 <- fec_lit %>% filter(!is.na(cell))
fec_lit1<- fec_lit1 %>% 
  mutate(chl = cell_adj(cell = cell)) 

fec_lit1 <- fec_lit1 %>% mutate(cell1 = chl_adj(chl))
which(fec_lit1$cell != fec_lit1$cell1)