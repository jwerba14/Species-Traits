## function to convert scendesmus chl a to cell concentration
## based on Ferreira et al 2016 Low Light intensity and energy starvation...

chl_adj <- function(chl){
  ((chl-0.0301)/9) * 10^8 
}

cell_adj <- function(cell){
  ((9*cell)/10^8)-0.0301
  
}

## make own equations based on Dust and Shindala 1970
chl_dat <- read.csv("chla_conversion.csv")
names(chl_dat)[1] <- "chla"

#eq <- lm(cells~chla+0, data = chl_dat)

ggplot(chl_dat, aes(chla, cells)) + geom_point(aes(color=type))
