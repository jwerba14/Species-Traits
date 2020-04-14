## function to convert scendesmus chl a to cell concentration
## based on Ferreira et al 2016 Low Light intensity and energy starvation...
#@article{ferreira2016low,
#  title={Low light intensity and nitrogen starvation modulate the chlorophyll content of Scenedesmus dimorphus},
#  author={Ferreira, VS and Pinto, RF and Sant'Anna, C},
#  journal={Journal of applied microbiology},

chl_adj <- function(chl){
  ((chl-0.0301)/9) * 10^8 
}

chl_adj2 <- function(chl){
  chl*10^8
}


cell_adj <- function(cell){
  ((9*cell)/10^8)+0.0301
  
}

cell_adj2 <- function(cell){
  (9*cell)/10^8
}
## make own equations based on Dust and Shindala 1970
#chl_dat <- read.csv("chla_conversion.csv")
#names(chl_dat)[1] <- "chla"


#ggplot(chl_dat, aes(chla, cells)) + geom_point(aes(color=type))
