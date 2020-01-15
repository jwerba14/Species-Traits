## function to convert scendesmus chl a to cell concentration
## based on Ferreira et al 2016 Low Light intensity and energy starvation...

chl_adj <- function(chl){
  ((chl-0.0301)/9) * 10^8 
}

cell_adj <- function(cell){
  ((9*cell)/10^8)-0.0301
  
}
