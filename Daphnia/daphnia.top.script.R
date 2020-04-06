### Packages and functions needed for all scripts
##hmmm currently indexing of folders is confusing because if you source and you're not in that folder can't source other files
## but then need to be back in this folder to source anything else...so now im re-setting wd a lot, and re loading things
## which doesn't seem at all streamlined...

source("set_up.R")


###For fecundity parameters
 
source("Fecundity/fecundity.top.script.R")  ## see script to only run pieces


### For death rate

source("Death/death.topscript.R")



### For feeding and excretion rate

source("Feeding_and_Excretion/feeding.excretion.topscript.R")

