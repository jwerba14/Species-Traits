## treat 1 -algae only 19,26,41,45,63,70,71,105,133,135
## treat 2- algae disturb 14,25,31,51,62,89, 94,110,112,125

## treat 3- daphnia only 1,8,13,55,58,66,99,104,130,138
## treat 4- daphnia disturb 6,29,30,50,81,82,93,111,117,126

## treat 5- cerio only 7,32,36,46,49,73,88,97,107,120
## treat 6- cerio disturb 11,22,35,57,69,79,96,119,123,137

## treat 7 - snail only 18,24,34,59,64,86,115,122,132,139
## treat 8- snail disturb 15,39,40,76,77,85,91,102,103,129,

## treat 9- daphnia and cerio 21,27,33,52, 72,75,100,101,131,140
## treat 10- daphnia and cerio disturb 4,12,38,54,68,74,92,106,118,136

## treat 11- daphnia and snail 5,10,23,44,83,84,87,114,121,127
## treat 12 - daphnia and snail disturb 9,28,37,56,67,78,90,113,116,128

## treat 13- daphnia, cerio and snail 3,17,42,47,60,61,80,95,98,108

## treat 14 - daphnia, cerio and snail disturb 2,16,20,43,48,53,65,109,124, 134
library(tidyverse)

dat <- read.csv("../Master_Data/Master_Data.csv")
pop <- read.csv("../Master_Data/Counts.csv")

dat <- dat %>% filter(TankNum != "NA")
dat$treatment <- 0

for (i in 1:nrow(dat)){
  if (dat$TankNum[i] ==19| dat$TankNum[i] == 26| dat$TankNum[i]==41|
      dat$TankNum[i] == 45|  dat$TankNum[i] ==63|
      dat$TankNum[i] ==70| dat$TankNum[i] ==71| 
      dat$TankNum[i] ==105| dat$TankNum[i] ==133|dat$TankNum[i] ==135){
    dat$treatment[i] <- 1
  } else if (dat$TankNum[i] == 14|dat$TankNum[i] ==25|dat$TankNum[i] ==31|dat$TankNum[i] ==51|
             dat$TankNum[i] ==62|dat$TankNum[i] ==89| dat$TankNum[i] ==94| dat$TankNum[i] ==110|
             dat$TankNum[i] ==112| dat$TankNum[i] ==125){
    dat$treatment[i] <- 2
  } else if (dat$TankNum[i] ==1|dat$TankNum[i] ==8|dat$TankNum[i] ==13|
             dat$TankNum[i] ==55|dat$TankNum[i] ==58|dat$TankNum[i] ==66|
             dat$TankNum[i] ==99|dat$TankNum[i] ==104|
             dat$TankNum[i] ==130|dat$TankNum[i] ==138
  ){
    dat$treatment[i] <- 3
  } else if (dat$TankNum[i] == 6|dat$TankNum[i] ==29|dat$TankNum[i] ==30|
             dat$TankNum[i] ==50|dat$TankNum[i] ==93|dat$TankNum[i] ==81|dat$TankNum[i] ==82|
             dat$TankNum[i] ==111|dat$TankNum[i] ==117|dat$TankNum[i] ==126
    
  ) {
    dat$treatment[i] <- 4
  } else if (dat$TankNum[i] ==7|dat$TankNum[i] ==32|dat$TankNum[i] ==36|
             dat$TankNum[i] ==46|dat$TankNum[i] ==49|dat$TankNum[i] ==73|
             dat$TankNum[i] ==88|dat$TankNum[i] ==97|dat$TankNum[i] ==107|
             dat$TankNum[i] ==120){
    dat$treatment[i] <- 5
  } else if (dat$TankNum[i] ==11|dat$TankNum[i] ==22|dat$TankNum[i] ==35|dat$TankNum[i] ==57|dat$TankNum[i] ==119|
             dat$TankNum[i] ==69|dat$TankNum[i] ==79|dat$TankNum[i] ==96|dat$TankNum[i] ==123|dat$TankNum[i] ==137){
    dat$treatment[i] <- 6
  } else if (dat$TankNum[i] ==18|dat$TankNum[i] ==24|dat$TankNum[i] ==34|dat$TankNum[i] ==59|
             dat$TankNum[i] ==64|dat$TankNum[i] ==86|
             dat$TankNum[i] ==115|dat$TankNum[i] ==122|dat$TankNum[i] ==132|dat$TankNum[i]==139){
    dat$treatment[i] <- 7
    } else if (dat$TankNum[i] ==15|dat$TankNum[i] ==39|dat$TankNum[i] ==40|dat$TankNum[i] ==76|
               dat$TankNum[i] ==77|dat$TankNum[i] ==85|dat$TankNum[i] ==91|dat$TankNum[i] ==102|dat$TankNum[i] ==103|
               dat$TankNum[i] ==129){
      dat$treatment[i] <- 8
    } else if (dat$TankNum[i] ==21|dat$TankNum[i] ==27|dat$TankNum[i] ==33|
               dat$TankNum[i] ==52|dat$TankNum[i] ==72|
               dat$TankNum[i] ==75|dat$TankNum[i] ==100|dat$TankNum[i] ==101|
               dat$TankNum[i] ==131|dat$TankNum[i] ==140){
      dat$treatment[i] <- 9
      } else if (dat$TankNum[i] ==12|dat$TankNum[i] ==38|dat$TankNum[i] ==54|
                 dat$TankNum[i] ==68|dat$TankNum[i] ==74|dat$TankNum[i] ==92|dat$TankNum[i] ==106|dat$TankNum[i] ==118|
                 dat$TankNum[i] ==136){
        dat$treatment[i] <- 10
      } else if (dat$TankNum[i] ==4|dat$TankNum[i] ==5|dat$TankNum[i] ==10|dat$TankNum[i] ==23|
                 dat$TankNum[i] ==44|dat$TankNum[i] ==83|
                 dat$TankNum[i] ==84|dat$TankNum[i] ==87|dat$TankNum[i] ==114|
                 dat$TankNum[i] ==121|dat$TankNum[i] ==127){
        dat$treatment[i] <- 11
      } else if (dat$TankNum[i] ==9|dat$TankNum[i] ==28|dat$TankNum[i] ==37|
                 dat$TankNum[i] ==56|dat$TankNum[i] ==67|dat$TankNum[i] ==78|dat$TankNum[i] ==90|
                 dat$TankNum[i] ==113|dat$TankNum[i] ==116|dat$TankNum[i] == 128) {
        dat$treatment[i] <- 12
      } else if (dat$TankNum[i] == 3|dat$TankNum[i] ==17|dat$TankNum[i] ==42|
                 dat$TankNum[i] ==47|dat$TankNum[i] ==60|dat$TankNum[i] ==61|
                 dat$TankNum[i] ==80|dat$TankNum[i] ==95|dat$TankNum[i] ==98|
                 dat$TankNum[i] ==108){
        dat$treatment[i] <- 13
      } else if (dat$TankNum[i] ==2|dat$TankNum[i] ==16|
                 dat$TankNum[i] ==20|dat$TankNum[i] ==43|dat$TankNum[i] ==48|dat$TankNum[i] ==53|dat$TankNum[i] ==65|
                 dat$TankNum[i] ==109|dat$TankNum[i] ==124|dat$TankNum[i] == 134){
        dat$treatment[i] <- 14
      }

}  

pop$treatment <- 0

for (i in 1:nrow(pop)){
  if (pop$TankNum[i] ==19| pop$TankNum[i] == 26| pop$TankNum[i] == 41| 
      pop$TankNum[i] == 45| pop$TankNum[i] ==51| pop$TankNum[i] ==63|
      pop$TankNum[i] ==70| pop$TankNum[i] ==71| pop$TankNum[i] ==89|
      pop$TankNum[i] ==105| pop$TankNum[i] ==133|pop$TankNum[i] ==135){
    pop$treatment[i] <- 1
  } else if (pop$TankNum[i] == 14|pop$TankNum[i] ==25|pop$TankNum[i] ==31|
             pop$TankNum[i] ==62| pop$TankNum[i] ==94| pop$TankNum[i] ==110|
             pop$TankNum[i] ==112| pop$TankNum[i] ==125){
    pop$treatment[i] <- 2
  } else if (pop$TankNum[i] ==1|pop$TankNum[i] ==8|pop$TankNum[i] ==13|
             pop$TankNum[i] ==55|pop$TankNum[i] ==58|pop$TankNum[i] ==66|
             pop$TankNum[i] ==82|pop$TankNum[i] ==99|pop$TankNum[i] ==104|
             pop$TankNum[i] ==130|pop$TankNum[i] ==138
  ){
    pop$treatment[i] <- 3
  } else if (pop$TankNum[i] == 6|pop$TankNum[i] ==29|pop$TankNum[i] ==30|
             pop$TankNum[i] ==50|pop$TankNum[i] ==81|pop$TankNum[i] ==93|
             pop$TankNum[i] ==111|pop$TankNum[i] ==117|pop$TankNum[i] ==126
             
  ) {
    pop$treatment[i] <- 4
  } else if (pop$TankNum[i] ==7|pop$TankNum[i] ==32|pop$TankNum[i] ==36|
             pop$TankNum[i] ==46|pop$TankNum[i] ==49|pop$TankNum[i] ==73|
             pop$TankNum[i] ==88|pop$TankNum[i] ==97|pop$TankNum[i] ==107|
             pop$TankNum[i] ==119|pop$TankNum[i] ==120|pop$TankNum[i] ==137){
    pop$treatment[i] <- 5
  } else if (pop$TankNum[i] ==11|pop$TankNum[i] ==22|pop$TankNum[i] ==35|pop$TankNum[i] ==57|
             pop$TankNum[i] ==69|pop$TankNum[i] ==79|pop$TankNum[i] ==96|pop$TankNum[i] ==123){
    pop$treatment[i] <- 6
  } else if (pop$TankNum[i] ==18|pop$TankNum[i] ==24|pop$TankNum[i] ==34|pop$TankNum[i] ==59|
             pop$TankNum[i] ==64|pop$TankNum[i] ==86|pop$TankNum[i] ==91|pop$TankNum[i] ==102|
             pop$TankNum[i] ==115|pop$TankNum[i] ==122|pop$TankNum[i] ==132|pop$TankNum[i]==139){
    pop$treatment[i] <- 7
  } else if (pop$TankNum[i] ==15|pop$TankNum[i] ==39|pop$TankNum[i] ==40|pop$TankNum[i] ==76|
             pop$TankNum[i] ==77|pop$TankNum[i] ==85|pop$TankNum[i] ==103|pop$TankNum[i] ==129){
    pop$treatment[i] <- 8
  } else if (pop$TankNum[i] ==21|pop$TankNum[i] ==27|pop$TankNum[i] ==33|
             pop$TankNum[i] ==47|pop$TankNum[i] ==52|pop$TankNum[i] ==72|
             pop$TankNum[i] ==75|pop$TankNum[i] ==100|pop$TankNum[i] ==101|
             pop$TankNum[i] ==106|pop$TankNum[i] ==118|pop$TankNum[i] ==131|pop$TankNum[i] ==140){
    pop$treatment[i] <- 9
  } else if (pop$TankNum[i] ==12|pop$TankNum[i] ==38|pop$TankNum[i] ==54|
             pop$TankNum[i] ==68|pop$TankNum[i] ==74|pop$TankNum[i] ==92|
             pop$TankNum[i] ==136){
    pop$treatment[i] <- 10
  } else if (pop$TankNum[i] ==5|pop$TankNum[i] ==10|pop$TankNum[i] ==23|
             pop$TankNum[i] ==44|pop$TankNum[i] ==78|pop$TankNum[i] ==83|
             pop$TankNum[i] ==84|pop$TankNum[i] ==87|pop$TankNum[i] ==114|
             pop$TankNum[i] ==121|pop$TankNum[i] ==127){
    pop$treatment[i] <- 11
  } else if (pop$TankNum[i] ==9|pop$TankNum[i] ==28|pop$TankNum[i] ==37|
             pop$TankNum[i] ==56|pop$TankNum[i] ==67|pop$TankNum[i] ==90|
             pop$TankNum[i] ==113|pop$TankNum[i] ==116|pop$TankNum[i] == 128) {
    pop$treatment[i] <- 12
  } else if (pop$TankNum[i] == 3|pop$TankNum[i] ==17|pop$TankNum[i] ==42|
             pop$TankNum[i] ==43|pop$TankNum[i] ==60|pop$TankNum[i] ==61|
             pop$TankNum[i] ==80|pop$TankNum[i] ==95|pop$TankNum[i] ==98|
             pop$TankNum[i] ==108){
    pop$treatment[i] <- 13
  } else if (pop$TankNum[i] ==2|pop$TankNum[i] ==4|pop$TankNum[i] ==16|
             pop$TankNum[i] ==20|pop$TankNum[i] ==48|pop$TankNum[i] ==53|pop$TankNum[i] ==65|
             pop$TankNum[i] ==109|pop$TankNum[i] ==124|pop$TankNum[i] == 134){
    pop$treatment[i] <- 14
  }
  
}  

