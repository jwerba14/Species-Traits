## set up for fitting loglik and graphing full ODE
source("../treatments.R")

## correct for blank flourimeter readings
blank <- read.csv("../Master_Data/Blank_algae.csv")
str(blank)
blank <- separate(blank, Date, c("Month", "Day"), sep = " ")
blank$Day <- as.numeric(as.character(blank$Day))

newdat <- left_join(dat, blank)
dim(newdat)
newdat$Blank.Chl[is.na(newdat$Blank.Chl)] <- 0

newdat$adj_chl <- newdat$Chl - newdat$Blank.Chl

## remove days when flourimeter was brooken

newdat <- newdat %>% filter(!(Month == "February"& Day == 9)) %>% 
  filter(!(Month == "February"& Day == 10)) %>%
  filter(!(Month == "February"& Day == 11)) %>% 
  filter(!(Month == "February"& Day == 12))

newdat <- newdat %>% dplyr::select(-c(X,X.1,X.2))

## remove ceriodaphnia treatments
dat <- newdat %>% filter(! treatment %in% c(5,6,9,10,13,14))
pop <- pop %>% filter(! treatment %in% c(5,6,9,10,13,14)) 
pop <- pop[-121, ] ## some reason accidentally entered the same row of data 2x

dd <- dat %>% dplyr:: select(ExptDay, NH4, Chl, TankNum, treatment)
names(dd) <- c("times", "ammonium","algae", "TankNum", "treatment")

dd$ammonium <- dd$ammonium * 1000 ## change to same scale as Chl

popd <- pop %>% dplyr::select(DaphniaC3,ExptDay, DaphniaC1Adult, DaphniaC1Juv, DaphniaC2Adult,
                  DaphniaC2Juv, DaphniaC3Juv, DaphniaC4Adult, DaphniaC4Juv, treatment, TankNum)
names(popd) <- c("Daphnia_C3_Adult","ExptDay", "Daphnia_C1_Adult", "Daphnia_C1_Juv", "Daphnia_C2_Adult",
                 "Daphnia_C2_Juv", "Daphnia_C3_Juv", "Daphnia_C4_Adult", "Daphnia_C4_Juv", "treatment", "TankNum")

popd <- popd %>% pivot_longer(-c(TankNum,ExptDay,treatment), names_to = "stage", values_to = "countD")


popB <- popd %>% separate(stage, into=c("Daphnia","subsample","stage"),sep ='_', fill = "right" )


pop_fin <- popB %>% dplyr::select(-Daphnia) %>% pivot_wider(everything(), names_from = stage, values_from = countD)

names(pop_fin)[1] <- "times"

pop_fin1 <- pop_fin %>% group_by(times,TankNum,treatment) %>% summarize(Adult = mean(Adult), Juv = mean(Juv))




