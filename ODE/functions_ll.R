
## calculate log likelihood

dnorm3 <- function(x,mean,log=FALSE) {
  sd <- sqrt(mean((x-mean)^2))
  dnorm(x,mean,sd,log=log)
}


loglik2 <- function (data) {
  times <- dd1$times
  pred <- tt.f1
  ff <- tt.f1[,c(1,3,4)] %>% filter(time %in% pop_fin2$times )
  mm <- tt.f1[, c(1,5)]%>% filter(time %in% dd1$times)
  amm <- dd1 %>% filter(!is.na(ammonium))
  aa <- tt.f1[, c(1,2)]%>% filter(time %in% amm$times)
  sum(dnorm3(x=dd1$algae,mean=mm[,2],log=TRUE))+
    sum(dnorm3(x=amm$ammonium, mean=aa[,2],log=TRUE))+
    sum(dnorm3(x=pop_fin2$Juv, mean=ff[,2], log=TRUE)) +
    sum(dnorm3(x=pop_fin2$Adult, mean=ff[,3],log=TRUE))
    
}  
