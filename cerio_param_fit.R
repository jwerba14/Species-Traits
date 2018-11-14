source(nh4_prac)
cerio <- read.csv("cerio_pop.csv")
cerio["Population"][is.na(cerio["Population"])] <- 1

cerio1 <- cerio[cerio$Rep==1,]

mod <- nls(Population~logist(r=r,k=k,t=Day,a0=3.35),data = cerio1,start = list(r=1,k=50))

cerio2 <- cerio[cerio$Rep==12,]

mod2 <- nls(Population~logist(r=r,k=k,t=Day,a0=3.35),data = cerio2,start = list(r=.1,k=5))

fit_nls <- function(subdat, par_names=c("r","k")) {
  
  nls_fit <- try(
    nls(chl~logist(r=r,k=k,t=date1,a0=start_chl),data=subdat,start = list(r=1,k=50)),
    silent = TRUE
  )
  
  p = 1
  while (class(nls_fit) == "try-error") {
    start_r <- rlnorm(1, -2, 1)
    start_k <- rlnorm(1, 3, 3)
    
    nls_fit <- try(
      nls(chl~logist(r=r,k=k,t=date1,a0=start_chl),data=subdat,start = list(r=start_r,k=start_k)),
      silent = TRUE
    )
    
    p = p + 1
    
    if (p > 100) { break }
    
  }
  
  col_names <- c(outer(par_names,c("est","se"),paste,sep="_"))
  if(class(nls_fit) == "try-error") {
    pp <- rep(NA,4) ## FIXME: get the right number of NAs programmatically
    ## FIXME: will fail if first group is bad
  } else {
    coef_tab <- coef(summary(nls_fit))
    pp <- c(coef_tab[,c("Estimate","Std. Error")])
  }
  ## collapse results to a one-row matrix
  dd <- data.frame(matrix(pp,nrow=1))
  names(dd) <- col_names
  return(dd)
}



c2 <- cerio %>%
  group_by(Rep) %>%
  mutate(start_chl = Avg_Chl[1])  ## this is a bit weird and not right bc should be constant throughout so maybe just have too many parameters

ults <- c2 %>% do(fit_nls(.))
