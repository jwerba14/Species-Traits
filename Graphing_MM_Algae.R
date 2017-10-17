library(ggplot2)
ch2 <- data.frame(ch1)
ch2$time <- seq(1,11)
ggplot(ch2, aes(time,X1))+geom_point()
ggplot(ch2, aes(time,X2))+geom_point()
ggplot(ch2, aes(time,X3))+geom_point()
ggplot(ch2, aes(time,X4))+geom_point()
ggplot(ch2, aes(time,X5))+geom_point()
ggplot(ch2, aes(time,X6))+geom_point()
ggplot(ch2, aes(time,X7))+geom_point()
ggplot(ch2, aes(time,X8))+geom_point()
ggplot(ch2, aes(time,X9))+geom_point()
ggplot(ch2, aes(time,X10))+geom_point()
ggplot(ch2, aes(time,X11))+geom_point()
ggplot(ch2, aes(time,X12))+geom_point()
ggplot(ch2, aes(time,X13))+geom_point()
ggplot(ch2, aes(time,X14))+geom_point()
ggplot(ch2, aes(time,X15))+geom_point()


# want to graph daily percent change
#for (j in 1:ncol(ch2)) {
  
#for (i in 2:nrow(ch2)) {
  #ch2$prop[i] <- ch2[i,j]/ch2[i-1,j] 
#}
#}
# ch2$X5[-1] / ch2$X5[-length(ch2$X5)]


ggplot(ch2, aes(time,prop))+geom_point()
 
#want to graph percent change against nh4
nh2 <- data.frame(nh1)
colnames(nh2)<- c("n1","n2","n3","n4","n5","n6","n7","n8","n9","n10",
                  "n11","n12","n13","n14","n15","n16","n17","n18","n19","n20","n21","n22","n23","n24","n25",
                  "n26","n27","n28","n29","n30")

all <- cbind(ch2,nh2)

ggplot(all, aes(n5,prop))+geom_point()
ggplot(all, aes(n5,X5))+geom_point()

### get proportional change for every replicate 
# loop over column each time rowbind so
ch3 <- ch2[,-c(31)]
newdat <- data.frame(
  chlorp = numeric(0),
  ammonia = numeric(0),
  repn = numeric(0)
)
for (i in 1:ncol(ch3)) {
  newdat <- rbind(newdat, data.frame(
   #chlorp = ch3[-1, i] / ch3[-length(ch3[ ,i]), i],
  #  chlorp = (ch3[-1, i] - ch3[-length(ch3[ ,i]), i]) / ch3[-length(ch3[ ,i]), i],
   chlorp = (ch3[-1, i] + death2(.00002,ch3[-length(ch3[ ,i]),i],0.5)*ch3[-length(ch3[,i]),i])/ 
    ch3[-length(ch3[ ,i]), i] - 1,
   #death(0.25,10,ch3[-length(ch3[ ,i]),i])
    ammonia = nh2[-length(nh2[ ,i]), i],
    repn = rep(i, 10)
  ))
}

gg1 <- ggplot(newdat[newdat$chlorp < 3, ], aes(ammonia, chlorp)) + geom_point() + theme_bw() +
  geom_smooth()

ggplot(newdat[newdat$ammonia < 30 & newdat$chlorp < 2.5, ], aes(ammonia, chlorp)) + geom_point() + theme_bw() 

## quick MM parameter fit

## add constant death (e.g 0.5)
library(nlmrt)
newdat_mmfit <- newdat
newdat_mmfit$chlorp <- newdat_mmfit$chlorp 
newdat_mmfit1 <- newdat_mmfit[newdat_mmfit$ammonia < 30,]
nls_fit <- nlxb(chlorp ~ a * ammonia / (s + ammonia),
     start = c(a = 2, s = 0.5),
     data = newdat_mmfit)

nls_fit_dat <- data.frame(
  ammonia = seq(0.001, 60, by = 0.1),
  chlorp = mikmen(nls_fit$coefficients[1], nls_fit$coefficients[2], seq(0.001, 60, by = 0.1)))

ggplot(newdat_mmfit[newdat_mmfit$chlorp < 3, ], aes(ammonia, chlorp)) + geom_point() +
  theme_bw() + geom_line(data = nls_fit_dat, lwd = 2, col = "blue")

## What this tells me is that the death process can't be constant, because it doesn't
## allow for parameters for the MM function to fit the data that make biological
## sense. If death were constant but higher, then maybe the MM parameters make more
## sense but the death process doesn't make much sense



## Look at growth given daily proportional change
test_dat <- data.frame(day = seq(1, 20, by = 1),
           amount = c(40, rep(0, 19)))
for(i in 2:nrow(test_dat)) {
  test_dat$amount[i] = test_dat$amount[i - 1] * 1.5
}
plot(test_dat)
test_dat[11, ]

