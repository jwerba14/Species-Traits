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

for (i in 2:nrow(ch2)) {
  ch2$prop[i] <- ch2$X5[i]/ch2$X5[i-1] 
}
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
ch3 <- ch2[,-c(31,32)]
newdat <- data.frame(
  chlorp = numeric(0),
  ammonia = numeric(0),
  repn = numeric(0)
)
for (i in 1:ncol(ch3)) {
  newdat <- rbind(newdat, data.frame(
    chlorp = ch3[-1, i] / ch3[-length(ch3[ ,i]), i],
    ammonia = nh2[-length(nh2[ ,i]), i],
    repn = rep(i, 10)
  ))
}

ggplot(newdat[newdat$chlorp < 4, ], aes(ammonia, chlorp)) + geom_point() + theme_bw() +
  geom_smooth()

ggplot(newdat[newdat$ammonia < 30 & newdat$chlorp < 2.5, ], aes(ammonia, chlorp)) + geom_point() + theme_bw() 


test_dat <- data.frame(day = seq(1, 20, by = 1),
           amount = c(40, rep(0, 19)))
for(i in 2:nrow(test_dat)) {
  test_dat$amount[i] = test_dat$amount[i - 1] * 1.5
}
plot(test_dat)
test_dat[11, ]

