avg152T1 <- read.img("avg152T1")

X <- nrow(avg152T1)
Y <- ncol(avg152T1)
Z <- nsli(avg152T1)
zrange <- range(avg152T1)

par(mfrow=c(10,10), mar=rep(0,4))
for (z in 1:Z) {
  image(1:X, 1:Y, avg152T1[,,z,1], zlim=zrange, col=grey(0:64/64),
        xlab="", ylab="", axes=FALSE)
}

