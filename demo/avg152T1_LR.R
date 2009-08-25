avg152T1.LR <- read.img("avg152T1_LR_nifti")

X <- nrow(avg152T1.LR)
Y <- ncol(avg152T1.LR)
Z <- nsli(avg152T1.LR)
zrange <- range(avg152T1.LR)

par(mfrow=c(10,10), mar=rep(0,4))
for (z in 1:Z) {
  image(1:X, 1:Y, avg152T1.LR[,,z], zlim=zrange, col=grey(0:64/64),
        xlab="", ylab="", axes=FALSE)
}

