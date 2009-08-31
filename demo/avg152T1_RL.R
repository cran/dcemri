avg152T1.RL <- read.img("avg152T1_RL_nifti")

X <- nrow(avg152T1.RL)
Y <- ncol(avg152T1.RL)
Z <- nsli(avg152T1.RL)
zrange <- range(avg152T1.RL)

par(mfrow=c(10,10), mar=rep(0,4))
for (z in 1:Z) {
  image(1:X, 1:Y, avg152T1.RL[X:1,,z], zlim=zrange, col=grey(0:64/64),
        xlab="", ylab="", axes=FALSE)
}

