filtered.func.data <- read.img("filtered_func_data")

X <- nrow(filtered.func.data)
Y <- ncol(filtered.func.data)
Z <- nsli(filtered.func.data)
W <- ntim(filtered.func.data)
zrange <- range(filtered.func.data)
w <- 1

zstat1 <- read.img("zstat1")

## X <- nrow(zstat1)
## Y <- ncol(zstat1)
## Z <- nsli(zstat1)
## zrange <- range(zstat1)
## 
## par(mfrow=c(5,5), mar=rep(0,4))
## for (z in 1:Z) {
##   image(1:X, 1:Y, zstat1[,,z], zlim=zrange, col=hotmetal(),
##         xlab="", ylab="", axes=FALSE)
## }

par(mfrow=c(5,5), mar=rep(0,4))
for (z in 1:Z) {
  image(1:X, 1:Y, filtered.func.data[,,z,w], zlim=zrange,
        col=grey(0:64/64), xlab="", ylab="", axes=FALSE)
  image(1:X, 1:Y, ifelse(abs(zstat1[,,z]) > 4, zstat1[,,z], NA),
        zlim=range(zstat1), col=hotmetal(), add=TRUE)
}
