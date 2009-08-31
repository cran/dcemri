data("buckley")

par(mfrow=c(2,2), mar=c(5,4,4,2))
matplot(buckley$time.min, breast$data[,1:5], type="l", ylim=c(0,1),
        col=1, lty=1, lwd=1.5, xlab="time (min)",
        ylab="SI increase (arb. units)", main="Changes in plasma flow")
lines(buckley$time.min, breast$data[,3], lwd=2)
matplot(buckley$time.min, breast$data[,6:9], type="l", ylim=c(0,1),
        col=1, lty=1, lwd=1.5, xlab="time (min)",
        ylab="SI increase (arb. units)",
        main="Changes in permeability-surface\narea product")
lines(buckley$time.min, breast$data[,3], lwd=2)
matplot(buckley$time.min, breast$data[,10:13], type="l", ylim=c(0,1),
        col=1, lty=1, lwd=1.5, xlab="time (min)",
        ylab="SI increase (arb. units)", main="Changes in plasma volume")
lines(buckley$time.min, breast$data[,3], lwd=2)
plot(buckley$time.min, buckley$input, type="l", lwd=1.5,
     xlab="time (min)", ylab="", main="Arterial Input Function")

par(mfrow=c(2,2), mar=c(5,4,4,2))
matplot(buckley$time.min, meningioma$data[,1:5], type="l",
        ylim=c(0,1.1), col=1, lty=1, lwd=1.5, xlab="time (min)",
        ylab="SI increase (arb. units)", main="Changes in plasma flow")
lines(buckley$time.min, meningioma$data[,3], lwd=2)
matplot(buckley$time.min, meningioma$data[,6:9], type="l",
        ylim=c(0,1.1), col=1, lty=1, lwd=1.5, xlab="time (min)",
        ylab="SI increase (arb. units)",
        main="Changes in permeability-surface\narea product")
lines(buckley$time.min, meningioma$data[,3], lwd=2)
matplot(buckley$time.min, meningioma$data[,10:13], type="l",
        ylim=c(0,1.1), col=1, lty=1, lwd=1.5, xlab="time (min)",
        ylab="SI increase (arb. units)", main="Changes in plasma volume")
lines(buckley$time.min, meningioma$data[,3], lwd=2)
plot(buckley$time.min, buckley$input, type="l", lwd=1.5,
     xlab="time (min)", ylab="", main="Arterial Input Function")

