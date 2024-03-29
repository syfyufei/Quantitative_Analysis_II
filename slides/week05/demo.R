

setwd("d:/baidu/tsinghua/courses/quantII/week04")
par(mar=c(3,3,0,0), mgp=c(1.5,0.5,0),tcl=-0.2)
curve(dnorm(x, mean=0, sd=1), from=-7, to=7, axes=FALSE, yaxs="i", xaxs="i",
    xlab="", ylab="", lwd=3)
axis(1)
axis(2)
curve(dlogis(x), lwd=3, col=2, add=TRUE)
curve(dnorm(x, mean=0, sd=1.6), lwd=3, col=4, add=TRUE)
curve(dnorm(x, mean=0, sd=(pi/sqrt(3))), lwd=3, col=3, add=TRUE)
abline(v=0, lty=2)
arrows(x0=4, y=0.1, x1=2.1, y1=0.1, lwd=3, col=2)
arrows(x0=-4, y=0.05, x1=-2.1, y1=0.05, lwd=3, col=1)
arrows(x0=-4, y=0.1, x1=-2.2, y1=0.1, lwd=3, col=1)
text(x=4, y=0.1, labels="logistic distribution", col=2, xpd=NA, adj=0)
text(x=-4, y=0.05, labels="N(0,1)", col=1, xpd=NA, adj=1)
text(x=4, y=0.1, labels="logistic distribution", col=2, xpd=NA, adj=0)
text(x=-4, y=0.05, labels="N(0,1)", col=1, xpd=NA, adj=1)
text(x=-4, y=0.1, labels="N(0,1.6)", col=4, xpd=NA, adj=1)
text(x=4, y=0.05, labels="N(0,1.8)", col=3, xpd=NA, adj=0)
arrows(x0=4, y=0.05, x1=3.2, y1=0.05, lwd=3, col=3)

par(mar=c(3,3,3,0), mgp=c(1.5,0.5,0),tcl=-0.2)
curve(dlogis(x), lwd=2, from=-7, to=7, axes=FALSE, yaxs="i", xaxs="i",
    xlab="", ylab="", main=expression("X"*beta*"=0"))
axis(1)
segments(-3, 0, -3, dlogis(-3))
text(-3, -0.01, expression(c[1]), xpd=NA, col=2)
segments(-1, 0, -1, dlogis(-1))
text(-1, -0.01, expression(c[2]), xpd=NA, col=2)
segments(1.5, 0, 1.5, dlogis(1.5))
text(1.5, -0.01, expression(c[3]), xpd=NA, col=2)
