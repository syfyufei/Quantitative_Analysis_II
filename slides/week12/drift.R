
n <- 500
y <- rep(NA, n+1)
y[1] <- 0
delta=2
u <- rnorm(n,0,1)
for(i in 2:(n+1)){
    y[i] <- y[i-1] + delta + u[i]
}
y1 <- y[2:(n+1)]


for(i in 2:(n+1)){
    y[i] <- y[i-1] + u[i]
}
y2 <- y[2:(n+1)]



par(mfrow=c(1,2), mar=c(3,3,3,1), mgp=c(1.5,0.2,0), tcl=-0.2)
plot(1:500, y1, type="l", ylab="Y", xlab="Time", main="Random Walk with Drift")
plot(1:500, y2, type="l", ylab="Y", xlab="Time", main="Random Walk without Drift")
abline(h=0, lty=2)



n <- 500
y <- rep(NA, n+1)
y[1] <- 0
delta=2
u <- rnorm(n,0,1)
for(i in 2:(n+1)){
    y[i] <- y[i-1] + u[i]
}
y <- y[2:(n+1)]

n <- 500
x <- rep(NA, n+1)
x[1] <- 0
delta=2
u <- rnorm(n,0,1)
for(i in 2:(n+1)){
    x[i] <- x[i-1] + u[i]
}
x <- x[2:(n+1)]


M1 <- lm(y ~ x)
library(lmtest)
dwtest(M1)

dwTest <- function(fit){
    res <- residuals(fit)
    n <- length(res)
    d <- sum((res[2:n] - res[1:(n-1)])^2)/sum(res^2)
    return(d)
}

library(tseries)
adf.test(na.exclude(y))




n <- 50
y <- rep(NA, n+1)
y[1:2] <- 0
mu <- 2
beta1 <- 0.3
beta2 <- 0.6
u <- rnorm(n,0,1)
for(i in 2:(n+1)){
    y[i] <- mu + beta1 * u[i] + beta2*u[i-1]
}
y1 <- y[2:(n+1)]

n <- 50
y <- rep(NA, n+2)
y[1:2] <- 0
mu <- 2
beta1 <- 0.3
beta2 <- 0.6
beta3 <- 0.7
for(i in 3:(n+2)){
    y[i] <- mu + beta1 * u[i] + beta2*u[i-1] + beta3*u[i-2]
}
y2 <- y[3:(n+2)]

par(mfrow=c(1,2), mar=c(3,3,3,1), mgp=c(1.5,0.2,0), tcl=-0.2)
plot(1:50, y1, type="l", ylab="Y", xlab="Time", main="MA(1)")
plot(1:50, y2, type="l", ylab="Y", xlab="Time", main="MA(2)")


M01 <- arima(y, order=c(1,2,1))
M02 <- arima(y, order=c(1,1,1))
AIC(M01)
AIC(M02)
