setwd("D:/baidu/Tsinghua/Courses/QuantII/week11/")
library(foreign)
dat <- read.dta("temp.dta")

attach(dat)

M1 <- lm(temp ~ year, data=dat)

pdf("fig04.pdf", width=5, height=3.5)
par(mar=c(3,3,1,1), mgp=c(2,0.2,0),tcl=-0.2)
plot(x=year, y=temp, type="l", col=4)
abline(lm(temp~year), col=2)
dev.off()

res <- residuals(M1)
yhat <- fitted(M1)

pdf("fig05.pdf", width=5, height=3.5)
par(mar=c(3,3,1,1), mgp=c(2,0.2,0),tcl=-0.2)
plot(x=yhat, y=res, ylim=c(-0.6, 0.6), xlab="fitted values", ylab="residuals")
abline(h=0, lty=2, col=2)
#lines(lowess(res ~ yhat, f=1/3), col=4)
dev.off()

pdf("fig06.pdf", width=5, height=3.5)
par(mar=c(3,3,1,1), mgp=c(2,0.2,0),tcl=-0.2)
plot(x=yhat, y=res, ylim=c(-0.6, 0.6), xlab="fitted values", ylab="residuals")
abline(h=0, lty=2, col=2)
lines(lowess(res ~ yhat, f=1/3), col=4)
dev.off()

library(tseries)
runs.test(as.factor(sign(res)))


chk <- res>0
n <- length(chk)
r <- 1+ sum(chk[-1] !=chk[-n])
n1 <- sum(chk)
n2 <- n-sum(chk)
mu <- (2*n1*n2/(n1+n2))+1
s <- sqrt((2*n1*n2*(2*n1*n2-n1-n2))/((n1+n2)^2*(n1+n2-1)))
z <- (r - mu)/s
z
2*pnorm(-abs(z))

pdf("fig07.pdf", width=5, height=3.5)
par(mar=c(3,3,1,1), mgp=c(2,0.2,0),tcl=-0.2)
print(acf(temp, lag.max=15))
dev.off()
pdf("fig08.pdf", width=5, height=3.5)
par(mar=c(3,3,1,1), mgp=c(2,0.2,0),tcl=-0.2)
print(pacf(temp, lag.max=15))
dev.off()

M1 <- lm(temp ~ year, data=dat)
res <- residuals(M1)
alpha <- cor(res[2:n], res[1:(n-1)])
alpha


yearF <- with(dat, year - alpha*c(NA, year[1:(n-1)]))
tempF <- with(dat, temp - alpha*c(NA, temp[1:(n-1)]))
M2 <- lm(tempF ~ yearF)

turkey <- read.csv("turkey.csv")
turkey <- na.exclude(turkey)
n <- dim(turkey)[1]

M3 <- lm(polity ~ gdp + open, data=turkey)
summary(M3)
res <- residuals(M3)
alpha <- cor(res[2:n], res[1:(n-1)])

pdf("turkeyAC.pdf", width=5, height=3.5)
par(mar=c(3,3,1,1), mgp=c(2,0.2,0),tcl=-0.2)
acf(turkey$polity)
dev.off()

pdf("turkeyPAC.pdf", width=5, height=3.5)
par(mar=c(3,3,1,1), mgp=c(2,0.2,0),tcl=-0.2)
pacf(turkey$polity)
dev.off()

pdf("turkeyCC.pdf", width=5, height=3.5)
par(mar=c(3,3,3,1), mgp=c(2,0.2,0),tcl=-0.2)
ccf(x=turkey$gdp, y=turkey$polity)
dev.off()

attach(turkey)
polF <- polity - alpha*c(NA, polity[1:(n-1)])
gdpF <- gdp - alpha*c(NA, gdp[1:(n-1)])
openF <- open - alpha*c(NA, open[1:(n-1)])

M4 <- lm(polF ~ gdpF + openF)
summary(M4)
