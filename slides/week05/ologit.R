setwd("D:/baidu/Tsinghua/Courses/QuantII/week05/")

library(foreign)
dat <- read.dta("E:/SynologyDrive/Github/Quantitative_Analysis_II/slides/week05/cgss2010short.dta")
summary(dat)
male <- ifelse(dat$a2=="男", 1, 0)

a3a <- ifelse(dat$a3a < 17, NA, dat$a3a)
age <- 2010-a3a

table(dat$a10)

ccpmember <- ifelse(dat$a10=="共产党员", 1, 0)

a36 <- as.numeric(dat$a36)

happy <- ifelse(a36<3, NA, a36-2)

a62 <- ifelse(dat$a62 > 9999996, NA, dat$a62)

probs <- c(0, 0.07, 0.25, 0.5, 0.75, 0.93, 1)
kpts <- quantile(a62, prob=probs, na.rm=TRUE)
hinc <- as.numeric(cut(a62, breaks=kpts, labels = 1:6, right=TRUE))

kpts2 <- quantile(a62, prob=c(0,1/6,2/6,3/6,4/6,5/6,1), na.rm=TRUE)
hinc2 <- as.numeric(cut(a62, breaks=kpts2, labels = 1:6, right=TRUE))

loghinc <- log(a62+1)

dat2 <- cbind.data.frame(happy, male, age, ccpmember, hinc, loghinc)
dat3 <- na.exclude(cbind.data.frame(happy, male, age, ccpmember, hinc, loghinc))


library(MASS)
M0 <- polr(ordered(happy)~1, method = "logistic", data=dat3)
summary(M0)
M1 <- polr(ordered(happy) ~ male + age + ccpmember + hinc, data=dat3,  method = "logistic", Hess=TRUE)
summary(M1)

# exp(coef(M1))

nullDev <- deviance(M0)
dev <- deviance(M1)
pR2 <- (nullDev-dev)/nullDev

LR <- nullDev - dev
k <- length(coef(M1))
prob <- pchisq(LR, df=k-1, lower.tail = FALSE)

# AIC(M1)  没有考虑自变量
# BIC(M1)  自变量数很多的情况

methods(predict)
getAnywhere(predict.polr)
phatMat <- predict(M1, newdata=dat3, type = "probs")
xMat <- with(dat3, cbind(male, age, ccpmember, hinc))
z <- xMat%*%as.matrix(coef(M1))
resids <- dat3$happy - z
#sd(z) 应该等于1.81 N(0, 1.81)
hist(resids)

library(brant)
brant(M1)

M2 <- polr(ordered(happy) ~ male + age + ccpmember + loghinc, data=dat2,  method = "logistic")
summary(M2)
brant(M2)

library(VGAM)  #进阶写法
M3.1 <- vglm(ordered(happy) ~ male + age + ccpmember + loghinc, cumulative(link = "logitlink", parallel = TRUE ~ -1 + male), data=dat2)
summary(M3.1)
M3.2 <- vglm(ordered(happy) ~ male + age + ccpmember + loghinc, cumulative(link = "logitlink", parallel = FALSE ~ 1 + age + ccpmember + loghinc), data=dat2)
summary(M3.2)


newDat <- cbind.data.frame(male = rep(1, 1000), age=rep(40, 1000), ccpmember = rep(1, 1000), loghinc = seq(0,16, length=1000))
phat <- predict(M2, newdata = newDat, type="probs")

par(mar=c(3,3,1,5), mgp=c(1.5,0.2,0), tcl=-0.2)
plot(0,0, ylim=c(0,1), xlim=range(loghinc, na.rm=TRUE), type="n",
     ylab="predictive probabilities", xlab="log(househould income)", xaxs="i", yaxs="i")
for(i in 1:5){
    lines(x=newDat$loghinc, y=phat[,i], col=i)
}
for(i in 1:5){
    text(x=16, y=phat[1000, i], labels=paste("Pr(happy=", i, ")", sep=""), xpd=NA, adj=0, col=i)
}
legend("topright", col=1:5, lty=1, legend=paste("Pr(happy=", 1:5, ")", sep=""))
# legend(locator(1), col=1:5, lty=1, legend=paste("Pr(happy=", 1:5, ")", sep=""))


cumSum <- function(pmat){
    K <- dim(pmat)[2]
    cpmat <- pmat
    for(i in 2:K){
        cpmat[,i] <- cpmat[,i] + cpmat[,i-1]
    }
    return(cpmat)
}

#累积图

cphat <- cumSum(phat)
cphat2 <- t(apply(phat, 1, cumsum))
par(mar=c(3,3,1,5), mgp=c(1.5,0.2,0), tcl=-0.2)
plot(0,0, ylim=c(0,1), xlim=range(loghinc, na.rm=TRUE), type="n",
    ylab="cumulative predictive probabilities", xlab="log(househould income)", xaxs="i", yaxs="i")
for(i in 1:5){
    lines(x=newDat$loghinc, y=cphat[,i], col=i)
}
text(x=16, y=cphat[1000, 1], labels=paste("Pr(happy=1", ")", sep=""), xpd=NA, adj=0, col=1)
for(i in 2:5){
    text(x=16, y=cphat[1000, i], labels=paste("Pr(", i, ">", "happy>=", i-1, ")", sep=""), xpd=NA, adj=0, col=i)
}


a36 <- as.numeric(dat$a36)
happy <- ifelse(a36<3, NA, a36-2)
happy <- ifelse(a36 %in% c(3,4), 1,
        ifelse(a36 %in% c(6, 7), 3,
        ifelse(a36 == 5, 2, NA)))
dat4 <- cbind.data.frame(happy, male, age, ccpmember, hinc, loghinc)
M3 <- polr(ordered(happy) ~ male + age + ccpmember + loghinc, data=dat4,  method = "logistic")
summary(M3)

brant(M3)
# 过了俩

phat <- predict(M3, newdata = newDat, type="prob")
par(mar=c(3,3,1,5), mgp=c(1.5,0.2,0), tcl=-0.2)
plot(0,0, ylim=c(0,1), xlim=range(loghinc, na.rm=TRUE), type="n",
    ylab="predictive probabilities", xlab="log(househould income)", xaxs="i", yaxs="i")
for(i in 1:3){
    lines(x=newDat$loghinc, y=phat[,i], col=i)
}
for(i in 1:3){
    text(x=16, y=phat[1000, i], labels=paste("Pr(happy=", i, ")", sep=""), xpd=NA, adj=0, col=i)
}
legend("topleft", col=1:3, lty=1, legend=paste("Pr(happy=", 1:3, ")", sep=""))


cphat <- cumSum(phat)
par(mar=c(3,3,1,5), mgp=c(1.5,0.2,0), tcl=-0.2)
plot(0,0, ylim=c(0,1), xlim=range(loghinc, na.rm=TRUE), type="n",
    ylab="cumulative predictive probabilities", xlab="log(househould income)", xaxs="i", yaxs="i")
for(i in 1:3){
    lines(x=newDat$loghinc, y=cphat[,i], col=i)
}
text(x=16, y=cphat[1000, 1], labels=paste("Pr(happy=1", ")", sep=""), xpd=NA, adj=0, col=1)
for(i in 2:3){
    text(x=16, y=cphat[1000, i], labels=paste("Pr(", i, ">", "happy>=", i-1, ")", sep=""), xpd=NA, adj=0, col=i)
}


exp(coef(M2))

