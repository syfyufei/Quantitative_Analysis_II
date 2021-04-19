setwd("d:/baidu/tsinghua/courses/quantii/hwk04")
library(haven)
dat <- read_dta("cgss2010.dta")

summary(dat)
male <- ifelse(dat$a2==1, 1,
    ifelse(dat$a2==2, 0, NA))
a3a <- ifelse(dat$a3a < 17, NA, dat$a3a)
age <- 2010-a3a
a62 <- ifelse(dat$a62 > 9999996, NA, dat$a62)
probs <- c(0, 0.07, 0.25, 0.5, 0.75, 0.93, 1)
kpts <- quantile(a62, prob=probs, na.rm=TRUE)
hinc <- as.numeric(cut(a62, breaks=kpts, labels = 1:6, right=TRUE))
kpts2 <- quantile(a62, prob=c(0,1/6,2/6,3/6,4/6,5/6,1), na.rm=TRUE)
hinc2 <- as.numeric(cut(a62, breaks=kpts2, labels = 1:6, right=TRUE))
loghinc <- log(a62+1)

a35 <- ifelse(dat$a35<0, NA, dat$a35)

dat1 <- cbind.data.frame("fair"=factor(a35), male, age, loghinc)
dat2 <- na.exclude(dat1)
library(MASS)
M1 <- polr(ordered(fair) ~ male + age + loghinc, data=dat2, method = "logistic")
M0 <- update(M1, .~1, data=dat2)


# pseudo R2
nullDev <- deviance(M0)
dev <- deviance(M1)

pR2 <- (nullDev-dev)/nullDev

# likelihood ratio test
LR <- nullDev - dev
k <- length(coef(M1))
prob <- pchisq(LR, df=k-1, lower.tail = FALSE)


# odds ratio
exp(coef(M1))

# predictive difference between gender
newX <- with(dat2, cbind.data.frame(male = c(0,1), age = mean(age), loghinc = mean(loghinc)))
phat <- predict(M1, newdata=newX, type = "probs")
phat[2,] - phat[1,]


# predictive probs curves along income
newX2 <- with(dat2, cbind.data.frame(male = rep(1, 1000), age = rep(40, 1000), loghinc = seq(0,16,length=1000)))

phat2 <- predict(M1, newdata = newX2, type="probs")

par(mar=c(3,3,1,5), mgp=c(1.5,0.2,0), tcl=-0.2)
plot(0,0, ylim=c(0,1), xlim=range(loghinc, na.rm=TRUE), type="n",
    ylab="predictive probabilities", xlab="log(househould income)", xaxs="i", yaxs="i")
for(i in 1:5){
    lines(x=newX2$loghinc, y=phat2[,i], col=i)
}
for(i in 1:5){
    text(x=16, y=phat2[1000, i], labels=paste("Pr(Fair=", i, ")", sep=""), xpd=NA, adj=0, col=i)
}
legend("topright", col=1:5, lty=1, legend=paste("Pr(Fair=", 1:5, ")", sep=""))
legend(locator(1), col=1:5, lty=1, legend=paste("Pr(Fair=", 1:5, ")", sep=""))

# brant test
library(brant)
brant(M1)


# multinomial logistic regression
library(nnet)
M3 <- multinom(factor(fair) ~ male + age + loghinc, data=dat2)
M4 <- update(M3, .~1, data=dat2)


# pseudo R2
nullDev <- deviance(M4)
dev <- deviance(M3)

pR2 <- (nullDev-dev)/nullDev

# likelihood test
LR <- nullDev - dev
k <- length(coef(M3))
prob <- pchisq(LR, df=k-1, lower.tail = FALSE)

# relative ratio test
exp(coef(M3))


# predictive probs between gender
newX <- with(dat2, cbind.data.frame(male = c(0,1), age = mean(age), loghinc = mean(loghinc)))
phat <- predict(M3, newdata=newX, type = "probs")
phat[2,] - phat[1,]

# predictive probs curves along income
newX2 <- with(dat2, cbind.data.frame(male = rep(1, 1000), age = rep(40, 1000), loghinc = seq(0,16,length=1000)))

phat2 <- predict(M3, newdata = newX2, type="probs")

par(mar=c(3,3,1,5), mgp=c(1.5,0.2,0), tcl=-0.2)
plot(0,0, ylim=c(0,1), xlim=range(loghinc, na.rm=TRUE), type="n",
    ylab="predictive probabilities", xlab="log(househould income)", xaxs="i", yaxs="i")
for(i in 1:5){
    lines(x=newX2$loghinc, y=phat2[,i], col=i)
}
for(i in 1:5){
    text(x=16, y=phat2[1000, i], labels=paste("Pr(Fair=", i, ")", sep=""), xpd=NA, adj=0, col=i)
}
legend("topright", col=1:5, lty=1, legend=paste("Pr(Fair=", 1:5, ")", sep=""))


# hausmen test
hmTest <- function(formula, data, choiceVar, keepLev, baseLev){
    library(nnet)
    lev <- keepLev[keepLev != baseLev]
    fitF <- multinom(formula, data=data, Hess=TRUE)
    subDat <- subset(data, subset = data[,choiceVar] %in% keepLev)
    fitR <- multinom(formula, data = subDat, Hess=TRUE)
    betaF <- coef(fitF)
    covF <- vcov(fitF)
    betaR <- coef(fitR)
    covR <- vcov(fitR)
    uu <- colnames(betaF)
    uu <- uu[order(uu)]
    K <- length(betaR)
    levLabs <- sort(rep(lev, length(uu)))
    uuu <- paste(levLabs, uu, sep=":")
    betaDiff <- betaR[as.character(lev),uu, drop=TRUE] - betaF[as.character(lev),uu, drop=TRUE]
    betaDiff <- unlist(split(betaDiff, row(betaDiff), drop=TRUE))
    varDiff <- covR[uuu,uuu] - covF[uuu,uuu]
    hm <- t(betaDiff) %*% solve(varDiff) %*% betaDiff
    prob <- pchisq(hm, df=K, lower.tail=FALSE)
    out <- list("chisq" = hm[1], "df" = K, "pValue" = prob[1])
    if(prob > 0.05){
        cat("\nHausman_McFadden test\nH0: IIA assumption is accepted\n")
    } else {
        cat("\nHausman_McFadden test\nH0: IIA assumption is rejected\n")
    }
    return(out)
}

hmTest(factor(fair) ~ male + age + loghinc, data=dat2, choiceVar ="fair", keepLev = c(1, 2,3,4), baseLev = 1)
hmTest(factor(fair) ~ male + age + loghinc, data=dat2, choiceVar ="fair", keepLev = c(1, 2,3,5), baseLev = 1)
hmTest(factor(fair) ~ male + age + loghinc, data=dat2, choiceVar ="fair", keepLev = c(1, 3,4,5), baseLev = 1)
hmTest(factor(fair) ~ male + age + loghinc, data=dat2, choiceVar ="fair", keepLev = c(1, 2,4,5), baseLev = 1)




# Small and Hsiao Test

smhsiaoTest <- function(formula, data, seed = 1234,
    choiceVar = "shopping", keepLev = c(1,2,3)){
    set.seed(seed)
    library(mlogit)
    n <- dim(data)[1]
    idx <- sample(1:n, n, replace=FALSE)
    half <- floor(n/2)
    idx1 <- idx[1:half]
    idx2 <- idx[(half+1):n]
    datA <- data[idx1,]
    datB <- data[idx2,]
    levs <- with(data, levels(get(choiceVar)))
    datA <- mlogit.data(datA, choice = choiceVar, shape = "wide", sep="_", alt.levels = levs)
    datB <- mlogit.data(datB, choice = choiceVar, shape = "wide", sep="_", alt.levels = levs)
    fitA <- mlogit(formula, data=datA)
    fitB <- mlogit(formula, data=datB)
    betaF <- coef(fitA) * 1/sqrt(2) + coef(fitB)*(1-1/sqrt(2))
    fitC <- mlogit(formula, data=datB, alt.subset = keepLev)
    betaR <- coef(fitC)
    LL1 <- logLik(fitC)
    uu <- names(betaR)
    betaFR <- betaF[uu]
    K <- length(betaR)
    fitD <- mlogit(formula, data=datB, alt.subset = keepLev, start = betaFR, iterlim=0)
    LL0 <- logLik(fitD)
    SH <- -2*(LL0 - LL1)
    prob <- pchisq(SH, df=K, lower.tail=FALSE)
    out <- list("chisq" = SH[1], "df" = K, "pValue" = prob[1], "LLU" = LL0, "LLR" = LL1)
    if(prob > 0.05){
        cat("\nSmall and Hsiao test\nH0: IIA assumption is accepted\n")
    } else {
        cat("\nSmall and Hsiao test\nH0: IIA assumption is rejected\n")
    }
    return(out)
}

smhsiaoTest(fair ~ 1|male + age + loghinc, data=dat2, seed = 1231, choiceVar ="fair", keepLev = c(1,2,3,5))
smhsiaoTest(fair ~ 1|male + age + loghinc, data=dat2, seed = 1232, choiceVar ="fair", keepLev = c(1,2,3,4))
smhsiaoTest(fair ~ 1|male + age + loghinc, data=dat2, seed = 1233, choiceVar ="fair", keepLev = c(1,2,4,5))
smhsiaoTest(fair ~ 1|male + age + loghinc, data=dat2, seed = 1234, choiceVar ="fair", keepLev = c(1,3,4,5))
