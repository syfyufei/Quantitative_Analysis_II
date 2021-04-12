setwd("D:\\Baidu\\Tsinghua\\Courses\\QuantII\\week06\\")
library(foreign)
dat <- read.dta("E:/SynologyDrive/Github/Quantitative_Analysis_II/slides/week06/cgss2010short.dta")

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


c7b <- ifelse(as.numeric(dat$c7b) %in% c(1, 6, 7, 8), NA, as.numeric(dat$c7b))
shopping <- c7b-1

#shopping <- factor(shopping, levels = 1:4, labels = levels(dat$c7b)[2:5])

dat1 <- cbind.data.frame(shopping, age, male, loghinc, ccpmember)
dat2 <- na.exclude(dat1)

library(nnet)
M0 <- multinom(factor(shopping) ~ 1, data=dat2)
summary(M0)
M1 <- multinom(factor(shopping) ~ age + male + loghinc + ccpmember, data=dat2, Hess=TRUE)
summary(M1)

# McFaden R2
nullDev <- deviance(M0)
dev <- deviance(M1)

pR2 <- (nullDev- dev) / nullDev
pR2 <- 1 - logLik(M1)/logLik(update(M1, .~1))


# liklihood ratio test
LR <- nullDev - dev
k <- length(coef(M1))
prob <- pchisq(LR, df=k-3, lower.tail = FALSE) # 3 constants


# relative risk ratio
exp(coef(M1))





# Hausman McFaden IIA test

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
    browser()
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

hmTest(formula = factor(shopping) ~ age + male + loghinc + ccpmember, data=dat2, choiceVar = "shopping", keepLev = c(1,2,3), baseLev = 1)
hmTest(formula = factor(shopping) ~ age + male + loghinc + ccpmember, data=dat2, choiceVar = "shopping", keepLev = c(1,2,4), baseLev = 1)
hmTest(formula = factor(shopping) ~ age + male + loghinc + ccpmember, data=dat2, choiceVar = "shopping", keepLev = c(1,3,4), baseLev = 1)



library(mlogit)
#library(mnlogit)
dat3 <- mlogit.data(dat2, choice = "shopping", shape = "wide", sep="_", alt.levels = 1:4)


M3 <- mlogit(shopping ~ 1 | age + male + loghinc + ccpmember, data=dat3)
M4 <- mlogit(shopping ~ 1 | age + male + loghinc + ccpmember, data=dat3, alt.subset = c(1,2,3))

mlogit:::hmftest(M3, M4)




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
    levs <- levels(as.factor(data[,choiceVar]))
    datA <- mlogit.data(datA, choice = choiceVar, shape = "wide", sep="_")#, alt.levels = levs)
    datB <- mlogit.data(datB, choice = choiceVar, shape = "wide", sep="_")#, alt.levels = levs)
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

smhsiaoTest(shopping ~ 1 | age + male + loghinc + ccpmember, data=dat2, seed = 11, keepLev = c(1,2,3))
smhsiaoTest(shopping ~ 1 | age + male + loghinc + ccpmember, data=dat2, seed = 22, keepLev = c(1,2,3))
smhsiaoTest(shopping ~ 1 | age + male + loghinc + ccpmember, data=dat2, seed = 33, keepLev = c(1,2,3))

smhsiaoTest(shopping ~ 1 | age + male + loghinc + ccpmember, data=dat2, seed = 111, keepLev = c(1,3,4))
smhsiaoTest(shopping ~ 1 | age + male + loghinc + ccpmember, data=dat2, seed = 222, keepLev = c(1,3,4))
smhsiaoTest(shopping ~ 1 | age + male + loghinc + ccpmember, data=dat2, seed = 333, keepLev = c(1,3,4))

smhsiaoTest(shopping ~ 1 | age + male + loghinc + ccpmember, data=dat2, seed = 1111, keepLev = c(1,2,4))
smhsiaoTest(shopping ~ 1 | age + male + loghinc + ccpmember, data=dat2, seed = 2222, keepLev = c(1,2,4))
smhsiaoTest(shopping ~ 1 | age + male + loghinc + ccpmember, data=dat2, seed = 3333, keepLev = c(1,2,4))





newDat <- cbind.data.frame(male = rep(1, 1000), age=rep(50, 1000), ccpmember = rep(0, 1000), loghinc = seq(0,16, length=1000))
phat <- predict(M1, newdata = newDat, type="probs")

par(mar=c(3,3,1,5), mgp=c(1.5,0.2,0), tcl=-0.2)
plot(0,0, ylim=c(0,1), xlim=range(loghinc, na.rm=TRUE), type="n",
    ylab="predictive probabilities", xlab="log(househould income)", xaxs="i", yaxs="i")
for(i in 1:4){
    lines(x=newDat$loghinc, y=phat[,i], col=i)
}
for(i in 1:4){
    text(x=16, y=phat[1000, i], labels=paste("Pr(shopping=", i, ")", sep=""), xpd=NA, adj=0, col=i)
}
legend("topleft", col=1:4, lty=1, legend=paste("Pr(shopping=", 1:4, ")", sep=""))
#legend(locator(1), col=1:4, lty=1, legend=paste("Pr(shopping=", 1:4, ")", sep=""))


dat11 <- cbind.data.frame(happy, age, male, loghinc, ccpmember)
dat22 <- na.exclude(dat11)

library(nnet)
M01 <- multinom(factor(happy) ~ 1, data=dat22)
M11 <- multinom(factor(happy) ~ age + male + loghinc + ccpmember, data=dat22, Hess=TRUE)

hmTest(factor(happy) ~ age + male + loghinc + ccpmember, data=dat22, choiceVar = "happy", keepLev=c(1,2,3,4), baseLev=1)
hmTest(factor(happy) ~ age + male + loghinc + ccpmember, data=dat22, choiceVar = "happy", keepLev=c(1,3,4,5), baseLev=1)
hmTest(factor(happy) ~ age + male + loghinc + ccpmember, data=dat22, choiceVar = "happy", keepLev=c(1,2,4,5), baseLev=1)
hmTest(factor(happy) ~ age + male + loghinc + ccpmember, data=dat22, choiceVar = "happy", keepLev=c(1,2,3,5), baseLev=1)

smhsiaoTest(happy ~ 1 | age + male + loghinc + ccpmember, data=dat11, choiceVar = "happy", seed = 133, keepLev = c(1,3,4,5))
smhsiaoTest(happy ~ 1 | age + male + loghinc + ccpmember, data=dat11, choiceVar = "happy", seed = 12222, keepLev = c(1,2,4,5))
smhsiaoTest(happy ~ 1 | age + male + loghinc + ccpmember, data=dat11, choiceVar = "happy", seed = 133, keepLev = c(1,2,3,5))
smhsiaoTest(happy ~ 1 | age + male + loghinc + ccpmember, data=dat11, choiceVar = "happy", seed = 111, keepLev = c(1,2,3,4))

#M12 <- mlogit(factor(shopping) ~ 1|age + male + loghinc + ccpmember, data=dat3, probit=TRUE)


library(MNP)
M13 <- mnp(factor(shopping) ~ age + male + loghinc + ccpmember, data=dat2, verbose=TRUE)
