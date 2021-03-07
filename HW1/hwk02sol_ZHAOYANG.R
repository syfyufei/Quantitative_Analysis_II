

setwd("d:/Baidu/Tsinghua/Courses/QuantII/hwk1.5/")
library(foreign)
dat <- read.dta("cgss2010.dta")

a3a <- ifelse(dat$a3a < 0, NA, dat$a3a)
age <- 2010 - a3a # continuous
a33 <- ifelse(as.numeric(dat$a33) < 4, NA, as.numeric(dat$a33))
a33 <- a33 - 3
trust <- a33 # ordinal
a5 <- ifelse(as.numeric(dat$a5) < 2, NA, as.numeric(dat$a5))
relig <- ifelse(a5 == 2, 0, 1) # binary
a2 <- ifelse(as.numeric(dat$a2)==4, 1, 0)
male <- a2  # binary

M1 <- glm(relig ~ age + male + factor(trust), family=binomial(link="logit"))
summary(M1)


invlogit <- function(x){
    ans <- 1/(1 + exp(-x))
    return(ans)
}

attributes(M1)

getMore <- function(obj){
    dev <- obj$deviance
    devNull <- obj$null.deviance
    LR <- devNull - dev
    k <- length(coef(obj))
    prob <- pchisq(LR, df=k-1, lower.tail = FALSE)
    psuedoR2 <- (devNull - dev)/devNull
    out <- list("LR"=LR, "df"=k-1, "prob"=prob, "R2"=psuedoR2)
    return(out)
}

getMore(M1)



bM1 <- coef(M1)
maxAge <- max(age, na.rm=TRUE)
minAge <- min(age, na.rm=TRUE)

par(mar=c(3,3,3,2), mgp=c(1.5,0.2,0), tcl=-0.2)
plot(x=age, y=relig, type="n", xlab = "male", ylab="Pr(relig=1)", axes=FALSE, frame.plot=TRUE)
points(x=jitter(age), y=jitter(relig, factor=0.2), pch=".")
axis(2, at=c(0,0.5,1))
axis(1)
curve(invlogit(bM1[1] + bM1[2]*x + bM1[3]*1 + bM1[4]*1), from=minAge, to=maxAge, add=TRUE)
curve(invlogit(bM1[1] + bM1[2]*x + bM1[3]*0 + bM1[4]*1), from=minAge, to=maxAge, lty=2, add=TRUE)
text(x=maxAge, y=invlogit(bM1[1] + bM1[2]*maxAge + bM1[3]*c(0,1) + bM1[4]*1),
    labels=c("female", "male"), adj=0, xpd=NA)



M2 <- glm(relig ~ age + male + age:male + factor(trust), family=binomial(link="logit"))
summary(M2)


bM2 <- coef(M2)

getMore(M2)

par(mar=c(3,3,3,2), mgp=c(1.5,0.2,0), tcl=-0.2)
plot(x=age, y=relig, type="n", xlab = "male", ylab="Pr(relig=1)", axes=FALSE, frame.plot=TRUE)
points(x=jitter(age), y=jitter(relig, factor=0.2), pch=".")
axis(2, at=c(0,0.5,1))
axis(1)
curve(invlogit(bM2[1] + bM2[2]*x + bM2[3]*1 + bM2[4]*1 + bM2[8]*1*x), from=minAge, to=maxAge, add=TRUE)
curve(invlogit(bM2[1] + bM2[2]*x + bM2[3]*0 + bM2[4]*1), from=minAge, to=maxAge, lty=2, add=TRUE)
text(x=maxAge, y=invlogit(bM2[1] + bM2[2]*maxAge + bM2[3]*c(0,1) + bM2[4]*1 +  + bM2[8]*c(0,1)*maxAge),
    labels=c("female", "male"), adj=0, xpd=NA)
