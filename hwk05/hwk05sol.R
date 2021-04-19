setwd("d:/baidu/tsinghua/courses/quantii/hwk04")
library(haven)
dat <- read_dta("cgss2010.dta")

setwd("d:/baidu/tsinghua/courses/quantii/hwk05")


summary(dat)
male <- ifelse(dat$a2==1, 1,
    ifelse(dat$a2==2, 0, NA))
a3a <- ifelse(dat$a3a < 17, NA, dat$a3a)
age <- 2010-a3a
a62 <- ifelse(dat$a62 > 9999996, NA, dat$a62)
loghinc <- log(a62+1)
ccpmember <- ifelse(dat$a10==1, 1, 0)
donate <- ifelse(is.na(dat$n35a), 0, dat$n35a)
donate <- ifelse(donate <0, NA, donate)

dat1 <- cbind.data.frame(donate, male, age, loghinc, ccpmember)
dat2 <- na.exclude(dat1)

M1 <- glm(donate ~ age + male + ccpmember + loghinc, data=dat2, family=poisson())
summary(M1)


# sandwich methods
library(sandwich)
covM1 <- vcovHC(M1, type="HC3")
seM1 <- sqrt(diag(covM1))

M1.1 <- glm(donate ~ male + ccpmember + loghinc, offset = log(age), data=dat2, family=poisson())
summary(M1.1)


M0 <- update(M1, .~1, data=dat2)

M2 <- glm(donate ~ age + male + ccpmember + loghinc, data=dat2, family=quasipoisson())
summary(M2)

# pseudo R2
nullDev <- deviance(M0)
dev <- deviance(M1)

pR2 <- (nullDev-dev)/nullDev
# 0.01009848


# likelihood ratio test
LR <- nullDev - dev
#363.2564
k <- length(coef(M1))
prob <- pchisq(LR, df=k-1, lower.tail = FALSE)


# irr, incidence rate ratio
IRR <- function(fit, newdata){
    IR <- predict(fit, newdata, type = "response") #lambdas
    IRR <- IR[2]/IR[1]
    return(IRR)
}
newX <- with(dat2, cbind.data.frame(age = c(20, 21), male = 1, ccpmember=1, loghinc=mean(loghinc)))
IRR(M1, newX)
#1.007256

newX <- with(dat2, cbind.data.frame(age = mean(age), male = c(0,1), ccpmember=1, loghinc=mean(loghinc)))
IRR(M1, newX)
#0.6635374

newX <- with(dat2, cbind.data.frame(age = mean(age), male = 1, ccpmember=c(0,1), loghinc=mean(loghinc)))
IRR(M1, newX)
#0.6451704

newX <- with(dat2, cbind.data.frame(age = mean(age), male = 1, ccpmember=1, loghinc=c(10, 11)))
IRR(M1, newX)
#1.115704

# we can use exp(coef(M1)) to get the same results aforementioned
exp(coef(M1))


# predictive difference between gender
newX <- with(dat2, cbind.data.frame(male = c(0,1), age = 30, loghinc = mean(loghinc),ccpmember=1))
yhat <- predict(M1, newdata=newX, type = "response")
yhat[2] - yhat[1]





# comparison between saturated and null model
newX <- with(dat2, cbind.data.frame(male = 1, age = 30,  loghinc = mean(loghinc), ccpmember=1))

numDonate <- seq(0, max(dat2$donate))
K <- length(numDonate)
phatN <- phatS <- rep(NA, K)
for(i in 1:K){
    lamS <- predict(M1, newdata=newX, type="response")
    phatS[i] <- dpois(numDonate[i], lambda=lamS)
    lamN <- predict(M0, newdata=newX, type="response")
    phatN[i] <- dpois(numDonate[i], lambda=lamN)
}


tabDonate <- table(dat2$donate)
pObserved <- tabDonate / sum(tabDonate)
xObserved <- names(pObserved)


plot(0, 0, xlim =c(0, 50), ylim=c(0, 1),
    type="n", axes=FALSE, frame.plot=TRUE,
    ylab="Probability of count", xlab="Number of donates",
    xaxs="i", yaxs="i")
lines(x = xObserved, y = pObserved)
lines(x = 1:K, y = phatS, col=2)
lines(x = 1:K, y = phatN, col=3)
legend("topright", col=1:3, lty=1, legend=c("Observed", "Saturated model", "Null model"))
axis(2)
axis(1)



# graphying predictive probability between genders along ment
rangeloghinc <- with(dat2, range(loghinc))
incFake <- seq(rangeloghinc[1],rangeloghinc[2],length=1000)
newXN <- with(dat, cbind.data.frame(male = 1, age = 30, ccpmember=0, loghinc=incFake))
newXC <- with(dat, cbind.data.frame(male = 1, age = 30, ccpmember=1, loghinc=incFake))
lambdaN <- predict(M1, newdata=newXN, type = "response")
lambdaC <- predict(M1, newdata=newXC, type = "response")

N <- 5 # donate 5 times
phatN <- dpois(N, lambdaN) # compute predictive probability given N = 5, and lambda predicted from the model
phatC <- dpois(N, lambdaC)

par(mar=c(3,3,1,5), mgp=c(2,0.5,0), tcl=-0.2)
plot(x=incFake, y=phatN, type="l", xlim=rangeloghinc, ylim=c(0,0.001),
    xlab = "log(income)", ylab = "Pr(donate = 5)", axes=FALSE, frame.plot=TRUE)
lines(x = incFake, y=phatC, col=2)
text(x = incFake[1000], y = phatN[1000], "Non CCP Member", adj=1, xpd=NA)
text(x = incFake[1000], y = phatC[1000], "CCP Member", adj=1, col=2, xpd=NA)
axis(2, c(0, 0.00025, 0.0005, 0.00075, 0.001), c("0", "0.00025", "0.0005", "0.00075", "0.001"))
axis(1)

# negative binomial
library(MASS)
M3 <- glm.nb(donate ~ age + male + ccpmember + loghinc, data=dat2)
summary(M3)


# alpha test
G2 <- 2*(logLik(M3) - logLik(M1))
#'log Lik.' 28895.09 (df=6)
pchisq(G2, df=1, lower.tail=FALSE)
# H0: Negbin is the same as Poisson (alpha=0)
# reject H0


# zip

# zero inflated poisson
library(pscl)
M4 <- zeroinfl(donate ~ age + male + ccpmember + loghinc, data=dat2, dist = "poisson", link = "logit")
summary(M4)

# zero inflated negative binomial
M5 <- zeroinfl(donate ~ age + male + ccpmember + loghinc, data=dat2, dist = "negbin", link = "logit")
summary(M5)

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


N <- dim(dat2)[1]
X <- with(dat2, cbind(1, age, male, loghinc, ccpmember))
K <- dim(X)[2]


dataList <- with(dat2, list("N"= N, "y" = donate, "X" = X, "K" = K))
M6 <- stan(file = 'zinb.stan', data = dataList, iter = 100, chains = 1)
M6 <- stan(fit = M6, data = dataList, iter = 1500, chains = 4, cores = 4)
print(M6, pars=c("beta", "beta_theta"))

logLik(M5)
#'log Lik.' -4431.42 (df=11)


logLikM6 <- extract(M6, pars="log_lik")$log_lik
logLikM6 <- sum(apply(logLikM6, 2, mean))
# -2202.848


# model comparison

AIC(M1)
AIC(M2)
AIC(M3)
AIC(M4)
AIC(M5)


BIC(M1)
BIC(M2)
BIC(M3)
BIC(M4)
BIC(M5)


vuong(M3, M5)
#Vuong Non-Nested Hypothesis Test-Statistic:
#(test-statistic is asymptotically distributed N(0,1) under the
# null that the models are indistinguishible)
#-------------------------------------------------------------
#              Vuong z-statistic             H_A p-value
#Raw                1.197403e-05 model1 > model2     0.5
#AIC-corrected      1.831503e+04 model1 > model2  <2e-16
#BIC-corrected      8.460118e+04 model1 > model2  <2e-16
