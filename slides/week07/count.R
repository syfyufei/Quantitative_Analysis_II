
setwd("d:/baidu/Tsinghua/courses/QuantII/week07/")
library(foreign)
dat <- read.dta("couart2.dta")
n <- dim(dat)[1]

dat$female <- ifelse(dat$fem == "Female", 1,
            ifelse(dat$fem == "Male", 0, NA))
dat$married <- ifelse(dat$mar == "Married", 1,
            ifelse(dat$mar == "Single", 0, NA))


# art: articles in last 3 yrs of Phd
# fem: female
# mar: married
# kid5: number of children age < 6
# phd: PhD prestige
# ment: Mentors artiles in last 3 yrs
# profage: professional age as the exposure


M1 <- glm(art ~ female + married + kid5 + phd + ment, data=dat, family=poisson(link="log"))
summary(M1)

# exposure, input as the log form in offset
M1.1 <- glm(art ~ female + married + kid5 + phd + ment, offset = log(profage), data=dat, family=poisson(link="log"))
summary(M1.1)

bootSE <- function(fit, data){
    n <- dim(data)[1]
    idx <- sample(1:n, n, replace=TRUE)
    newData <- data[idx,]
    fit <- update(fit, data = newData)
    betas <- coef(fit)
    return(betas)
}

foo <- function() replicate(1000, bootSE(fit = M1, data = dat))

# bootstrap
library(parallel)
library(abind)
cl <- makeCluster(parallel::detectCores())
#cl <- makeCluster(spec = 4)
clusterExport(cl = cl, c("M1", "dat", "bootSE")) # export object to each thread
tryCatch(res <- clusterCall(cl=cl, fun = foo), finally = stopCluster(cl))
res2 <- abind(res, along=2)
simSes <- apply(res2, 1, sd)

# sandwich methods
library(sandwich)
covM1 <- vcovHC(M1, type="HC3")
seM1 <- sqrt(diag(covM1))

# sandwich manually
X <- model.matrix(M1)
hatX <- lm.influence(M1)$hat
Omega <- diag(M1$residuals^2/(1-hatX)^2)
cov2 <- solve(t(X)%*%X) %*% t(X) %*% Omega %*% X %*% solve(t(X)%*%X)
sesM1 <- sqrt(diag(cov2))

# predicted lambda
lambda <- predict(M1, type= "response") # predictive counts

xbetas <- predict(M1, type = "link") #  == fitted(M1)
all(lambda== exp(xbetas))

# pseudo R2
M0 <- update(M1, .~1)
devNull <- deviance(M0)
dev <- deviance(M1)
pR2 <- (devNull - dev) / devNull


# likelihood ratio test
LR <- devNull - dev
k <- length(coef(M1))
prob <- pchisq(LR, df=k-1, lower.tail = FALSE)



# irr, incidence rate ratio
IRR <- function(fit, newdata){
    IR <- predict(fit, newdata, type = "response") #lambdas
    IRR <- IR[2]/IR[1]
    return(IRR)
}

newX <- with(dat, cbind.data.frame(female = c(0,1), married = 0,  kid5 = 0, phd = 0, ment = 0))
IRR(M1, newX)
# 0.7777501
# female publishes 20% (1-0.7988403) less paper than male


# comparison between saturated and null model
newX <- with(dat, cbind.data.frame(female = 1, married = 1,  kid5 = mean(kid5), phd = mean(phd), ment = mean(ment)))

numPaper <- seq(0, max(dat$art))
K <- length(numPaper)
phatN <- phatS <- rep(NA, K)
for(i in 1:K){
    lamS <- predict(M1, newdata=newX, type="response")
    phatS[i] <- dpois(numPaper[i], lambda=lamS)
    lamN <- predict(M0, newdata=newX, type="response")
    phatN[i] <- dpois(numPaper[i], lambda=lamN)
}

tabPaper <- table(dat$art)
pObserved <- tabPaper / sum(tabPaper)
xObserved <- names(pObserved)


plot(0, 0, xlim =c(0, K), ylim=c(0, 0.4), type="n", axes=FALSE, frame.plot=TRUE,
    ylab="Probability of count", xlab="Number of papers")
lines(x = xObserved, y = pObserved)
#segments(x0=as.numeric(xObserved), y0=0, x1=as.numeric(xObserved), y1=pObserved)
lines(x = 1:K, y = phatS, col=2)
lines(x = 1:K, y = phatN, col=3)
legend("topright", col=1:3, lty=1, legend=c("Observed", "Saturated model", "Null model"))
axis(2)
axis(1)




# graphying predictive probability between genders along ment
rangeMent <- with(dat, range(ment))
mentFake <- seq(rangeMent[1],rangeMent[2],length=1000)
newXF <- with(dat, cbind.data.frame(female = 1, married = 1, kid5=0, phd=mean(phd), ment = mentFake))
newXM <- with(dat, cbind.data.frame(female = 0, married = 1, kid5=0, phd=mean(phd), ment = mentFake))
lambdaF <- predict(M1, newdata=newXF, type = "response")
lambdaM <- predict(M1, newdata=newXM, type = "response")

N <- 2 # publish 2 papers
phatF <- dpois(N, lambdaF) # compute predictive probability given N = 2, and lambda predicted from the model
phatM <- dpois(N, lambdaM)

par(mar=c(3,3,1,1), mgp=c(2,0.5,0), tcl=-0.2)
plot(x=mentFake, y=phatF, type="l", xlim=rangeMent, ylim=c(0,0.5),
    xlab = "ment", ylab = "Pr(art = 2)")
lines(x = mentFake, y=phatM, col=2)
text(x = mentFake[500], y = phatF[500], "Female", adj=0)
text(x = mentFake[500], y = phatM[500], "Male", adj=1, col=2)
#legend("topright", lty=1, col=c(1,2), legend = c("Female", "Male"))
legend(locator(1), lty=1, col=c(1,2), legend = c("Female", "Male"))



M2 <- glm(art ~ female + married + kid5 + phd + ment, data=dat, family=quasipoisson)
summary(M2)


# negative binomial
library(MASS)
M3 <- glm.nb(art ~ female + married + kid5 + phd + ment, data=dat)
summary(M3)


# alpha test
G2 <- 2*(logLik(M3) - logLik(M1))
pchisq(G2, df=1, lower.tail=FALSE)
# H0: Negbin is the same as Poisson (alpha=0)


# hurdle model
library(pscl)
M4 <- hurdle(art ~ female + married + kid5 + phd + ment, data=dat, dist = "poisson", zero.dist = "binomial", link = "logit")
summary(M4)

dat$artBin <- ifelse(dat$art > 0, 1, dat$art)
dat$artNew <- ifelse(dat$art==0, NA, dat$art)
M4.1 <- glm(artBin ~ female + married + kid5 + phd + ment, data=dat, family=binomial)
summary(M4.1)
library(VGAM)
# zero truncated poisson
M4.2 <- vglm(artNew ~ female + married + kid5 + phd + ment, data=dat, family=pospoisson())
summary(M4.2)
# zero truncated negative binomial
M4.3 <- vglm(artNew ~ female + married + kid5 + phd + ment, data=dat, family=posnegbinomial())
summary(M4.3)

# zero inflated poisson
M5 <- zeroinfl(art ~ female + married + kid5 + phd + ment, data=dat, dist = "poisson", link = "logit")
summary(M5)

foo2 <- function() {
    require(pscl)
    replicate(250, bootSE(fit = M5, data = dat))
}
cl <- makeCluster(parallel::detectCores())
#cl <- makeCluster(spec = 4)
clusterExport(cl = cl, c("M5", "dat", "bootSE")) # export object to each thread
tryCatch(res <- clusterCall(cl=cl, fun = foo2), finally = stopCluster(cl))
res2 <- abind(res, along=2)
simSes <- apply(res2, 1, sd)



# zero inflated negative binomial
M6 <- zeroinfl(art ~ female + married + kid5 + phd + ment, data=dat, dist = "negbin", link = "logit")
summary(M6)
M6.2 <- glm(art ~ female + married + kid5 + phd + ment, data=dat, family=poisson())
summary(M6.2)

# zerp altered poisson
M7.1 <- vglm(artNew ~ female + married + kid5 + phd + ment, data=dat, family=zapoisson)
summary(M7.1)
# zero altered negative binomial
M7.2 <- vglm(artNew ~ female + married + kid5 + phd + ment, data=dat, family=zanegbinomial)
summary(M7.2)




# AIC BIC
AIC(M1)
AIC(M2)
AIC(M3)
AIC(M4)
AIC(M5)
AIC(M6)

BIC(M1)
BIC(M2)
BIC(M3)
BIC(M4)
BIC(M5)
BIC(M6)


# vuong test
vuong(M3, M6)
# significant means M6 is better than M3

# fit count data with Bayesian methods
#library(rstan)
#options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)
#Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')
#N <- dim(dat)[1]
#X <- with(dat, cbind(1, female, married, kid5, phd, ment))
#K <- dim(X)[2]
#
## poisson
#dataList <- with(dat, list("N"= N, "y" = art, "X" = X, "K" = K, "offset" = rep(0,N)))
#BM01 <- stan(file = 'poisson.stan', data = dataList, iter = 100, chains = 1)
#BM01 <- stan(fit = BM01, data = dataList, iter = 3000, chains = 3, cores = 3)
#print(BM01)
#pairs(BM01)
#stan_Rhats(BM01)
#
## quasi_poisson
#dataList <- with(dat, list("N"= N, "y" = art, "X" = X, "K" = K))#, "offset" = rep(0,N)))
#BM02 <- stan(file = 'quasipoisson.stan', data = dataList, iter = 100, chains = 1)
#BM02 <- stan(fit = BM02, data = dataList, iter = 1000, chains = 3, cores = 3)
#print(BM02)
#pairs(BM02)
#stan_Rhats(BM02)
