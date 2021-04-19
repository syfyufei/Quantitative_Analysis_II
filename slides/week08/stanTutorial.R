setwd("d:/baidu/Tsinghua/Courses/QuantII/week08/")
#setwd("~/Documents/week08")
library(arm)
library(rstan)
library(rstanarm)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



library(foreign)

# simple linear regression
kidData <- read.dta("kidiq.dta")
summary(kidData)
N <- dim(kidData)[1]

M01 <- lm(kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq, data=kidData)#, prior = R2(0.1))
summary(M01)
M01a <- stan_lm(kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq, data=kidData, prior = R2(0.1))
summary(M01a)

dataList <- with(kidData, list("N"= N, "kidScore" = kid_score, "momHs" = mom_hs, "momIq" = mom_iq))

BM01 <- stan(file = 'kidiq.stan', data = dataList, iter = 100, chains = 1)
BM01 <- stan(fit = BM01, data = dataList, iter = 1000, chains = 4, cores = 4)
print(BM01)
#pairs(BM01)

betaPost <- extract(BM01, "beta")$beta
betaMean <- colMeans(betaPost)
betaSE <- apply(betaPost, 2, sd)

BM01a <- stan(file = 'kidiq2.stan', data = dataList, iter = 100, chains = 1)
BM01a <- stan(fit = BM01a, data = dataList, iter = 2000, chains = 4, cores = 4)
print(BM01a)
#pairs(BM01a)

yhat <- extract(BM01a, pars="yhat")$yhat
dim(yhat)




# standardization
zMomHs <- rescale(kidData$mom_hs, binary.inputs = "full")
zMomIq <- rescale(kidData$mom_iq)
M02 <- lm(kid_score ~ zMomHs + zMomIq + zMomHs:zMomIq, data=kidData)
summary(M02)
dataList <- with(kidData, list("N"= N, "kidScore" = kid_score, "momHs" = mom_hs, "momIq" = mom_iq))
BM02 <- stan(file = 'zkidiq.stan', data = dataList, iter = 100, chains = 1)
BM02 <- stan(fit = BM02, data = dataList, iter = 1000, chains = 4, cores = 4)
print(BM02)

# categorical x
M03 <- lm(kid_score ~ factor(mom_work), data=kidData)
summary(M03)
dataList <- with(kidData, list("N"= N, "kidScore" = kid_score, "momWork" = mom_work))
BM03 <- stan(file = 'kidiqMomWk.stan', data = dataList, iter = 100, chains = 1)
BM03 <- stan(fit = BM03, data = dataList, iter = 1000, chains = 4, cores = 4)
print(BM03)

rm(list=ls())
wells <- read.table("wells.dat")
N <- dim(wells)[1]
M04 <- glm(switch ~ I(dist/100), data = wells, family = binomial(link = "logit"))
summary(M04)
M04a <- stan_glm(switch ~ I(dist/100), data = wells, family = binomial(link = "logit"))
summary(M04a)

dataList <- with(wells, list("N" = N, "y" = switch, "dist" = dist))
BM04 <- stan(file = 'wellslogit.stan', data = dataList, iter = 100, chains = 1)
BM04 <- stan(fit = BM04, data = dataList, iter = 1000, chains = 4, cores=4)
print(BM04)

M05 <- glm(switch ~ I(dist/100), data = wells, family = binomial(link = "probit"))
summary(M05)
dataList <- with(wells, list("N" = N, "y" = switch, "dist" = dist))
BM05 <- stan(file = 'wellsProbit.stan', data = dataList, iter = 100, chains = 1)
BM05 <- stan(fit = BM05, data = dataList, iter = 1000, chains = 4, cores=4)

rm(list=ls())
# 2sls
dat <- read.dta("sesame.dta")
summary(dat)
N <- dim(dat)[1]
table(dat$viewcat)
regular <- ifelse(dat$viewcat %in% c(2,3,4), 1, 0)
encour <- ifelse(dat$viewenc==1, 1, 0)
library(systemfit)
eq1 <- postlet ~ regular
eq2 <- regular ~ encour
system <- list(eq1, eq2)
inst <- ~ encour
TSLS <- systemfit(system, "2SLS", inst=inst, data=dat)
summary(TSLS)
yt <- with(dat,cbind(postlet, regular))
dataList <- with(dat, list("yt" = yt, "z" = encour, "N" = N))
BM08 <- stan(file = 'tsls.stan', data = dataList, iter = 100, chains = 1)
BM08 <- stan(fit = BM08, data = dataList, iter = 2000, chains = 4, cores = 4, thin = 2)
betaPost <- extract(BM08, "b")$b


# ordinal outcome
library(MASS)
M09 <- polr(ordered(Sat) ~ as.numeric(Infl) + as.numeric(Type) + as.numeric(Cont), weights = Freq, data = housing)
summary(M09)




x <- with(housing, cbind(Infl, Type, Cont))
N <- dim(housing)[1]
D <- 3
K <- length(unique(housing$Sat))
dataList <- with(housing, list("y" = as.numeric(as.factor(Sat)), "x" = x, "N" = N, "D" = D, "K" = K))
BM09 <- stan(file = 'ologit.stan', data = dataList, iter = 100, chains = 1)
BM09 <- stan(fit = BM09, data = dataList, iter = 1000, chains = 4, cores=4)
print(BM09)




# unordered categorical outcome
library(nnet)
M10 <- multinom(factor(Sat) ~ as.numeric(Infl) + as.numeric(Type) + as.numeric(Cont) + Freq -1, data = housing)
summary(M10)
x <- with(housing, cbind(Infl, Type, Cont, Freq))
N <- dim(housing)[1]
D <- ncol(x)
K <- length(unique(housing$Sat))
dataList <- with(housing, list("y" = as.numeric(as.factor(Sat)), "x" = x, "N" = N, "D" = D, "K" = K))
BM10 <- stan(file = 'mlogit.stan', data = dataList, iter = 100, chains = 1)
BM10 <- stan(fit = BM10, data = dataList, iter = 1000, chains = 4, cores=4)
print(BM10, pars="beta", digits=3)


rm(list=ls())
library(MASS)
library(pscl)

dat <- read.dta("couart2.dta")
n <- dim(dat)[1]

dat$female <- ifelse(dat$fem == "Female", 1,
            ifelse(dat$fem == "Male", 0, NA))
dat$married <- ifelse(dat$mar == "Married", 1,
            ifelse(dat$mar == "Single", 0, NA))


N <- dim(dat)[1]
X <- with(dat, cbind(1, female, married, kid5, phd, ment))
K <- dim(X)[2]

# poisson

M11 <- glm(art ~ female + married + kid5 + phd + ment, data=dat, family=poisson(link="log"))
summary(M11)

dataList <- with(dat, list("N"= N, "y" = art, "X" = X, "K" = K, "offset" = rep(0,N)))
BM11 <- stan(file = 'poisson.stan', data = dataList, iter = 100, chains = 1)
BM11 <- stan(fit = BM11, data = dataList, iter = 1500, chains = 4, cores = 4)
print(BM11)
#pairs(BM11)



M12 <- glm(art ~ female + married + kid5 + phd + ment, data=dat, family=quasipoisson)
summary(M12)

# quasi_poisson
dataList <- with(dat, list("N"= N, "y" = art, "X" = X, "K" = K))#, "offset" = rep(0,N)))
BM12 <- stan(file = 'quasipoisson.stan', data = dataList, iter = 100, chains = 1)
BM12 <- stan(fit = BM12, data = dataList, iter = 1500, chains = 4, cores = 4)
print(BM12)


# zip
M13 <- zeroinfl(art ~ female + married + kid5 + phd + ment, data=dat, dist = "poisson", link = "logit")
summary(M13)
dataList <- with(dat, list("N"= N, "y" = art, "X" = X, "K" = K))#, "offset" = rep(0,N)))
BM13 <- stan(file = 'zip.stan', data = dataList, iter = 100, chains = 1)
BM13 <- stan(fit = BM13, data = dataList, iter = 1500, chains = 4, cores = 4)
print(BM13, pars=c("beta", "beta_theta", "lp__"))

# negbin
M14 <- glm.nb(art ~ female + married + kid5 + phd + ment, data=dat)
summary(M14)

dataList <- with(dat, list("N"= N, "y" = art, "X" = X, "K" = K))
BM14 <- stan(file = 'negbin.stan', data = dataList, iter = 100, chains = 1)
BM14 <- stan(fit = BM14, data = dataList, iter = 1500, chains = 4, cores = 4)
print(BM14, pars=c("beta", "theta", "alpha", "lp__"))
