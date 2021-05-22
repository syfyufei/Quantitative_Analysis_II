setwd("d:/baidu/Tsinghua/Courses/QuantII/week14")
set.seed(431243)
library(arm)

group <- rep(1:10, rep(10,10))
u.all <- rnorm(10,2,1)[group]
g0 <- -1
g1 <- 0.5
sigma.a <- 1
mu.a <- mean(g0 + g1*u.all + sigma.a)
mu.b <- 3
sigma.b <- 4
rho <- 0.56
Sigma.ab <- array (c(sigma.a^2, rho*sigma.a*sigma.b,
                  rho*sigma.a*sigma.b, sigma.b^2), c(2,2))
sigma.y <- 1
ab <- mvrnorm (10, c(mu.a,mu.b), Sigma.ab)
a <- ab[,1]
b <- ab[,2]
x <- rnorm (100)
y <- rnorm (100, a[group] + b*x, sigma.y)
y2 <- rbinom(100, size=1, prob=invlogit(a[group] + b*x))

library(foreign)
dat <- cbind.data.frame(y, y2, x, u=u.all, group)
write.dta(dat, file="dat.dta")


M0 <- lmer(y ~ 1 + (1|group))
display(M0)
library(performace)
icc(M0)

M1 <- lmer(y ~ x + (1|group))
display(M1)

M2 <- lmer(y ~ x + (1+x|group))
display(M2)

M3 <- lmer(y ~ x + u.all + (1+x|group))
display(M3)

coef(M1)
fixef(M1)
ranef(M1)
se.fixef(M1)
se.ranef(M1)
se.coef(M1)

fixef(M1)["x"] + 1.96*c(-1,1)*se.fixef(M1)["x"]

coef(M1)$group[5,"(Intercept)"] + 2*c(-1,1)*se.ranef(M1)$group[5]
ranef(M1)$group[5,"(Intercept)"] + 2*c(-1,1)*se.ranef(M1)$group[5]

pooled <- lm(y ~ x)
nopooled <- lm(y ~ x + factor(group))



a.hat.M1 <- coef(M1)$group[,"(Intercept)"]
b.hat.M1 <- coef(M1)$group[,"x"]
par(mfrow=c(3,4), mar=c(3,3,3,0.5),mgp=c(2,0.2,0),tcl=-0.2)
for(j in 1:10){
    plot(x[group==j], y[group==j], xlim=c(-3,3), ylim=c(-12,12),
        xlab="x", ylab="y", main=paste("group = ", j, sep=""))
    curve(coef(pooled)["(Intercept)"] + coef(pooled)["x"]*x,
        col="red", add=TRUE)
    curve(coef(nopooled)[j+1] + coef(nopooled)["x"]*x,
        col="forestgreen", add=TRUE)
    curve(a.hat.M1[j] + b.hat.M1[j]*x, lwd=1, col="black", add=TRUE)
}



a.hat.M2 <- coef(M2)$group[,"(Intercept)"]
b.hat.M2 <- coef(M2)$group[,"x"]
par(mfrow=c(3,4), mar=c(3,3,3,0.5),mgp=c(2,0.2,0),tcl=-0.2)
for(j in 1:10){
    plot(x[group==j], y[group==j], xlim=c(-3,3), ylim=c(-12,12),
        xlab="x", ylab="y", main=paste("group = ", j, sep=""))
    curve(a.hat.M2[j] + b.hat.M2[j]*x, lwd=1, col="black", add=TRUE)
}


x.new <- 3
sigma.y.hat <- sigma.hat(M1)$sigma$data
coef.hat <- as.matrix(coef(M2)$group[5,])
y.tilde <- rnorm(1, coef.hat %*% c(1,x.new), sigma.y.hat)

n.sims <- 1000
sigma.y.hat <- sigma.hat(M1)$sigma$data
coef.hat <- as.matrix(coef(M2)$group[5,])
y.tilde <- rnorm(n.sims, coef.hat %*% c(1,x.new), sigma.y.hat)
quantile(y.tilde, probs=c(0.025,0.25,0.5,0.75,0.975))




M4 <- glmer(y2 ~ x + (1 + x | group), family=binomial("logit"))
display(M4)



radon <- read.dta("radon.dta")
N <- dim(radon)[1]
J <- length(unique(radon$county))
dataList <- with(radon, list("y" = y, "county" = county, "N" = N, "J" = J, "x" = x))
M06 <- lmer(y ~ x + (1+x|county), data=radon)
summary(M06)

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')



BM06 <- stan(file = 'vivs.stan', data = dataList, iter = 100, chains = 1)
BM06 <- stan(fit = BM06, data = dataList, iter = 1000, chains = 3, cores=3)
print(BM06)

BM06a <- stan(file = 'vivs2.stan', data = dataList, iter = 100, chains = 1, cores=1)
BM06a <- stan(fit = BM06a, data = dataList, iter = 1000, chains = 3, cores=3)
print(BM06a)

BM06b <- stan(file = 'vivs3.stan', data = dataList, iter = 100, chains = 1, cores=1)
BM06b <- stan(fit = BM06b, data = dataList, iter = 1000, chains = 3, cores=3)
print(BM06b)

u <- with(radon, tapply(uAll, county, mean))
u.all <- with(radon, u[county])

M07 <-  lmer(y ~ x + u.all + (1+x|county), data=radon)
summary(M07)

dataList <- with(radon, list("y" = y, "county" = county, "N" = N, "J" = J, "x" = x, "u" = u))
inits <-  function(){
list(gA0 = rnorm(1), gA1 = rnorm(1), gB0 = rnorm(1), gB1 = rnorm(1),
    sigma = runif(1), sigmaA = runif(1), sigmaB = runif(1))
}
BM07 <- stan(file = 'vivsg.stan', init = inits, data = dataList, iter = 100, chains = 1)
BM07 <- stan(fit = BM07, data = dataList, iter = 1000, chains = 3, cores=3)
print(BM07, pars=c("a", "b", "gA0", "gA1", "gB0", "gB1"))

BM08 <- stan(file = 'vivsg2.stan', init = "random", data = dataList, iter = 100, chains = 1)
BM08 <- stan(fit = BM08, data = dataList, iter = 1000, chains = 3, cores=3)
print(BM08, pars=c("a", "b", "gA0", "gA1", "gB0", "gB1"))
