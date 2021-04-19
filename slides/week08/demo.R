


pop <- rescale(c(5.4, 3.2, 6.5, 9.0))
land <- rescale(c(2.3, 1.2, 0.5, 0.3))
indus <- rescale(c(0.4, 0.3, 0.2, 0.15))
resource <- c(0,1,1,0)
eduyrs <- rescale(c(8.3, 9.4, 9.8, 13))
gender <- c(0.3, 0.5, 0.4, 0.48)
gdp <- rescale(c(5430, 6780, 5980, 10200))
dat <- cbind.data.frame(gdp, pop, land, indus, resource, eduyrs, gender)

M0 <- lm(gdp ~ pop + land + indus + resource + eduyrs + gender, data=dat)
summary(M0)


library(rstanarm)
options(mc.cores = parallel::detectCores())
M1 <- stan_glm(gdp ~ pop + land + indus + resource + eduyrs + gender,
    prior_intercept = normal(0,10), prior = normal(0,1), data = dat)
summary(M1)

M2 <- stan_glm(gdp ~ pop + land + indus + resource + eduyrs + gender,
    prior_intercept = normal(0,1000), prior = normal(0,1000), data = dat)
summary(M2)



curve(dnorm(x, 0, 1), from=-6, to=6, ylab="Density")
curve(dnorm(x, 0, 1000), add=TRUE, col=2)
text(x=1+0.1, y=dnorm(1, 0, 1), "Normal(0,1)", adj=0)
text(x=3.5, y=dnorm(3.5,0,1000)+0.008, "Normal(0,1000)", adj=0, col=2, xpd=TRUE)


n <- 100
x1 <- rnorm (n)
x2 <- rbinom (n, 1, .5)
b0 <- 1
b1 <- 1.5
b2 <- 2
y <- rbinom (n, 1, invlogit(b0+b1*x1+b2*x2))
y <- ifelse (x2==1, 1, y)
dat <- cbind.data.frame(y, x1, x2)
M1 <- glm (y ~ x1 + x2, family=binomial(link="logit"), data = dat)
display (M1)
M4 <- stan_glm(y ~ x1 + x2, family=binomial(link="logit"),
    prior = student_t(7,0,2.5), prior_intercept = student_t(7,0,10), data = dat)
M5 <- stan_glm(y ~ x1 + x2, family=binomial(link="logit"),
    prior = student_t(1,0,2.5), prior_intercept = student_t(1,0,10), data = dat)


setwd("d:/baidu/Tsinghua/talk/BayesTalk")
n <- 100
a <- 1
b <- 2
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- a + b*x1 + rnorm(100)
dat <- cbind.data.frame(y, x1, x2)
M6 <- lm(y ~ x1 + x2, data = dat)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
dataList <- with(dat, list("N"= n, "y" = y, "x1" = x1, "x2" = x2))
BM01 <- stan(file = 'ols.stan', data = dataList, iter = 100, chains = 1)
BM01 <- stan(fit = BM01, data = dataList, iter = 3000, chains = 3, cores = 3)
print(BM01)
