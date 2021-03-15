setwd("G:/QuantII/quiz01")
library(foreign)

dat <- read.dta("Acemoglu.dta")

library(systemfit)
eq1 <- logpgp95 ~ avexpr + f_french + sjlofr + catho80 + muslim80 + no_cpm80
eq2 <- avexpr ~ logem4 + f_french + sjlofr + catho80 + muslim80 + no_cpm80
system <- list(eq1, eq2)
inst <- ~ logem4 + f_french + sjlofr + catho80 + muslim80 + no_cpm80
TSLS <- systemfit(system, method="2SLS", inst=inst, data=dat)
summary(TSLS)

OLS <- lm(eq1, data=dat)
OLS2 <- lm(avexpr ~ logem4 + f_french + sjlofr + catho80 + muslim80 + no_cpm80, data=dat)
Xmat <- with(dat, cbind(1, logem4, f_french, sjlofr, catho80, muslim80, no_cpm80))
n <- dim(dat)[1]
K <- length(coef(OLS))
betas <- coef(OLS2)
avexprhat <-  Xmat %*% betas + rnorm(n, 0, sigma(OLS2))

#avexprhat <- predict(OLS2)

# Heckman test for endogeneity
TM1 <- lm(logpgp95 ~ avexpr + avexprhat + f_french + sjlofr + catho80 + muslim80 + no_cpm80, data=dat)
summary(TM1)



# Test for Exogeneity of Z
res <- resid(TSLS)$eq1
TM2 <- lm(res~logem4 + f_french + sjlofr + catho80 + muslim80 + no_cpm80, data=dat)
summary(TM2)
R2 <- summary(TM2)$r.squared
cQuant <- TM2$df.residual*R2
pchisq(cQuant, K-1, lower.tail=FALSE)


# Hausmen test for the Endogeneity of avexpr

B1 <- coef(TSLS)[1:K]
B2 <- coef(OLS)[1:K]
VB1 <- vcov(TSLS)[1:K,1:K]
VB2 <- vcov(OLS)[1:K,1:K]
hQuant <- t(B1-B2)%*%solve(VB1-VB2)%*%(B1-B2)
pchisq(hQuant, K, lower.tail=FALSE)
