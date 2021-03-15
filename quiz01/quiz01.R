######################
#### Wenquan Wu
#### 2020312259
#### quiz01
######################

library(foreign)
library(AER)
library(stargazer)
library(dplyr)
data <- read.dta('./Acemoglu.dta') %>% na.omit()


# 2SLS
TSLS <- ivreg(logpgp95 ~ avexpr + f_french + sjlofr + catho80 + muslim80 + no_cpm80 |
              .-avexpr + logem4, data = data)
stargazer(M1, type = 'text')

# OLS
OLS <- ivreg(logpgp95 ~ avexpr + f_french + sjlofr + catho80 + muslim80 + no_cpm80,
              data = data)

# Heckman test for endogeneity
M1 <- lm(avexpr ~ f_french + sjlofr + catho80 + muslim80 + no_cpm80 + logem4,
          data = data)
avexpr_hat <- predict(M1)

TM1 <- lm(logpgp95 ~ avexpr + f_french + sjlofr + catho80 + muslim80 + no_cpm80 + avexpr_hat,
          data = data)
summary(TM1)
#  F > 10 且avexpr和avexpr_hat都显著，拒绝弱工具变量假设

# Test for Exogeneity of Z
n <- TSLS$n
res <- resid(TSLS)
TM2 <- lm(res ~ f_french + sjlofr + catho80 + muslim80 + no_cpm80 + logem4, data = data)
summary(TM2)
R2 <- summary(TM2)$r.squared
cQuant <- n * R2
pchisq(cQuant, 5, lower.tail=FALSE) 
#  P = 1，因此无法拒绝原假设，。 

# Hausmen test for the Endogeneity of X3
B1 <- coef(TSLS)[1:6]
B2 <- coef(OLS)[1:6]
VB1 <- vcov(TSLS)[1:6, 1:6]  #
VB2 <- vcov(OLS)[1:6, 1:6]
hQuant <- t(B1 - B2) %*% solve(VB1 - VB2) %*% (B1 - B2)
pchisq(hQuant, 6, lower.tail = FALSE) 
#  P = 0.2064239，因此无法拒绝原假设，IV回归与原来的OLS回归没有显著不同。

