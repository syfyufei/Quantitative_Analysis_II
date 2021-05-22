

setwd("E:/SynologyDrive/Github/Quantitative_Analysis_II/slides/week13")
library(foreign)
dat <- read.dta("garrum6.dta")
library(plm)

mp <- plm(gdp ~ oild + demand + corp + leftlab + clint, data=dat, index =
"country", model = "pooling")
summary(mp)

M0 <- lm(gdp ~ oild + demand + corp + leftlab + clint, data=dat)
summary(M0)  # 与mp结果相等
mf <- plm(gdp ~ oild + demand + corp + leftlab + clint, data=dat, index =
"country", model = "within")
summary(mf)
# F-test manually
# H0: u=0
df1 <- df.residual(mp)-df.residual(mf) # number of dummies K-1
df2 <- df.residual(mf)                 # degree of freedom of fixed effect model (N-K-1)
ssrp <- sum(resid(mp)^2)               # sum of residual square of pooling model
ssrf <- sum(resid(mf)^2)               # sum of residual square of fixed effect model
fQuant <- df2*(ssrp-ssrf)/ssrf/df1
pf(fQuant, df1, df2, lower.tail=FALSE) 
# significant means reject H0, ie u#0, so fixed effect model is appropriate
# F-test in a function
pFtest(mf, mp) # 统计显著  固定效应模型合适



mr <- plm(gdp ~ oild + demand + corp + leftlab + clint, data=dat, index =
"country", model = "random") #随机效应模型
summary(mr)
# Hausmen test manually
# H0 Re is appropriate, fe=re, re is more efficient
# Ha Re is inappropriate, fe#re, re is biased/inconsistent
mfB <- coef(mf)[1:5]
mrB <- coef(mr)[2:6]
mfV <- vcov(mf)[1:5,1:5]
mrV <- vcov(mr)[2:6,2:6]
hQuant <- t(mfB-mrB)%*%solve(mfV-mrV)%*%(mfB-mrB)
hQuant
pchisq(hQuant, 5, lower.tail=FALSE)
phtest(mf, mr) # 随机效应模型更有效率。只提取自变量相同的系数做检验。 检验结果统计显著的，因此随机效应模型比较不合适，固定效应模型比较合适。
# 检验结果只是参考

#PCSE manually
mOLS <- lm(gdp ~ oild + demand + corp + leftlab + clint, data=dat)
summary(mOLS)

nK <- length(unique(dat$country))
nT <- length(unique(dat$year))
E <- matrix(resid(mOLS), nrow=nT, ncol=nK)
S <- t(E)%*%E/nT
IT <- diag(nT)
Omega <- S%x%IT
X <- model.matrix(mOLS)
XXinv <- solve(t(X)%*%X)
middle <- t(X)%*%Omega%*%X
PCSE <- sqrt(diag(XXinv %*% middle %*% XXinv))
# PCSE with function

library(pcse)

mPCSE <- pcse(mOLS, dat$country, dat$year)
summary(mPCSE)

library(panelAR)
mPCSEAR1 <- panelAR(gdp ~ oild + demand + corp + leftlab + clint, data=dat,
panelVar = "country", timeVar = "year", autoCorr = "ar1",  panelCorrMethod =
"pcse")
summary(mPCSEAR1)
# # AR + pcse

mPCSEArellano <- panelAR(gdp ~ oild + demand + corp + leftlab + clint +
factor(country), data=dat, panelVar = "country", timeVar = "year", autoCorr
= "none",  panelCorrMethod = "pcse") # Arellano不考虑序相关的做法
summary(mPCSEArellano)

# pcse 无法解决面板不平衡和面板缺失问题