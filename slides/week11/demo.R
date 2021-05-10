library("Matching")
data("lalonde")
attach(lalonde)


m1 <- glm(treat ~ age + educ + black + hisp + married +
  nodegr + re74 + re75, family = binomial, data = lalonde)

pm1 <- Match(Y = re78, Tr = treat, X = m1$fitted, estimand = "ATT", M = 1, replace = TRUE)
summary(pm1)
pm2 <- Match(Y = re78, Tr = treat, X = m1$fitted, estimand = "ATT", M = 1, replace = FALSE)
summary(pm2)

mb <- MatchBalance(treat ~ age + educ + black + hisp + married +
  nodegr + re74 + re75,
  match.out = pm1, nboots = 1000, data = lalonde)

par(mfrow=c(1,2), mar=c(3,3,3,1), mgp=c(2,0.2,0), tcl=-0.2)
qqplot(lalonde$age[pm1$index.control], lalonde$age[pm1$index.treated],
    xlim=c(20,50), ylim=c(20,50),
    xlab="Control Observations", ylab="Treatment Observations", main="age")
abline(coef = c(0, 1), lty=2)
qqplot(lalonde$re74[pm1$index.control], lalonde$re74[pm1$index.treated],
    xlim=c(0,35000), ylim=c(0,35000),
    xlab="Control Observations", ylab="Treatment Observations", main="re74")
abline(coef = c(0, 1), lty=2)


X <- cbind(age, educ, black, hisp, married, nodegr, re74, re75)
set.seed(1)
library(rgenoud)
genout <- GenMatch(Tr=treat, X=m1$fitted, BalanceMatrix=X, estimand="ATT")
mout <- Match(Y=re78, Tr=treat, X=m1$fitted, estimand="ATT", Weight.matrix=genout)
summary(mout)
mb1 <- MatchBalance(treat ~ age + educ + black + hisp + married +
  nodegr + re74 + re75,
  match.out = mout, nboots = 1000, data = lalonde)

par(mfrow=c(1,2), mar=c(3,3,3,1), mgp=c(2,0.2,0), tcl=-0.2)
qqplot(lalonde$age[mout$index.control], lalonde$age[mout$index.treated],
    xlim=c(20,50), ylim=c(20,50),
    xlab="Control Observations", ylab="Treatment Observations", main="age")
abline(coef = c(0, 1), lty=2)
qqplot(lalonde$re74[mout$index.control], lalonde$re74[mout$index.treated],
    xlim=c(0,35000), ylim=c(0,35000),
    xlab="Control Observations", ylab="Treatment Observations", main="re74")
abline(coef = c(0, 1), lty=2)


library(rbounds)
psens(x = mout, Gamma = 2, GammaInc = 0.1)
hlsens(x = mout, Gamma = 2, GammaInc = 0.1)

Y <- lalonde$re78
treat <- lalonde$treat

exactMatch <- Match(Y = Y, Tr = treat, X = m1$fitted, estimand = "ATT", exact=TRUE)
nearMatch <- Match(Y = Y, Tr = treat, X = m1$fitted, estimand = "ATT")
nearMatch3 <- Match(Y = Y, Tr = treat, X = m1$fitted, estimand = "ATT", M=3)
nearMatch3noReplace <- Match(Y = Y, Tr = treat, X = m1$fitted, estimand = "ATT", M=3, replace=FALSE)

csMatch <- Match(Y = Y, Tr = treat, X = m1$fitted, estimand = "ATT", CommonSupport=TRUE)

mout <- Match(Y=re78, Tr=treat, X=m1$fitted, estimand="ATT", Weight.matrix=genout)
mout2 <- Match(Y=re78, Tr=treat, X=m1$fitted, estimand="ATT", Weight.matrix=genout, CommonSupport=TRUE)


boots <- function(data){
    n <- dim(data)[1]
    idx <- sample(1:n, size=n, replace=TRUE)
    newDat <- data[idx,]
    return(newDat)
}

nSims <- 5000
att <- rep(NA, nSims)
for(s in 1:nSims){
    dat <- boots(lalonde)
    pFit <- glm(treat ~ age + educ + black + hisp + married + nodegr + re74 + re75, family = binomial, data = dat)
    mFit <- Match(Y = dat$re78, Tr = dat$treat, X = pFit$fitted, estimand = "ATT", M=3)
    att[s] <- mFit$est
}
sd(att)




library(MatchIt)
# data(lalonde)
# 1对1最近邻匹配法
set.seed(1)
mNearest1v1 <- matchit(treat ~ age + educ + black + hisp + married +
  nodegr + re74 + re75, data = lalonde, method = "nearest", ratio=1)
# 1对2最近邻匹配法
mNearest1v2 <- matchit(treat ~ age + educ + black + hisp + married +
  nodegr + re74 + re75, data = lalonde, method = "nearest", ratio=2)
# 1对1最近邻匹配法，控制组样本可替代重复使用
mNearestReplace <- matchit(treat ~ age + educ + black + hisp + married +
  nodegr + re74 + re75, data = lalonde, method = "nearest", ratio=1,
  replace = TRUE)
# 马氏距离匹配法
mMahalanobis <- matchit(treat ~ age + educ + black + hisp + married +
  nodegr + re74 + re75, data = lalonde, method = "nearest",
  distance = "mahalanobis")
# 最佳匹配法
mOptimal <- matchit(treat ~ age + educ + black + hisp + married +
  nodegr + re74 + re75, data = lalonde, method = "optimal", ratio=2)


print(mNearest1v1)
plot(mNearest1v1, type="QQ")
plot(mNearest1v1, type="jitter")
plot(mNearest1v1, type="hist")

sNearest <- summary(mNearest1v1, standardize = TRUE)
print(sNearest)
plot(sNearest)

mData <- match.data(mNearest1v1,  group = "all")
mDataTreated <- match.data(mNearest1v1, group = "treat")
mDataControl <- match.data(mNearest1v1,  group = "control")
library(Zelig)
z.out <- zelig(re78 ~ treat + age + educ + black + hisp + nodegr +
married + re74 + re75, data = mData, model = "ls")   # ate,通常只关心att
xTreated <- setx(z.out, treat = 1)
xControl <- setx(z.out, treat = 0)
ATE <- sim(z.out, x = xControl, x1 = xTreated)

library(rbounds)
YTreated <- mData$re78[mData$treat==1]
YControl <- mData$re78[mData$treat==0]
psens(x = YControl, y=YTreated, Gamma = 4, GammaInc = 0.1)  # 敏感性检验 0.05 下界，大于二显著
hlsens(x = YControl, y=YTreated, Gamma = 4, GammaInc = 0.1) # 敏感性检验 0  大于二包含零
