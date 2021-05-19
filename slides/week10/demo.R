library("Matching")
data("lalonde")
attach(lalonde)


m1 <- glm(treat ~ age + educ + black + hisp + married +
  nodegr + re74 + re75, family = binomial, data = lalonde)

pm1 <- Match(Y = re78, Tr = treat, X = m1$fitted, estimand = "ATT", M = 1, replace = TRUE)
summary(pm1)  #最近邻匹配法 显著 样本可以重复使用 x是倾向值 ATT ATC ATE M = 一对一还是一对多
pm2 <- Match(Y = re78, Tr = treat, X = m1$fitted, estimand = "ATT", M = 1, replace = FALSE)
summary(pm2) #最近邻匹配法 显著 样本不可以重复使用

mb <- MatchBalance(treat ~ age + educ + black + hisp + married +
  nodegr + re74 + re75,
  match.out = pm1, nboots = 1000, data = lalonde)

# 是否平衡 前后都是不显著，两组间没有显著性差异。结果好

par(mfrow=c(1,2), mar=c(3,3,3,1), mgp=c(2,0.2,0), tcl=-0.2)
qqplot(lalonde$age[pm1$index.control], lalonde$age[pm1$index.treated],
    xlim=c(20,50), ylim=c(20,50),
    xlab="Control Observations", ylab="Treatment Observations", main="age")
abline(coef = c(0, 1), lty=2)
qqplot(lalonde$re74[pm1$index.control], lalonde$re74[pm1$index.treated],
    xlim=c(0,35000), ylim=c(0,35000),
    xlab="Control Observations", ylab="Treatment Observations", main="re74")
abline(coef = c(0, 1), lty=2)

# qqplot ，希望在线上，但结果不好。限制推论

X <- cbind(age, educ, black, hisp, married, nodegr, re74, re75)
set.seed(1)
library(rgenoud)
genout <- GenMatch(Tr=treat, X=m1$fitted, BalanceMatrix=X, estimand="ATT")
mout <- Match(Y=re78, Tr=treat, X=m1$fitted, estimand="ATT", Weight.matrix=genout)
summary(mout)
# 带数据方法 通常不用

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
# 敏感性检验 大于1.3时处理效用失效。是高敏感的结果，结果不过关。

hlsens(x = mout, Gamma = 2, GammaInc = 0.1)
# 1.6 没有通过敏感性检验 。重新调整

Y <- lalonde$re78
treat <- lalonde$treat

exactMatch <- Match(Y = Y, Tr = treat, X = m1$fitted, estimand = "ATT", exact=TRUE)
nearMatch <- Match(Y = Y, Tr = treat, X = m1$fitted, estimand = "ATT")
nearMatch3 <- Match(Y = Y, Tr = treat, X = m1$fitted, estimand = "ATT", M=3)
nearMatch3noReplace <- Match(Y = Y, Tr = treat, X = m1$fitted, estimand = "ATT", M=3, replace=FALSE)

csMatch <- Match(Y = Y, Tr = treat, X = m1$fitted, estimand = "ATT", CommonSupport=TRUE)
# 重合部分的检验

mout <- Match(Y=re78, Tr=treat, X=m1$fitted, estimand="ATT", Weight.matrix=genout)
# 丢掉
mout2 <- Match(Y=re78, Tr=treat, X=m1$fitted, estimand="ATT", Weight.matrix=genout, CommonSupport=TRUE)
# 没丢掉。 重合不是太大

#mdata <- mout$mdata
#ATE <- lm(Y ~ Tr, data=mdata)





library(MatchIt)
data(lalonde)
# 1对1最近邻匹配法
set.seed(1)
mNearest1v1 <- matchit(treat ~ age + educ + black + hispan + married +
  nodegree + re74 + re75, data = lalonde, method = "nearest", ratio=1)

mNearest1v1 <- matchit(treat ~ age + educ  + married +
                         nodegree + re74 + re75, data = lalonde, method = "nearest", ratio=1)
# 1对2最近邻匹配法
mNearest1v2 <- matchit(treat ~ age + educ + black + hispan + married +
  nodegree + re74 + re75, data = lalonde, method = "nearest", ratio=2)
# 1对1最近邻匹配法，控制组样本可替代重复使用
mNearestReplace <- matchit(treat ~ age + educ + black + hispan + married +
  nodegree + re74 + re75, data = lalonde, method = "nearest", ratio=1,
  replace = TRUE)
# 马氏距离匹配法
mMahalanobis <- matchit(treat ~ age + educ + black + hispan + married +
  nodegree + re74 + re75, data = lalonde, method = "nearest",
  distance = "mahalanobis")
# 最佳匹配法
mOptimal <- matchit(treat ~ age + educ + black + hispan + married +
  nodegree + re74 + re75, data = lalonde, method = "optimal", ratio=2)


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
# 需要通过模拟获得处理效用。不建议使用matchit包

library(Zelig)
z.out <- zelig(re78 ~ treat + age + educ + black + hispan + nodegree +
married + re74 + re75, data = mData, model = "ls")
xTreated <- setx(z.out, treat = 1)
xControl <- setx(z.out, treat = 0)
ATE <- sim(z.out, x = xControl, x1 = xTreated)

# 敏感性检验
library(rbounds)
YTreated <- mData$re78[mData$treat==1]
YControl <- mData$re78[mData$treat==0]
psens(x = YControl, y=YTreated, Gamma = 2, GammaInc = 0.1)
hlsens(x = YControl, y=YTreated, Gamma = 2, GammaInc = 0.1)
# 1.5部分。结果差不都

# 怎么做。
# 1. 选择replace
# 2. 考虑放进一个机器学习的方法or gary king optinal match 做备选方案
# 即使到1.7 1.8仍不过关。如果收入膨胀1.8在现实中是否合理。
# Match包标准误仍需要bootstrap