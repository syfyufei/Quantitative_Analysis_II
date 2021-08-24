setwd("d:/baidu/tsinghua/courses/quantii/hwk03")
library(haven)
dat <- read_dta("abs.dta")
summary(dat)

corMat <- cor(dat, use = "pairwise.complete.obs") #correlation matrix
#covMat <- cov(dat, use = "pairwise.complete.obs") #covariance matrix
#install.packages("corrplot")
library(corrplot)
res <- cor.mtest(dat, conf.level = 0.95)

corrplot(corMat, method = "ellipse", type = "lower", p.mat = res$p, sig.level = 0.05, order = "hclust")

library(psych)
# bartlett test
cortest.bartlett(corMat, n=dim(dat)[1])


# cronbach alpha
alpha(dat)

# KMO

kmo <- KMO(dat)
print(kmo)
print(kmo$Image)


# eigen value
ev <- eigen(corMat)
ev$values # find value > 1
sum(ev$values>1)



# stats
pcafit <- princomp(~., data=dat, cor=TRUE)
summary(pcafit) # print variance accounted for
loadings(pcafit) # pc loadings
plot(pcafit,type="lines") # scree plot
#pcafit$scores # the principal components
#biplot(pcafit)



M1 <- factanal(~., data=dat, factors=3, scores="regression") # scores = "bartlett"
M2 <- update(M1, factor=3, roation = "varimax")
M2$uniquenesses
print(M2, digits=2, cutoff=.3, sort=TRUE)
loading <- M2$loadings[,1:2]
scores <- M2$scores


coefs <- solve(cor(dat, use = "pairwise.complete.obs")) %*% M2$loadings
factors <- scale(dat) %*% coefs


factors <- scale(dat)%*% solve(cor(dat, use="complete")) %*% M2$loadings
