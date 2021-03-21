setwd("d:/baidu/tsinghua/courses/quantii/week04")
library(foreign)
dat <- read.dta("./slides/week04/wdi.dta")
summary(dat)
dat <- dat[,c(-1,-2)]

n <- dim(dat)[1]
corMat <- cor(dat, use = "pairwise.complete.obs") #correlation matrix
#covMat <- cov(dat, use = "pairwise.complete.obs") #covariance matrix
#install.packages("corrplot")
library(corrplot)
res <- cor.mtest(dat, conf.level = 0.95)

corrplot(corMat, method = "ellipse", type = "lower", p.mat = res$p, sig.level = 0.05, order = "hclust")

library(psych)
# bartlett test
cortest.bartlett(corMat, n=n)

# bartlett test on raw data
#k <- dim(dat)[2]
#nj <- sapply(apply(dat, 2, na.exclude), length)
#N <- sum(nj)
#s2 <- apply(dat, 2, var, na.rm=TRUE)
#S2 <- sum((nj-1)*s2)/(n-k)
#cc <- 1 + (1/3*(k-1))*(sum(1/(nj-1)) - 1/(N-k))
#B <- ((N-k)*log(S2) - sum((nj-1)*log(s2)))/cc
#pchisq(B, df=k-1, lower.tail=FALSE)



# cronbach alpha
alpha(dat)


#raw_alpha: Cronbach’s α (values �? .7 or .8 indicate good reliability; Kline (1999))
#std.alpha: this should be similar to raw_alpha (we only need the raw alpha though)
#G6: Guttman’s lambda 6 (calculated from the squared multiple correlation or ‘smc�?)
#average_r: average inter-item correlation (this is used to calculate std.alpha)
#mean: scale mean (the mean of the means of all individuals)
#sd: scale sd

dat <- dat[,-5]

kmo <- KMO(dat)
print(kmo)
#0.00 to 0.49 unacceptable.
#0.50 to 0.59 miserable.
#0.60 to 0.69 mediocre.
#0.70 to 0.79 middling.
#0.80 to 0.89 meritorious.
#0.90 to 1.00 marvelous.

# anti-image correlation matrix
print(kmo$Image)
print(cov2cor(solve(corMat)))

#corMat <- cor(dat, use = "pairwise.complete.obs") #correlation matrix


# eigen value
ev <- eigen(corMat)
ev$values # find value > 1
sum(ev$values) # sum of eigenvalue = num of variable

# stats
pcafit <- princomp(~., data=dat, cor=TRUE)
summary(pcafit) # print variance accounted for
loadings(pcafit) # pc loadings
plot(pcafit,type="lines") # scree plot
#pcafit$scores # the principal components
#biplot(pcafit)


library(psych)
fit1 <- principal(dat, nfactors=3, rotate="varimax")
print(fit1, digits=2)


M1 <- factanal(~., data=dat, factors=3, scores="regression") # scores = "bartlett"
M2 <- update(M1, factor=3, roation = "varimax")
M2$uniquenesses
print(M2, digits=2, cutoff=.3, sort=TRUE)
loading <- M2$loadings[,1:2]
scores <- M2$scores

coefs <- solve(cor(dat, use = "pairwise.complete.obs")) %*% M2$loadings
factorScores <- scale(dat) %*% coefs


factorScores <- scale(dat)%*% solve(cor(dat, use="pairwise.complete.obs")) %*% M2$loadings
