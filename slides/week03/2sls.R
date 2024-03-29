setwd("d:/")

#set random number seed
set.seed(1)

# set up the data
n <- 50000
a0 <- 3
b1 <- 2
b2 <- 5
b3 <- 3
a2 <- 1
p1 <- 6
p2 <- 8
p3 <- 4.5
p4 <- 1.5
p5 <- 1.2
u <- rnorm(n)
v <- rnorm(n)
x1 <- 3*runif(n)
x2 <- 10*runif(n)
z1 <- 5*runif(n)
z2 <- 3*runif(n)

y <- (a0 + b1*x1 + b2*x2 + b3*(a2 + p1*x1 + p2*x2 + p4*z1 + p5*z2 + v) + u)/(1-b3*p3)
x3 <- a2 + p1*x1 + p2*x2 + p3*y + p4*z1 + p5*z2 + v

# save the data
library(foreign)
dat <- cbind.data.frame(y, x1, x2, x3, z1, z2)
write.dta(dat, file="2sls.dta")

# run OLS
OLS <- lm(y~x1+x2+x3)
summary(OLS)
# note the coef of x3 is wrong

# run 2SLS
library(systemfit)
eq1 <- y ~ x1 + x2 + x3
eq2 <- x3 ~ x1 + x2 + y
system <- list(eq1, eq2)
inst <- ~ x1 + x2 + z1 + z2
TSLS <- systemfit(system, method="2SLS", inst=inst)
summary(TSLS)

# run 2SLS by hand
M1 <- lm(x3 ~ x1 + x2 + z1 + z2)
x3hat <- predict(M1)
cor(x3, x3hat)
M2 <- lm(y~x1+x2+x3hat)
summary(M2)
# note the coef of x3hat is correct but the se is wrong

# how to correct the SES above
MSE1 <- var(resid(M2))
yhat <- predict(OLS)
MSE2 <- var(y-yhat)
MSE2 <- var(resid(OLS))
ratio <- MSE2/MSE1
newSES <- ratio*summary(M2)$coefficients[,2]

bootFUN <- function(dat, nsims = 1000){
    n <- nrow(dat)
    out <- rep(NA, nsims)
    i <- 1
    while (i <= nsims){
        idx <- sample(1:n, n, replace=TRUE)
        tmp <- as.data.frame(dat[idx, ])
        M1 <- lm(x3 ~ x1 + x2 + z1 + z2, data=tmp)
        x3hat <- predict(M1)
        M2 <- lm(y~x1+x2+x3hat, data=tmp)
        out[i] <- coef(M2)["x3hat"]
        i <- i + 1
    }
    return(out)
}


out <- bootFUN(dat)
sd(out)

hist(out)
aa <- coef(M2)[4]
abline(v = mean(aa), col = 2)
abline(v = 3, col =3)

# now lets parallelize bootFUN()
bootFUN2 <- function(){
    n <- nrow(dat)
    idx <- sample(1:n, n, replace=TRUE)
    tmp <- as.data.frame(dat[idx, ])
    M1 <- lm(x3 ~ x1 + x2 + z1 + z2, data=tmp)
    x3hat <- predict(M1)
    M2 <- lm(y~x1+x2+x3hat, data=tmp)
    return(coef(M2)["x3hat"])
}

foo <- function() replicate(300, bootFUN2())

library(parallel)
cl <- makeCluster(parallel::detectCores())
#cl <- makeCluster(spec = 4)
clusterExport(cl = cl, c("dat", "bootFUN2")) # export object to each thread
tryCatch(res <- clusterCall(cl=cl, fun = foo), finally = stopCluster(cl))

sd(unlist(res))




# Heckman test for endogeneity
TM1 <- lm(y ~ x1 + x2 + x3 + x3hat)
summary(TM1)

# Test for Exogeneity of Z
res <- resid(TSLS)$eq1
TM2 <- lm(res~x1+x2+z1+z2)
summary(TM2)
R2 <- summary(TM2)$r.squared
cQuant <- n*R2
pchisq(cQuant, 3, lower.tail=FALSE)
#df的计算 ： 工具变量数- 被工具变量解释的工具变量数， K-1

# Hausmen test for the Endogeneity of X3
B1 <- coef(TSLS)[1:4]
B2 <- coef(OLS)[1:4]
VB1 <- vcov(TSLS)[1:4,1:4]  #协方差矩阵
VB2 <- vcov(OLS)[1:4,1:4]
hQuant <- t(B1-B2)%*%solve(VB1-VB2)%*%(B1-B2)
pchisq(hQuant, 4, lower.tail=FALSE)
#df为K
