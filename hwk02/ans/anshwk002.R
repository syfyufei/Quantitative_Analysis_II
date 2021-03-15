setwd("G:/QuantII/hwk02")

#============================
# Q1
#============================
# call foreign package to read stata file
library(foreign)
Q1dat <- read.dta("2sls.dta")
names(Q1dat)

M1 <- lm(y ~ x1 + x2 + x3, data=Q1dat)
summary(M1)
coef(M1)["x3"]

# do 2sls manually
M2 <- lm(x3 ~ x1 + x2 + z1 + z2, data=Q1dat)
x3hat <- predict(M2, type="response")
#x3hat <- fitted(M2)
M3 <- lm(y ~ x1 + x2 + x3hat, data=Q1dat)
coef(M3)["x3hat"]

# do it using tsls()
install.packages("sem")
library(sem)
M4 <- tsls(y ~ x1 + x2 + x3, ~ x1 + x2 + z1 + z2, data=Q1dat)
summary(M4)
coef(M4)["x3"]



# run 2SLS
library(systemfit)
eq1 <- y ~ x1 + x2 + x3
eq2 <- x3 ~ x1 + x2 + y
system <- list(eq1, eq2)
inst <- ~ x1 + x2 + z1 + z2
TSLS <- systemfit(system, method="2SLS", inst=inst, data=Q1dat)
summary(TSLS)
coef(TSLS)

# test Z
TM1 <- lm(y ~ x1 + x2 + x3 + x3hat, dat=Q1dat)
summary(TM1)
# reject H0 that Z is a weak instrument


# test for exogeneity of z1 and z2
res <- resid(TSLS)$eq1
TM2 <- lm(res~x1+x2+z1+z2, dat=Q1dat)
summary(TM2)
R2 <- summary(TM2)$r.squared
n <- dim(Q1dat)[1]
cQuant <- n*R2
pchisq(cQuant, 4-1, lower.tail=FALSE) # cannot reject H0

# Hausam test for endogeneity of x3
B1 <- coef(TSLS)[1:4]
B2 <- coef(M1)[1:4]
VB1 <- vcov(TSLS)[1:4,1:4]
VB2 <- vcov(M1)[1:4,1:4]
hQuant <- t(B1-B2)%*%solve(VB1-VB2)%*%(B1-B2)
pchisq(hQuant, 4, lower.tail=FALSE)
 # reject H0


 # now lets parallelize bootFUN()
bootFUN <- function(dat){
    n <- nrow(dat)
    idx <- sample(1:n, n, replace=TRUE)
    tmp <- as.data.frame(dat[idx, ])
    M1 <- lm(x3 ~ x1 + x2 + z1 + z2, data=tmp)
    x3hat <- predict(M1)
    M2 <- lm(y~x1+x2+x3hat, data=tmp)
    return(coef(M2)["x3hat"])
}

foo <- function(n) replicate(n, bootFUN(Q1dat))

library(parallel)
cl <- makeCluster(parallel::detectCores())
#cl <- makeCluster(spec = 4)
clusterExport(cl = cl, c("Q1dat", "bootFUN")) # export object to each thread
tryCatch(res <- clusterCall(cl=cl, fun = foo, n=250), finally = stopCluster(cl))

sd(unlist(res))
