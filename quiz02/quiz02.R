library(foreign)
library(tidyverse)

# question1 ####

dat <- read.dta('./quiz02/ts.dta')

pdf("tsAcf.pdf", width=5, height=3.5)
par(mar=c(3,3,1,1), mgp=c(2,0.2,0),tcl=-0.2)
acf(dat$ts)
dev.off()

pdf("tsPAcf.pdf", width=5, height=3.5)
par(mar=c(3,3,1,1), mgp=c(2,0.2,0),tcl=-0.2)
pacf(dat$ts)
dev.off()

# AR(2)

# arima(dat$ts)
# aic_list <- data.frame(i, j, k, AIC(M0))



for (i in 1:5) {
  for (j in 1:5){
    for (k in 1:5){
      M0 <- arima(dat$ts, order=c(i,j,k))
      # aic <- AIC(M0)
      # aic_now <- data.frame(i, j, k, AIC(M0))
      # merge(aic_list, aic_now)
      print(i)
      print(j)
      print(k)
      print(AIC(M0))
    }
  }
}


# question2 ####
library(arm)
dat1 <- read_csv('./aclpData.csv')
dat2 <- na.omit(dat1)
attach(dat2)

M1 <- lmer(log(GDP) ~ REG + BRITCOL + OIL + OPEN + POP +
             (1 + REG|COUNTRY) + (1|REGION) + (1|YEAR), data = dat2)  
