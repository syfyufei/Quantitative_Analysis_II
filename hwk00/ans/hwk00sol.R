

setwd("H:\\QauntII\hwk00\")

library(foreign)
dat <- read.dta("kidiq.dta")


# p3
M1 <- lm(kid_score ~ mom_hs, data=dat)
summary(M1)

# p5  a twoway scatter plot
attach(dat)
par(mar=c(3,3,1,0.5), mgp=c(1.5,0.2,0), tcl=-0.2)
plot(x = mom_hs, y = kid_score, type = "n", xlim = c(-0.5,1.5), ylim = c(0,150),
    yaxs="i", xlab = "Mother completed high school", ylab = "Child test scores",
    axes=FALSE, frame.plot = TRUE)
points(x = jitter(mom_hs), y = kid_score, pch = 1)
axis(1, at = c(0,1), labels = c("No", "Yes"), tck=FALSE)
axis(2, at = seq(0,150,by=50))
curve(coef(M1)[1] + coef(M1)[2]*x, from = 0, to = 1, add=TRUE)


# p6
M2 <- lm(kid_score ~ mom_iq, data=dat)
summary(M2)

# p8
par(mar=c(3,3,1,0.5), mgp=c(1.5,0.2,0), tcl=-0.2)
plot(x = mom_iq, y = kid_score, type = "n", xlim = c(60,140), ylim = c(0,150),
    yaxs="i", xlab = "Mother IQ scores", ylab = "Child test scores",
    axes=FALSE, frame.plot = TRUE)
points(x = mom_iq, y = kid_score, pch = 1)
axis(1, at = seq(60,140,by=20))
axis(2, at = seq(0,150,by=50))
curve(coef(M2)[1] + coef(M2)[2]*x, from = min(mom_iq), to = max(mom_iq), add = TRUE)


# p9
M3 <- lm(kid_score ~ mom_hs + mom_iq, data=dat)
summary(M3)

#p11
par(mar=c(3,3,1,0.5), mgp=c(1.5,0.2,0), tcl=-0.2)
plot(x = mom_iq, y = kid_score, type = "n", xlim = c(60,140), ylim = c(0,150),
    yaxs="i", xlab = "Mother IQ scores", ylab = "Child test scores",
    axes=FALSE, frame.plot = TRUE)
points(x = mom_iq[mom_hs==1], y = kid_score[mom_hs==1], pch = 1)
points(x = mom_iq[mom_hs==0], y = kid_score[mom_hs==0], pch = 1, col=2)
axis(1, at = seq(60,140,by=20))
axis(2, at = seq(0,150,by=50))
curve(coef(M3)[1] + coef(M3)[2]*1 + coef(M3)[3]*x, from = min(mom_iq[mom_hs==1]), to = max(mom_iq[mom_hs==1]), add = TRUE)
curve(coef(M3)[1] + coef(M3)[3]*x, from = min(mom_iq[mom_hs==0]), to = max(mom_iq[mom_hs==0]), col=2, add = TRUE)
text(x=min(mom_iq[mom_hs==1]), y=coef(M3)[1] + coef(M3)[2] + coef(M3)[3]*min(mom_iq[mom_hs==1]), "mom_hs==1", adj=1)
text(x=min(mom_iq[mom_hs==0]), y=coef(M3)[1] + coef(M3)[3]*min(mom_iq[mom_hs==0]), "mom_hs==0", adj=1, col=2)


# p15
M4 <- lm(kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq, data=dat)
summary(M4)

# P17
par(mar=c(3,3,1,5), mgp=c(1.5,0.2,0), tcl=-0.2)
plot(x = mom_iq, y = kid_score, type = "n", xlim = c(60,140), ylim = c(0,150),
    yaxs="i", xlab = "Mother IQ scores", ylab = "Child test scores",
    axes=FALSE, frame.plot = TRUE)
points(x = mom_iq[mom_hs==1], y = kid_score[mom_hs==1], pch = 1)
points(x = mom_iq[mom_hs==0], y = kid_score[mom_hs==0], pch = 1, col=2)
axis(1, at = seq(60,140,by=20))
axis(2, at = seq(0,150,by=50))
curve(coef(M4)[1] + coef(M4)[2] + coef(M4)[3]*x + coef(M4)[4]*x, from = min(mom_iq[mom_hs==1]), to = max(mom_iq[mom_hs==1]), add = TRUE)
curve(coef(M4)[1] + coef(M4)[3]*x, from = min(mom_iq[mom_hs==0]), to = max(mom_iq[mom_hs==0]), col=2, add = TRUE)
text(x=max(mom_iq[mom_hs==1]), y=coef(M4)[1] + coef(M4)[2] + coef(M4)[3]*max(mom_iq[mom_hs==1]) + coef(M4)[4]*max(mom_iq[mom_hs==1]), "mom_hs==1", adj=0, xpd=NA)
text(x=min(mom_iq[mom_hs==0]), y=coef(M4)[1] + coef(M4)[3]*min(mom_iq[mom_hs==0]), "mom_hs==0", adj=1, col=2)

# p25
stdize <- function(x){
    u <- x - mean(x)
    se <- sd(x)
    newX <- u/se
    return(newX)
}



M5 <- lm(stdize(kid_score) ~ stdize(mom_hs) + stdize(mom_iq), data=dat)
summary(M5)


# p29
M6 <- lm(log(kid_score) ~ mom_hs + log(mom_iq), data=dat)
summary(M6)

# p34
M7 <- lm(kid_score ~ factor(mom_work), data=dat)
summary(M7)

# P35
center <- function(x){
    return(x - mean(x))
}
M8 <- lm(kid_score ~ factor(mom_work)*center(mom_iq), data=dat)
summary(M8)
