# 1.1 普通话越好的人会不会越热衷于非正式政治参与。
# y: D12c . 在这些活动或行动中，您是否担任过以下角色？
# x1: A8a 您个人去年全年的总收入是多少？
# x2: A10. 您目前的政治面貌是： 
# x3: A50. 您觉得自己说普通话的能力是什么水平？

#1.2 政治参与为因变量，收入（x1）、党员身份（x2），普通话水平（x3）为自变量

library(foreign)
library(DescTools)
library(dplyr)
library(lmtest)
library(BSDA)
library(stargazer)
options(scipen = 200, digits = 2) 

data <- read.dta('./HW1/cgss2010_12.dta') 

mydata <- data[,c('a49', 'a8a', 'a10','d12c')] %>% 
  na.omit()

mydata <- mydata[mydata$a10 != '拒绝回答缺失值' &
                   mydata$d12c != '拒绝回答' &
                   mydata$d12c != '不知道' &
                   mydata$a49 != '拒绝回答缺失值' &
                   mydata$a49 != '不知道缺失值', ]


mydata <- mydata %>% mutate( cpcer = ifelse(a10 == '共产党员', TRUE, FALSE)) %>% 
  mutate(protest = ifelse( d12c == '从未参与', FALSE, TRUE))

mydata <- mydata[, c('a8a', 'a49', 'cpcer', 'protest')]
names(mydata) <- c('income', 'nation_lan', 'cpcer', 'protest')
mydata$income <- as.numeric(mydata$income)

#-----------1.4---------
m1 <- glm(protest ~ income + cpcer + nation_lan, 
          family = binomial(link = "logit"), data = mydata)
m0 <- glm(protest ~ 1,
          family = binomial(link = "logit"), data = mydata)

summary(m1)
stargazer(m1, type = 'text')
summary(m0)

coef(m1)
confint(m1)
exp(coef(m1))
exp(confint(m1))

#--------1.5-----------
anova(m1, test="Chisq")

#---------1.6----------
PseudoR2(m1)

#---------1.7----------
z.test(m1, sigma.x=0.5, conf.level=0.95)


#--------1.9-----------
invlogit<-function(x){
  1/(1+exp(-x))
}
coef(m1)[3]

# logLik()
attach(mydata)
par(mar=c(3,3,3,2), mgp=c(1.5,0.2,0), tcl=-0.2)
plot(x = income, y = protest, type = 'n', xlab = 'income', ylab = 'protest', axes = FALSE, 
     frame.plot = TRUE, xlim = c (0,9999999), yaxs = 'i')                   
points(x = income,  y = jitter(as.numeric(protest), factor = 1.5), pch = '.')
axis(2, at=c(0,0.5,1))
axis(1)
curve(invlogit(coef(m1)[1] + coef(m1)[2] * x + coef(m1)[3] * TRUE + coef(m1)[4] * 1), from = min(income), to = max(income), add = TRUE, col=2)
curve(invlogit(coef(m1)[1] + coef(m1)[2] * x + coef(m1)[3] * FALSE + coef(m1)[4] * 1), from = min(income), to = max(income), add = TRUE, col=1)
text(x = max(income), y = invlogit( coef(m1)[1] + coef(m1)[2] * max(income) + coef(m1)[3] * c(0,1) + coef(m1)[4] * 1), labels=c("非党员", "党员"), 
     adj=0, xpd=NA)
save.image(file = './fig/fig1.jpg' ,width = 8, height = 5, dpi = 100)
#---------------interaction term----------
#-----------2.4---------
m2 <- glm(protest ~ income + cpcer + nation_lan + cpcer:income, 
          family = binomial(link = "logit"), data = mydata)
summary(m2)

#--------2.5-----------
anova(m2, test="Chisq")

#---------2.6----------
PseudoR2(m2)

#---------2.7----------
z.test(m2, sigma.x=0.5, conf.level=0.95)

#--------2.9-----------
invlogit<-function(x){
  1/(1+exp(-x))
}

# logLik()

# jpeg(p.picture,width=800 ,height = 500)

par(mar=c(3,3,3,2), mgp=c(1.5,0.2,0), tcl=-0.2)
plot(x = income, y = protest, type = 'n', xlab = 'income', ylab = 'protest', axes = FALSE, 
     frame.plot = TRUE, xlim = c (0,9999999), yaxs = 'i')                   
points(x = income,  y = jitter(as.numeric(protest), factor = 1.5), pch = '.')
axis(2, at=c(0,0.5,1))
axis(1)
curve(invlogit(coef(m2)[1] + coef(m2)[2] * x + coef(m2)[3] * TRUE + coef(m2)[4] * 1 + coef(m2)[8] * 1 * x), from = min(income), to = max(income), add = TRUE, col=2)
curve(invlogit(coef(m2)[1] + coef(m2)[2] * x + coef(m2)[3] * FALSE + coef(m2)[4] * 1), from = min(income), to = max(income), add = TRUE, col=1)
text(x = max(income), y = invlogit( coef(m1)[1] + coef(m1)[2] * max(income) + coef(m1)[3] * c(0,1) + coef(m1)[4] * 1 + 
                                      coef(m2)[8] *  c(0,1) * max(income)), labels=c("非党员", "党员"), adj=0, xpd=NA)


