#########################################################
##  政治数据分析
##  Bonus: OLS and Diag
##  Ye Xinyi, Dong Yanyao, Wu Wenquan
##  2020年11月22日
#########################################################


##################1. OLS Regression #####################

library(stargazer)
library(tidyverse)
pokemon <- read.csv("./evolution/pokemon75.csv")  
#see https://www.openintro.org/book/statdata/index.php?data=pokemon
summary(pokemon)
stargazer(pokemon, type="text")

fit1 = lm(cp_new ~ species + hp + weight + height + cp, data=pokemon)

# fit1 = lmfit1 = lm(cp_new ~ species + hp + weight + height + cp, data=pokemon)(cp_new ~ . - notes - name, data=pokemon) 
# fit2 = lm(cp_new ~ species + hp + weight + height + cp + type.1 + type.2, data=pokemon) 
summary(fit1)


# 调用系数
coefficients(fit1)
# 置信区间， 95% 和99%
confint(fit1) 
confint(fit1, level=0.99)
# predicted values 
fitted(fit1)
# residuals of the fitted model
residuals(fit1)
# prediction
predict(fit1)
# 对系数绘图
library(broom) 
library(ggplot2)
library(GGally)
ggcoef(fit1, exclude_intercept = TRUE)
# 汇报回归结果
stargazer(fit1,
          type="text", 
          no.space=TRUE, 
          model.names=FALSE,
          column.labels = c("model 1"))

#####################2. Diagnose ######################

library(broom)
tidy(fit1)
pokemon2 <- augment_columns(fit1, pokemon)
names(pokemon2)
detach(pokemon)
attach(pokemon2)


##2.1 Perfect and High Multicollinearity
#得到的数值如果为1，说明完全独立，大于10是严重的共线性
library(carData)
library(car) 
vif(fit1)

##2.2 Linearity 
plot(.resid ~ .fitted,ylab="Residuals") 

#or use ggplot
library(gridExtra)
r_cp_new<-ggplot(pokemon2, aes(x=.fitted, y=.resid)) + geom_hline(yintercept=0) +
  geom_point() + geom_smooth(method='loess', se=TRUE)

r_cp <- ggplot(pokemon2, aes(x=cp, y=.resid)) + geom_hline(yintercept=0) +
  geom_point() + geom_smooth(method='loess', se=TRUE)
grid.arrange(r_cp_new, r_cp, ncol=2)

## 2.3 Model Specification
# 使用F检验
# null hypothesis：two models are identical
fit2 <- lm(cp_new ~ species + hp + weight + height + cp +I(.fitted^2), data=pokemon)
summary(fit2)
anova(fit1, fit2)


## 2.4 Constant Error Variance/Heteroscedasticity
plot(fitted(fit1),residuals(fit1),xlab="Fitted",ylab="Residuals")
abline(h=0)

#formal test:Cook/Weisberg score test and Breush/Pagan test
# both:null hypothesis is constant error variance,  alternative hypothesis is that the error variance is not constant
# Cook/Weisberg score test of constant error variance
library(car)
ncvTest(fit1)
# Breush/Pagan test of constant error variance
library(lmtest) 
bptest(fit1)
#出现异方差，解决方法是重新计算异方差下的标准误


## 2.5 Independence of Error Term Observations
plot<-ggplot(pokemon2, aes(.fitted, .resid)) + geom_hline(yintercept=0) +
  geom_point() + facet_wrap(~species)
plot
#formal test
#Durbin–Watson test:The null hypothesis is that the errors are uncorrelated
library(lmtest)
dwtest(fit1)

## 2.6 normality test
qqnorm(residuals(fit1),ylab="Residuals",main="")
qqline(residuals(fit1))
#formal test
shapiro.test(pokemon2$.resid) 
options(scipen = 100) # 关掉科学计数法
shapiro.test(pokemon2$.resid)

## 2.7.Influential Observations
library(car)
influencePlot(fit1)
cook <- cooks.distance(fit1)
library(faraway)
halfnorm(cook,3,labs=pokemon2$name,ylab="Cook’s distances")


##################3.1    ###########################################
######################################################################


##################1. OLS Regression #####################

library(stargazer)
library(tidyverse)
pokemon <- read.csv("./evolution/pokemon75_without_Eevee.csv")  
#see https://www.openintro.org/book/statdata/index.php?data=pokemon
summary(pokemon)
stargazer(pokemon, type="text")


fit1 = lm(cp_new ~ species + hp + weight + height + cp, data=pokemon)
# fit1 = lm(cp_new ~ . - notes - name, data=pokemon) 
# fit2 = lm(cp_new ~ species + hp + weight + height + cp + type.1 + type.2, data=pokemon) 
summary(fit1)


# 调用系数
coefficients(fit1)
# 置信区间， 95% 和99%
confint(fit1) 
confint(fit1, level=0.99)
# predicted values 
fitted(fit1)
# residuals of the fitted model
residuals(fit1)
# prediction
predict(fit1)
# 对系数绘图
library(broom) 
library(ggplot2)
library(GGally)
ggcoef(fit1, exclude_intercept = TRUE)
# 汇报回归结果
stargazer(fit1,
          type="text", 
          no.space=TRUE, 
          model.names=FALSE,
          column.labels = c("model 1"))

#####################2. Diagnose ######################

library(broom)
tidy(fit1)
pokemon2 <- augment_columns(fit1, pokemon)
names(pokemon2)
detach(pokemon)
attach(pokemon2)


##2.1 Perfect and High Multicollinearity
#得到的数值如果为1，说明完全独立，大于10是严重的共线性
library(carData)
library(car) 
vif(fit1)

##2.2 Linearity 
plot(.resid ~ .fitted,ylab="Residuals") 

#or use ggplot
library(gridExtra)
r_cp_new<-ggplot(pokemon2, aes(x=.fitted, y=.resid)) + geom_hline(yintercept=0) +
  geom_point() + geom_smooth(method='loess', se=TRUE)

r_cp <- ggplot(pokemon2, aes(x=cp, y=.resid)) + geom_hline(yintercept=0) +
  geom_point() + geom_smooth(method='loess', se=TRUE)
grid.arrange(r_cp_new, r_cp, ncol=2)



## 2.4 Constant Error Variance/Heteroscedasticity
plot(fitted(fit1),residuals(fit1),xlab="Fitted",ylab="Residuals")
abline(h=0)

#formal test:Cook/Weisberg score test and Breush/Pagan test
# both:null hypothesis is constant error variance,  alternative hypothesis is that the error variance is not constant
# Cook/Weisberg score test of constant error variance
library(car)
ncvTest(fit1)
# Breush/Pagan test of constant error variance
library(lmtest) 
bptest(fit1)
#出现异方差，解决方法是重新计算异方差下的标准误


## 2.5 Independence of Error Term Observations
plot<-ggplot(pokemon2, aes(.fitted, .resid)) + geom_hline(yintercept=0) +
  geom_point() + facet_wrap(~species)
plot
#formal test
#Durbin–Watson test:The null hypothesis is that the errors are uncorrelated
library(lmtest)
dwtest(fit1)

## 2.6 normality test
qqnorm(residuals(fit1),ylab="Residuals",main="")
qqline(residuals(fit1))
#formal test
shapiro.test(pokemon2$.resid) 
options(scipen = 100) # 关掉科学计数法
shapiro.test(pokemon2$.resid)

## 2.7.Influential Observations
library(car)
influencePlot(fit1)
cook <- cooks.distance(fit1)
library(faraway)
halfnorm(cook,3,labs=pokemon2$name,ylab="Cook’s distances")

#####################3.2  ##########################


## 2.3 Model Specification
# 使用F检验
# null hypothesis：two models are identical
fit2 <- lm(cp_new ~ species + hp + weight + height + cp + I(hp^2), data=pokemon)
summary(fit2)
anova(fit1, fit2)



##############################3

pokemon3 <- pokemon %>% filter(species!="Eevee")

fit3 = lm(cp_new ~ species + hp + weight + height + cp, data=pokemon)


# 汇报回归结果
stargazer(fit3,
          type="text", 
          no.space=TRUE, 
          model.names=FALSE,
          column.labels = c("model 2"))

library(broom)
tidy(fit1)
pokemon4 <- augment_columns(fit3, pokemon3)
names(pokemon4)
detach(pokemon3)
attach(pokemon3)



##2.2 Linearity 
plot(.resid ~ .fitted,ylab="Residuals") 
abline(h=0)
#or use ggplot
library(gridExtra)
r_cp_new<-ggplot(pokemon3, aes(x=.fitted, y=.resid)) + geom_hline(yintercept=0) +
  geom_point() + geom_smooth(method='loess', se=TRUE)

r_cp <- ggplot(pokemon3, aes(x=cp, y=.resid)) + geom_hline(yintercept=0) +
  geom_point() + geom_smooth(method='loess', se=TRUE)
grid.arrange(r_cp_new, r_cp, ncol=2)

## 2.4 Constant Error Variance/Heteroscedasticity
plot(fitted(fit3),residuals(fit3),xlab="Fitted",ylab="Residuals")
abline(h=0)

## 2.6 normality test
qqnorm(residuals(fit3),ylab="Residuals",main="")
qqline(residuals(fit3))
