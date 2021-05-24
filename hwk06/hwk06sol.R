
# 设定工作目录
setwd("d:/baidu/data/cgss/cgss2010")

# 呼叫foriegn套件以读取stata数据
library(foreign)
dat <- read.dta("cgss2010.dta", convert.factor=FALSE)

# 变量处理
treat <- dat$a10
treat <- ifelse(treat==4, 1, 0)

Y <- ifelse(dat$a8a>=9999997, NA, dat$a8a)
log.Y <- log(Y+1)

male <- dat$a2
male <- ifelse(male==2, 1, 0)

race <- dat$a4
han <- ifelse(race==2, 1, 0)
meng <- ifelse(race==3, 1, 0)
man <- ifelse(race==4, 1, 0)
hui <- ifelse(race==5, 1, 0)
zang <- ifelse(race==6, 1, 0)
zhuang <- ifelse(race==7, 1, 0)
wei <- ifelse(race==8, 1, 0)

age <- dat$a3a
age <- ifelse(age < 0, age, age)
age <- 2010 - age

mandarin <- ifelse(dat$a50 < 1, NA, dat$a50)

english <- ifelse(dat$a52 < 1, NA, dat$a52)

faCCPmember <- ifelse(dat$a89c==1, 1,
        ifelse(dat$a89c<1, NA, 0))


faEdu <- ifelse(dat$a89b %in% c(1,2,3), 1,
    ifelse(dat$a89b %in% 4, 2,
    ifelse(dat$a89b %in% c(5,6,7,8), 3,
    ifelse(dat$a89b %in% c(9,10,11,12), 4,
    ifelse(dat$a89b %in% 13, 5, NA)))))

edu <- ifelse(dat$a7a %in% c(1,2,3), 1,
    ifelse(dat$a7a %in% 4, 2,
    ifelse(dat$a7a %in% c(5,6,7,8), 3,
    ifelse(dat$a7a %in% c(9,10,11,12), 4,
    ifelse(dat$a7a %in% 13, 5, NA)))))


height <- ifelse(dat$a13<110, NA, dat$a13)


weight <- ifelse(dat$a14<70, NA, dat$a14)

# 将变量另存为新的数据物件

newdata <- cbind.data.frame(hincome = Y, treat, male, age,
#race,
    edu, height, weight, faEdu, faCCPmember, english, mandarin,
    han, meng, man, hui, zang,
	zhuang, wei)

# 将数据中的缺失值一并排除
newdata <- na.exclude(newdata)

# 将数据置于记忆体中
attach(newdata)

# 使用logit回归估计倾向值
M1 <- glm(treat ~ male + age + factor(edu) +
    mandarin + english + faCCPmember + factor(faEdu) + height + weight +
	han + meng + man + hui + zang + zhuang + wei,
	family=binomial(link="logit"), data=newdata)

# 呼叫Matching套件
library(Matching)

# 1对1最近邻匹配法对照组样本可替换
M2 <- Match(Y=newdata$hincome, Tr=newdata$treat, X=M1$fitted, M=1, replace=TRUE)
# 1对5最近邻匹配法对照组样本可替换
M3 <- Match(Y=newdata$hincome, Tr=newdata$treat, X=M1$fitted, M=5, replace=TRUE)

# 检验平衡
MB1 <- MatchBalance(treat ~ male + age + factor(edu) +
    mandarin + english + faCCPmember + factor(faEdu) + height + weight +
	han + meng + man + hui + zang + zhuang + wei,
  match.out = M2, nboots = 1000, data = newdata)

MB2 <- MatchBalance(treat ~ male + age + factor(edu) +
    mandarin + english + faCCPmember + factor(faEdu) + height + weight +
	han + meng + man + hui + zang + zhuang + wei,
  match.out = M3, nboots = 1000, data = newdata)

# 可视化检验平衡
par(mfrow=c(1,2), mar=c(3,3,3,1), mgp=c(2,0.2,0), tcl=-0.2)
qqplot(newdata$age[M3$index.control], newdata$age[M3$index.treated],
    #xlim=c(20,50), ylim=c(20,50),
    xlab="Control Observations", ylab="Treatment Observations", main="age")
abline(coef = c(0, 1), lty=2)

# 1对5最近邻匹配法对照组样本可替换加上重合要求
M4 <- Match(Y=hincome, Tr=newdata$treat, X=M1$fitted, M=5, replace=TRUE, CommonSupport=TRUE)


# 呼叫rbounds套件
library(rbounds)
psens(x = M3, Gamma = 2, GammaInc = 0.1)
hlsens(x = M3, Gamma = 2, GammaInc = 0.1)
