library(tidyverse)
library(haven)
library(arm)
library(stargazer)
library(modelsummary)
library(broom.mixed)
library(sjstats)
library(effects)
library(cowplot)
sjPlot::plot_model(mod)
options(scipen = 100)
options(digits = 2)

# library(foreign)
# dta1 <- read.table('./report/data/W1.dat',header = T,sep=" ")
w1 <- read_sav('./report/data/W1.sav')
w2 <- read_sav('./report/data/W2.sav')
w3 <- read_sav('./report/data/W3.sav')
w4 <- read_sav('./report/data/W4.sav')

# select and bind -----
# w1
demo_w1 <- w1$q098
# 1 'Not at all satisfied'
# 2 'Not very satisfied'
# 3 'Fairly satisfied'
# 4 'Very satisfied'
# 98 'Don't know'
# 99 'No answer'

support_w1 <- w1$q130
# 1 'Strongly agree'
# 2 'Somewhat agree'
# 3 'Somewhat disagree'
# 4 'Strongly disagree'
# 98 'Don't know'
# 99 'No answer'


city_w1 <- w1$level3


year_w1 <- w1$yrsurvey
gender_w1 <- w1$se002
# cohord_w1 <- w1$se003
age_w1 <- w1$se003a
edu_w1 <- w1$se005a # year
region_w1 <- w1$se006
dialect_w1 <- w1$se014
income_w1 <- w1$se009
plur_w1 <- w1$q135 #pluralism
formal_part_w1 <- w1$q075 
# fee_part_w1 <- w1$q079
# sign_part_w1 <- w1$q079
march_part_w1 <- w1$q079
# force_part_w1 <- w1$q079

sub_w1 <- cbind.data.frame(demo_w1,
                           support_w1,
                           city_w1,
                           year_w1,
                           gender_w1,
                           age_w1,
                           edu_w1,
                           region_w1,
                           dialect_w1,
                           income_w1,
                           plur_w1,
                           formal_part_w1,
                           # fee_part_w1,
                           # sign_part_w1,
                           march_part_w1
                           # force_part_w1
                           )

names(sub_w1) <- c('demo',
                   'support',
                   'city',
                   'year',
                   'gender',
                   'age',
                   'edu',
                   'region',
                   'dialect',
                   'income',
                   'plur',
                   'formal',
                   # 'fee',
                   # 'sign',
                   'march'
                   # 'force',
                   )

sub_w1$formal <- ifelse(sub_w1$formal == 9 |
                          sub_w1$formal == 98 |
                          sub_w1$formal == 99, 
                      NA_real_, sub_w1$formal)

# sub_w1$fee <- ifelse(sub_w1$fee == 9 |
#                        sub_w1$fee == 98 |
#                        sub_w1$fee == 99, 
#                         NA_real_, sub_w1$fee)
# sub_w1$sign <- ifelse(sub_w1$sign == 9 |
#                         sub_w1$sign == 98 |
#                         sub_w1$sign == 99, 
#                         NA_real_, sub_w1$sign)
sub_w1$march <- ifelse(sub_w1$march == 9 |
                         sub_w1$march == 98 |
                         sub_w1$march == 99, 
                        NA_real_, sub_w1$march)

# sub_w1$force <- ifelse(sub_w1$force == 9 |
#                          sub_w1$force == 98 |
#                          sub_w1$force == 99, 
#                        NA_real_, sub_w1$force)

# w2
demo_w2 <- w2$q098
support_w2 <- w2$q130
city_w2 <- w2$level3
year_w2 <- rep(2008,length(demo_w2))
gender_w2 <- w2$se002
# cohord_w1 <- w1$se003
age_w2 <- w2$se003a
edu_w2 <- w2$se005a # year
region_w2 <- w2$se006
dialect_w2 <- w2$se014
income_w2 <- w2$se009 
plur_w2 <- w2$q135  #pluralism

formal_part_w2 <- w2$q075 
# fee_part_w2 <- w2$qII86
# sign_part_w2 <- w2$qII87
march_part_w2 <- w2$qII88
# force_part_w2 <- w2$qII89




sub_w2 <- cbind.data.frame(demo_w2,
                           support_w2,
                           city_w2,
                           year_w2,
                           gender_w2,
                           age_w2,
                           edu_w2,
                           region_w2,
                           dialect_w2,
                           income_w2,
                           plur_w2,
                           formal_part_w2,
                           # fee_part_w2,
                           march_part_w2)

names(sub_w2) <- c('demo',
                   'support',
                   'city',
                   'year',
                   'gender',
                   'age',
                   'edu',
                   'region',
                   'dialect',
                   'income',
                   'plur',
                   'formal',
                   # 'fee',
                   'march'
                   )


sub_w2$formal <- ifelse(sub_w2$formal == 7 |
                          sub_w2$formal == 8 |
                          sub_w2$formal == 9, 
                        NA_real_, sub_w2$formal)
# sub_w2$fee <- ifelse(sub_w2$fee == 7 |
#                           sub_w2$fee == 8 |
#                           sub_w2$fee == 9, 
#                         NA_real_, sub_w2$fee)
sub_w2$march <- ifelse(sub_w2$march == 7 |
                          sub_w2$march == 8 |
                          sub_w2$march == 9, 
                        NA_real_, sub_w2$march)



# w3
demo_w3 <- w3$q098
support_w3 <- w3$q130
city_w3 <- w3$level3
year_w3 <- rep(2011,length(demo_w3))
gender_w3 <- w3$se002
# cohord_w1 <- w1$se003
age_w3 <- w3$se003a
edu_w3 <- w3$se005a # year
region_w3 <- w3$se006
dialect_w3 <- w3$se014
income_w3 <- w3$se009 
plur_w3 <- w3$q135  #pluralism
formal_part_w3 <- w3$q075 
# fee_part_w3 <- w3$qII86
march_part_w3 <- w3$qII88




sub_w3 <- cbind.data.frame(demo_w3,
                           support_w3,
                           city_w3,
                           year_w3,
                           gender_w3,
                           age_w3,
                           edu_w3,
                           region_w3,
                           dialect_w3,
                           income_w3,
                           plur_w3,
                           formal_part_w3,
                           # fee_part_w3,
                           march_part_w3
                           )

names(sub_w3) <- c('demo',
                   'support',
                   'city',
                   'year',
                   'gender',
                   'age',
                   'edu',
                   'region',
                   'dialect',
                   'income',
                   'plur',
                   'formal',
                   # 'fee',
                   'march'
                   )

sub_w3$formal <- ifelse(sub_w3$formal == 7 |
                          sub_w3$formal == 8 |
                          sub_w3$formal == 9, 
                        NA_real_, sub_w3$formal)
# sub_w3$fee <- ifelse(sub_w3$fee == 7 |
#                        sub_w3$fee == 8 |
#                        sub_w3$fee == 9, 
#                      NA_real_, sub_w3$fee)
sub_w3$march <- ifelse(sub_w3$march == 7 |
                         sub_w3$march == 8 |
                         sub_w3$march == 9, 
                       NA_real_, sub_w3$march)

# w4
demo_w4 <- w4$q92 # diff
support_w4 <- w4$q85 # diff
city_w4 <- w4$level
year_w4 <- rep(2016,length(demo_w4))
gender_w4 <- w4$se2
# cohord_w1 <- w1$se003
age_w4 <- w4$se3_2
edu_w4 <- w4$se5a # year
region_w4 <- w4$se6
dialect_w4 <- w4$se11
income_w4 <- w4$se14 
plur_w4 <- w4$q144  #pluralism

formal_part_w4 <- w4$q69
# fee_part_w3 <- w3$qII86
march_part_w4 <- w4$q76



# demo_w3 <- w3$q098
# support_w3 <- w3$q130
# city_w3 <- w3$level3
# year_w3 <- rep(2011,length(demo_w3))
# gender_w3 <- w3$se002
# # cohord_w1 <- w1$se003
# age_w3 <- w3$se003a
# edu_w3 <- w3$se005a # year
# region_w3 <- w3$se006
# dialect_w3 <- w3$se014
# income_w3 <- w3$se009 
# plur_w3 <- w3$q135  #pluralism

sub_w4 <- cbind.data.frame(demo_w4,
                           support_w4,
                           city_w4,
                           year_w4,
                           gender_w4,
                           age_w4,
                           edu_w4,
                           region_w4,
                           dialect_w4,
                           income_w4,
                           plur_w4,
                           formal_part_w4,
                           # fee_part_w3,
                           march_part_w4)

names(sub_w4) <- c('demo',
                   'support',
                   'city',
                   'year',
                   'gender',
                   'age',
                   'edu',
                   'region',
                   'dialect',
                   'income',
                   'plur',
                   'formal',
                   # 'fee',
                   'march')

sub_w4$formal <- ifelse(sub_w4$formal == 7 |
                          sub_w4$formal == 8 |
                          sub_w4$formal == 9 |
                          sub_w4$formal == -1, 
                        NA_real_, sub_w4$formal)

sub_w4$march <- ifelse(sub_w4$march == 7 |
                         sub_w4$march == 8 |
                         sub_w4$march == 9 |
                         sub_w4$march == -1, 
                       NA_real_, sub_w4$march)


dat <- bind_rows(sub_w1, sub_w2, sub_w3, sub_w4)


# clean ----
attach(dat)
dat$demo <- ifelse(dat$demo == 9 | dat$demo == 8,
                   NA_real_, dat$demo)

dat$support <- ifelse(dat$support == 9 |
                        dat$support == 8 |
                        dat$support == 7, 
                   NA_real_, dat$support)

dat$city <- ifelse(dat$city == -1, 
                   NA_real_, dat$city)

dat$city <- ifelse(dat$city == 3, 
                   NA_real_, dat$city)

dat$age <- ifelse(dat$age == -1 |
                   dat$age == 99, 
                   NA_real_, dat$age)

dat$edu <- ifelse(dat$edu == -1 |
                    dat$edu == 99, 
                  NA_real_, dat$edu)

dat$year <- as.integer(dat$year)


dat <- dat %>%  
  mutate(religion = case_when(
    region == 10 & year == 2011 ~ 8,
    region == 20 ~ 9,
    region == 40 ~ 7,
    region == 60 ~ 6,
    region == 61 ~ 3,
    region == 70 ~ 3,
    region == 76 ~ 2,
    region == 80 ~ 10,
    region == 90 ~ 10,
    region == 98 ~ NA_real_,
  ))  # 99 regect answer


dat$dialect <- ifelse(dat$dialect == 9 |
                        dat$dialect == 8 |  
                        dat$dialect == 99,
                      NA_real_, dat$dialect)

dat$income <- ifelse(dat$income == 97 |
                        dat$income == 98 |  
                        dat$income == 99,
                      NA_real_, dat$income)


dat$plur <- ifelse(dat$plur == 8 |  
                       dat$plur == 9,
                     NA_real_, dat$plur)


dat <- dat %>% mutate(citizen = case_when(
  demo < 3 & support < 3 ~ 1,
  demo > 2 ~ 0,
  support > 2 ~ 0
))

dat2 <- dat

dat2$citizen <- as.factor(dat2$citizen)
# dat2$city   <- as.factor(dat2$city)
dat2$year   <- as.factor(dat2$year)
# dat2$gender <- as.factor(dat2$gender)
# dat2$religion <- as.factor(dat2$religion)
# dat2$dialect <- as.factor(dat2$dialect)
dat2$income <- as.factor(dat2$income)
dat2$plur <- as.factor(dat2$plur)
dat2$formal <- as.factor(dat2$formal)
dat2$march <- as.factor(dat2$march)


dat_clean <- subset(dat2, select = -c(dialect, religion, region))

# dat_clean <- na.exclude(dat_clean)

length(dat$demo)



# descirbe ----
stargazer(dat_clean)

# regression----

# M0 <- glmer(citizen ~ 1 + (1|gender) + (1|city) + age + (1|income) +
#               edu + (1|religion) + (1|dialect) + (1|year) + plur,
#             family = binomial(link = 'logit'), data = dat2)

M0 <- glmer(citizen ~ 1 + city + gender + edu + income + 
              (1|year) + (1|age) + (1|year:age) + (1|plur),
            family = binomial(link = 'logit'), data = dat_clean,
            na.action=na.omit)

M1 <- glmer(citizen ~ 1 + city + gender + edu + income + age +
              (1|year)  + (1|plur) + (1|year:age),
            family = binomial(link = 'logit'), data = dat_clean,
            na.action=na.omit)

M2 <- glmer(citizen ~ 1 + city + gender + edu + income + age + plur +
              (1|year)  + (1|year:age),
            family = binaomial(link = 'logit'), data = dat_clean,
            na.action=na.omit)

M3 <- glmer(citizen ~ 1 + city + gender + edu + income + age + plur + 
              march + formal + (1|year)  + (1|year:age),
            family = binomial(link = 'logit'), data = dat_clean,
            na.action=na.omit)

M3 <- glmer(citizen ~ 1 + city + gender + edu + income + age + plur + 
              march + formal + 
              (1|year)  + (1|year:age),
            family = binomial(link = 'logit'), data = dat_clean,
            na.action=na.omit)


models <- list()
models[['Model1']] <- M0
models[['Model2']] <- M2
models[['Model3']] <- M3
# models[['Model4']] <- M4
msummary(models)

msummary(models, output = 'latex')

icc(M2)
VarCorr(M0) #f


sjPlot::plot_model(M0,
                   show.values=TRUE, 
                   show.p=TRUE) + theme_cowplot()



sjPlot::plot_model(M2,
                   show.values=TRUE, 
                   show.p=TRUE) + theme_cowplot()

sjPlot::plot_model(M3,
                   show.values=TRUE, 
                   show.p=TRUE) + theme_cowplot()





effects_urchin <- effects::effect(term= "edu", mod= M3)
summary(effects_urchin)
x_urch <- as.data.frame(effects_urchin)
urchin_plot <- ggplot() + 
  #2
  geom_point(data=dat_clean, aes(edu, citizen),position = position_jitter()) + 
  #3
  geom_point(data=x_urch, aes(x=edu,  y= fit), color="blue" ) +
  #4
  geom_line(data=x_urch, aes(x=edu, y = fit), color="blue") +
  # 5
  geom_ribbon(data= x_urch, aes(x=edu, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  #6
  labs(x="edu", y="critical citizen") +
    
  theme_cowplot()

urchin_plot
