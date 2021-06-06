library(tidyverse)
library(haven)
library(arm)

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
                           plur_w1)

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
                   'plur')

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
                           plur_w2)

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
                   'plur')

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
                           plur_w3)

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
                   'plur')

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
                           plur_w4)

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
                   'plur')


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

dat <- na.exclude(dat)
length(dat$demo)

# descirbe ----

# regression----