##############################################
#### Quantitative Analysis (II) (70700263)
#### HW1
#### Wu Wenquan 
#### 2020312259
##############################################


####load####
library(foreign)
library(stargazer)
library(tidyverse)
library(cowplot)
data <- read.dta('./kidiq.dta')
attach(data)

# p8
reg_p8 <- lm(kid_score ~ mom_hs)
summary(reg_p8)
# stargazer(reg_p8, type = 'text')

# p10
plot_p10 <- ggplot(data, aes(as.character(data$mom_hs), kid_score)) +
  geom_hline(aes(yintercept=50), color = 'grey90' ) +
  geom_hline(aes(yintercept=100), color = 'grey90' ) +
  geom_point(position = position_jitter(0.05), shape = 1, size = 2) +
  theme_cowplot() +
  ylab('Child test scores') +
  xlab('Mother completed high school') +
  xlim(c(-0.2, 1.2)) +
  scale_x_discrete(labels = c('1' = "Yes",'0' = "No"))
plot_p10

# p14
reg_p14 <- lm(kid_score ~ mom_iq + mom_hs )
summary(reg_p14)
# stargazer(reg_p14, type = 'text')

# p16
plot_p16 <- ggplot(data, aes(mom_iq, kid_score, 
                             color = as.character(data$mom_hs),
                             shape = as.character(data$mom_hs))) +
  geom_hline(aes(yintercept = 50), color = 'grey90' ) +
  geom_hline(aes(yintercept = 100), color = 'grey90' ) +
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  theme_cowplot() +
  scale_color_manual(values = c("black" ,"red")) +
  scale_shape_manual(values = c(1, 19)) +
  ylab('Child test scores') +
  xlab('Mother IQ scores') +
  theme(legend.position = 'none')
plot_p16

# p25
normalize <- function(x){
  norm_x <- (x - mean(x)) / sd(x)
  return(norm_x)
}

s_mom_iq <- sd(mom_iq)
data$stdmom_iq <- normalize(mom_iq)
s_mom_hs  <-  sd(mom_hs)
data$stdmom_hs <- normalize(mom_hs)
s_kid_score <- sd(kid_score)
data$stdkid_score <- normalize(kid_score)

reg_p25 <- lm(stdkid_score ~ stdmom_iq + stdmom_hs + stdmom_iq:stdmom_hs )
summary(reg_p25)
# stargazer(reg_p25, type = 'text')

# p27
data$std2mom_iq <- cmom_iq / (2*s_mom_iq)
data$std2mom_hs <- cmom_hs/(2*s_mom_hs)
data$std2momiqhs <- std2mom_iq*std2mom_hs

reg_p27 <- lm(kid_score ~ std2mom_iq + std2mom_hs + std2momiqhs)
summary(reg_p27)

# p33
reg_p33 <- lm(kid_score ~ log(mom_iq) + mom_hs)
summary(reg_p33)
# stargazer(reg_p33, type = 'text')

# p37
reg_p37 <- lm(kid_score ~ as.character(mom_work))
summary(reg_p37)
# stargazer(reg_p37, type = 'text')
