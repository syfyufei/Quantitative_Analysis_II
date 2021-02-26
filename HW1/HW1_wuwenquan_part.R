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
