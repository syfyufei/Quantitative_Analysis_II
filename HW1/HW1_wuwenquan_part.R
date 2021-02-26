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

