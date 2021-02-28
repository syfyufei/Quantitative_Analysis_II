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
library(lm.beta)
data <- read.dta('./kidiq.dta')
attach(data)

# p8
reg_p8 <- lm(kid_score ~ mom_hs)
stargazer(reg_p8, type = 'text')

# p10
plot_p10 <- ggplot(data, aes(as.character(data$mom_hs), kid_score)) +
  geom_hline(aes(yintercept=50), color = 'grey90' ) +
  geom_hline(aes(yintercept=100), color = 'grey90' ) +
  # geom_line(aes(data$mom_hs, kid_score)) +
  stat_smooth(data, aes(as.numeric(data$mom_hs), kid_score, group = NULL)),
method = 'glm', se = FALSE,) +
  geom_line(position="identity") +
  geom_point(position = position_jitter(0.05), shape = 1, size = 2) +
  theme_cowplot() +
  ylab('Child test scores') +
  xlab('Mother completed high school') +
  xlim(c(-0.2, 1.2)) +
  scale_x_discrete(labels = c('1' = "Yes",'0' = "No"))+
  geom_smooth(method = lm, se = FALSE)
plot_p10


ggplot(data, aes(as.character(data$mom_hs), kid_score)) +
  geom_line()
()
# p11
reg_p11 <- lm(kid_score ~ mom_iq, data = data)
stargazer(reg_p11, type = 'text')

# p13
plot_p13 <- ggplot(data, aes(x = mom_iq,y = kid_score)) + 
  theme_cowplot() +
  geom_hline(aes(yintercept=50), color = 'grey90' ) +
  geom_hline(aes(yintercept=100), color = 'grey90' ) +
  geom_point(shape=19, color = '#660874') +
  xlab("Mother IQ score") + 
  ylab("Child test scores") + 
  geom_smooth(method = lm, se = FALSE, color = 'red')
plot_p13

# p14
reg_p14 <- lm(kid_score ~ mom_iq + mom_hs )
stargazer(reg_p14, type = 'text')

# p16
reg_p14
plot_p16 <- ggplot(data, aes(mom_iq, kid_score, 
                             color = as.character(data$mom_hs),
                             shape = as.character(data$mom_hs))) +
  geom_hline(aes(yintercept = 50), color = 'grey90' ) +
  geom_hline(aes(yintercept = 100), color = 'grey90' ) +
  geom_point() + 
  geom_smooth(aes(mom_iq, fitted(reg_p14))) +
  theme_cowplot() +
  scale_color_manual(values = c("black" ,"red")) +
  scale_shape_manual(values = c(1, 19)) +
  ylab('Child test scores') +
  xlab('Mother IQ scores') +
  theme(legend.position = 'none')
plot_p16

# p20
reg_p20 <- lm(kid_score ~ mom_iq + mom_hs + mom_iq*mom_hs, data = data)
stargazer(reg_p20, type = 'text')

# p22
plot_p22 <- ggplot(data, aes(mom_iq, kid_score, 
                             color = as.character(mom_hs),
                             shape = as.character(mom_hs))) +
  geom_hline(aes(yintercept = 50), color = 'grey90' ) +
  geom_hline(aes(yintercept = 100), color = 'grey90' ) +
  geom_point() + 
  theme_cowplot() +
  scale_color_manual(values = c("black" ,"red")) +
  scale_shape_manual(values = c(1, 19)) +
  ylab('Child test scores') +
  xlab('Mother IQ scores') +
  theme(legend.position = 'none') +
  geom_smooth(aes(mom_iq, kid_score), method = lm, formula = y ~ x, se = FALSE)
plot_p22

# p25, but I think there may have some wrong in page 25, because it calculate  sd(x) but didn't use sd(x) in next step. 
normalize <- function(x){
  norm_x <- (x - mean(x))
  return(norm_x)
}


data$stdmom_iq <- normalize(mom_iq)
data$stdmom_hs <- normalize(mom_hs)

reg_p25 <- lm(kid_score ~ stdmom_iq + stdmom_hs + I(stdmom_iq*stdmom_hs), data = data)
stargazer(reg_p25, type = 'text')

# p27
normalize2 <- function(x){
  temp <- (x - mean(x))
  norm_x <- temp /( 2 * sd(x) )
  return(norm_x)
}
data$std2mom_iq <- normalize2(mom_iq)
data$std2mom_hs <- normalize2(mom_hs)


reg_p27 <- lm(kid_score ~ std2mom_iq + std2mom_hs + I(std2mom_iq*std2mom_hs), data = data)
stargazer(reg_p27, type = 'text')

# p30
reg_p30 <- lm(kid_score ~ mom_iq + mom_hs, data = data)
stargazer(reg_p30, type = 'text')
reg_p30_beta <- lm.beta(reg_p30)
reg_p30_beta

# p32
reg_p32 <- lm(log(kid_score) ~ mom_iq + mom_hs, data = data)
stargazer(reg_p32, type = 'text')

# p33
reg_p33 <- lm(kid_score ~ log(mom_iq) + mom_hs)
stargazer(reg_p33, type = 'text')

# p39
reg_p39 <- lm(kid_score ~ as.character(mom_work))
stargazer(reg_p39, type = 'text')
