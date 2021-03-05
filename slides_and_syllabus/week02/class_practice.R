library(tidyverse)
library(foreign)
data <-  read.dta('./slides/week02/wells.dta')
data$dist100 <- data$dist/100
M0 <- glm(switch ~ 1, data = data , family = binomial(link = 'logit'))

M1 <- glm(switch ~ dist100 + arsenic, data = data , family = binomial(link = 'logit'))
attach(data)

M2 <- glm(switch ~ dist100, data = data , family = binomial(link = 'logit'))

invlogit <- function(x){
  1 / (1 + exp(-x))
}

plot(x = dist100, y = switch, type = 'n', xlab = 'dist100', ylab = 'dd', axes = FALSE, 
              frame.plot = TRUE, xlim = c (0,1), yaxs = 'i')                   
points(x = dist100,  y = jitter(switch, factor = 0.2), pch = '.')
# curve(invlogit(coef(M2)[1] + coef(M2)[2] * x), from = min(dist100), to = max(dist100), add = TRUE)
curve(invlogit(coef(M1)[1] + coef(M1)[2] * x + coef(M1)[3] * 1), from = min(dist100), to = max(dist100), add = TRUE)
curve(invlogit(coef(M1)[1] + coef(M1)[2] * x + coef(M1)[3] * 2), from = min(dist100), to = max(dist100), add = TRUE)

#logLik(<M>)