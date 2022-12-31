# ------------------------------------------------------------------------------
# name: max and graph.R
# author: Yu Xia
# description: try to solve a single variable optimize problem in R, and plot graph
# last updated: Sep 30, 2022
# ------------------------------------------------------------------------------

options(scipen = 99999999)
f <- function (a, w) (2/3)*(1-exp(-0.01*(0.35*a+1.02)*w))+(1/3)*(1-exp(-0.01*((-0.01*a+1.02)*w)))
amax <- optimise(f, c(-9999999,9999999), tol = 0.000000001, w=1, maximum = TRUE)
amax

library(ggplot2)
library(ggthemes)

ggplot() +
  xlim(-99999,99999)+
  ylim(-999,99)+
  geom_function(fun = function(x) (2/3)*(1-exp(-0.01*(0.35*x+1.02)))+(1/3)*(1-exp(-0.01*((-0.01*x+1.02)))))+
  theme_minimal() +
  scale_fill_economist()


library("SciViews")
x<-ln(70)/(0.01*0.36)
x

y <- 2500*ln(70)/9
y
