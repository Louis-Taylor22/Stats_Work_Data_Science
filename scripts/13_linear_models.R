library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
#Loading necessary packages!
#---\-_-/---
#Creating a linear model 
lsmodel0 <- lm(formula = height ~ 1, data = darwin)
