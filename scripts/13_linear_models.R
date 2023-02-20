library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
#Loading necessary packages!
#---\-_-/---
#Creating a linear model 
lsmodel0 <- lm(formula = height ~ 1, data = darwin)
#Using summary to investigate the model 
summary(lsmodel0)
#Can also use a package called broom to do this provided by tidyverse library
#See below for that!
#---/-_-\---
broom::tidy(lsmodel0)
#summarises information about model components
broom::glance(lsmodel0)
#reports information about the entire model
broom::augment(lsmodel0)
#adds information about individual observations to a data set and it can be used
#to model predictions onto a new dataset