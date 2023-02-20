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

#---/-_-/---
#Comparing means 
lsmodel1 <- lm(height ~ type, data=darwin)

# note that the following is identical

# lsmodel1 <- lm(height ~ 1 + type, data=darwin)

#Now that the model formual contains the pollination type
#in addition to an intercept...



darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()



#---/-_-/---
#Confidence intervals 
confint(lsmodel1)

GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95)

#Graphing confidence intervals to investigate Darwin's hypothesis


#Getting R to calculate the mean and standard error of the other treatment level
darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy()


#We can also use the emeans package to do a similiar thing - see book 


#---/-_-/---
#Assumption checking
#Need to check whether the assumptions of the model are adequately met
#So that we know whether our analysis can be trusted 
#First we need to check
# - that the residual/unexplained variance in our date is approx. normally distributed 
# - that the residual/unexplained variance is approximately equal between our groups 

#Residuals are the difference between observed and fitted values 
#produced by the model 
#in this case - 
# The heights of the plants against the treatment means 
#The assumption of equal variance applies because of this pooled variance approach
#(e.g. we have two treatments with 15 replicates - but by pooling the variance across treatments we have a sample size of 30).

#Several functions exist to check assumptions of linear models
#, and the easiest way to do this is to make graphs. We can do this in several ways, 
#in base R with the plot() function, and by using the performance::check_model() function.

performance::check_model(lsmodel1)


#normal distribution 
performance::check_model(lsmodel1, check=c("normality","qq"))

#QQ plot 
# See coursebook for more about these!

#Equal variance
performance::check_model(lsmodel1, check="homogeneity")

plot(lsmodel1, which=c(1,3))


#In order to assess if variances are equal we can plot the residuals (variance) 
#of our data against the fitted (predicted) values. If the residuals were zero, 
#this would mean there is no error, and our data exactly matches our estimates. 
#In reality, there will always be residual error, but as long as it is evenly 
#distributed between treatments this is ok.

#The check_models plot provides what we call 'standardized residuals' where we 
#divide the residual error by the standard deviation.

#In this instance we can see that the higher fitted values (Cross treatment) appears
#to be more variable than the lower fitted values. Again, this is not too bad, 
#but not perfect. This is probably being influence at least partially by the potential outliers.

#---/-_-/---
#Outliers 
performance::check_model(lsmodel1, check="outliers")