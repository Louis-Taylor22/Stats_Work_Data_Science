library(tidyverse)
library(here)
library(kableExtra)
#Load in necessary packages 
darwin <- read_csv(here("data", "darwin.csv"))

#--- Checking the data ---
# check the structure of the data
glimpse(darwin)

# check data is in a tidy format
head(darwin)

# check variable names
colnames(darwin)


# clean up column names

darwin <- janitor::clean_names(darwin)

# check for duplication
darwin %>% 
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
darwin %>% 
  summarise(min=min(height, na.rm=TRUE), 
            max=max(height, na.rm=TRUE))

# check for typos by looking at distinct characters/values

darwin %>% 
  distinct(pair)

darwin %>% 
  distinct(type)

# missing values
darwin %>% 
  is.na() %>% 
  sum()

# quick summary

summary(darwin)

#---Basic visulaisation ---

darwin %>% 
  ggplot(aes(x=type,
             y=height))+
  geom_point()
ggsave("figures/Basic_Darwin_Data_Visualisation.png", dpi=300)

#---Comparing Groups---

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))
# the code above calculates the standard deviation and the mean for darwin data set - seperately for self and cross
#--- Converting SD and Mean into figure ---
# make a new object
darwin_summary <-darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

# make a summary plot
darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()
ggsave("figures/Darwin_MeanSD_Figure.png", dpi=300)

# Creates a graph where by central dot represents mean and the shows standard deviation 

#---Creating a new table using Kable---

# use kable extra functions to make a nice table (could be replaced with kable() if needed)
darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

#---Estimating the difference of mean height in self and crossed plants---
# pivot data to wide format then subtract Selfed plant heights from Crossed plant heights

darwin_wide <- darwin %>% 
  pivot_wider(names_from = type, values_from = height) %>% 
  mutate(difference = Cross - Self)
darwin_wide

difference_summary <- darwin_wide %>% 
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())

difference_summary

#Just calculated the mean difference between the two means 
#Now calculating the standard error
difference_summary %>% 
  mutate(se= sd/sqrt(n))
#the average difference in height was 2.62 ± 1.22 inches (mean ± SE).


#--- Calculating error and the significance of the mean difference ---
#Null hypothesis - there is no difference in the mean height of self vs crossed plants
#Alternate hypothesis - inbreeding reduces the fitness of the selfed plants, observed as selfed plants on average being smaller than crossed plants


# Learning about normal distributions
#Create a sequence of 100 equally spaced numbers between -4 and 4
x <- seq(-4, 4, length=100)

#create a vector of values that shows the height of the probability distribution
#for each value in x
y <- dnorm(x)

#plot x and y as a scatterplot with connected lines (type = "l") and add
#an x-axis with custom labels
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))

lowerCI <- 2.62-(2*1.22)

upperCI <- 2.62+(2*1.22)

lowerCI
upperCI

#Above is the calculated confidence intervals for the data 

