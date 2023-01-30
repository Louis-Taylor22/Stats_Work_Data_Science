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