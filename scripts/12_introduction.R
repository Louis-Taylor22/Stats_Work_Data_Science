library(tidyverse)
library(here)
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