# data pulled from package here: https://ramikrispin.github.io/coronavirus/
# devtools::install_github("RamiKrispin/coronavirus") #note: use github version with daily data updates

# Provides a tidy format dataset of the 2019 Novel Coronavirus COVID-19 (2019-nCoV) epidemic. 
# The raw data pulled from the Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE) 
# Coronavirus repository.

library(tidyverse)
library(janitor)
library(coronavirus)

#load the data
data("coronavirus")

head(coronavirus)

#clean names
coronavirus <- coronavirus %>% 
  clean_names()


#initial sample queries...

#total cases by region and type
summary_df <- coronavirus %>% 
  group_by(country_region, type) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(desc(total_cases))

summary_df 

# Summary of new cases during the past 24 hours by country and type (as of 2020-03-01)
coronavirus %>% 
  filter(date == max(date))

coronavirus %>% 
  filter(date == max(date)) %>%
  select(country = country_region, type, cases) %>%
  group_by(country, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = total_cases) %>%
  arrange(desc(confirmed))




