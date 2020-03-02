# data pulled from package here: https://ramikrispin.github.io/coronavirus/
# devtools::install_github("RamiKrispin/coronavirus") #note: use github version with daily data updates

# Provides a tidy format dataset of the 2019 Novel Coronavirus COVID-19 (2019-nCoV) epidemic. 
# The raw data pulled from the Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE) 
# Coronavirus repository.

library(tidyverse)
library(janitor)
library(plotly)
library(DT)
library(coronavirus)

#load the data
data("coronavirus")

head(coronavirus)

#clean names
coronavirus <- coronavirus %>% 
  clean_names()


#initial sample charts...

#total cases by type
total_cases <- coronavirus %>% 
  group_by(type) %>%
  summarise(cases = sum(cases)) %>%
  mutate(type = factor(type, levels = c("confirmed", "recovered", "death")))

total_cases

plot_ly(data = total_cases, 
        x = ~ type, 
        y = ~cases, 
        type = 'bar',
        text = ~ paste(type, cases, sep = ": "),
        hoverinfo = 'text') %>%
  layout(title = "Coronavirus - Cases Distribution",
         yaxis = list(title = "Number of Cases"),
         xaxis = list(title = "Case Type"),
         hovermode = "compare")


#top countries
confirmed_country <- coronavirus %>% 
  filter(type == "confirmed") %>%
  group_by(country_region) %>%
  summarise(total_cases = sum(cases)) %>%
  mutate(perc = total_cases / sum(total_cases)) %>%
  arrange(desc(total_cases))

confirmed_country %>%
  head(10) %>%
  datatable(rownames = FALSE,
            colnames = c("Country", "Cases", "Perc of Total")) %>%
  formatPercentage("perc", 2)



#recovery and death rates
coronavirus %>% 
  filter(country_region != "Others") %>%
  group_by(country_region, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(desc(confirmed)) %>%
  filter(confirmed >= 25) %>%
  mutate(recover_rate = recovered / confirmed,
         death_rate = death / confirmed)  %>%
  datatable(rownames = FALSE,
            colnames = c("Country", "Confirmed", "Recovered", "Death", "Recovery Rate", "Death Rate")) %>%
  formatPercentage("recover_rate", 2) %>%
  formatPercentage("death_rate", 2) 


