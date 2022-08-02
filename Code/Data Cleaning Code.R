#Load libraries

library(tidyverse)
library(purrr)
library(lubridate)

#Create vector of file names for trend files
trend_files <- list.files('Data/Google_Trends', 
                          full.names = TRUE)

#Read in trend files to single data frame
trend_data <- trend_files %>% map_df(read_csv)

#Read in scorecard data and name ID data
score_data <- 
  read_csv('Data/Most+Recent+Cohorts+(Scorecard+Elements).csv')

name_data <-
  read_csv('Data/id_name_link.csv')

#Format dates in trend data
trend_data <- trend_data %>%
  mutate(date = ymd(str_sub(monthorweek, 1, 10))) %>% 
  mutate(month = floor_date(date, unit = 'month'))

#Standardize index variable in trend data
trend_data <- trend_data %>% 
  group_by(schname, keyword) %>% 
  mutate(mean_index = mean(index), stdDev_index = sd(index), std_index = 
           (index - mean_index) / stdDev_index)

#Remove duplicate schools from name data
name_data <- name_data %>%
  group_by(schname) %>% 
  mutate(n = n()) %>% 
  filter(n == 1)

#Merge data into single df
df1 <- trend_data %>% 
  inner_join(name_data, by = 'schname') %>% 
  rename(UNITID = unitid)

df1 <- df1 %>% 
  inner_join(score_data, by = 'UNITID')

rm(name_data, score_data, trend_data, trend_files)

