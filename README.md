# CASE STUDY: Bellabeat Fitness Data Analysis 
##### Author: Izumi T

##### Date: December, 2023

#
_The case study follows the six step data analysis process:_

###  [Ask](#1-ask)
###  [Prepare](#2-prepare)
###  [Process](#3-process)
###  [Analyze](#4-analyze)
###  [Share](#5-share)
###  [Act](#6-act)

## Introduction
Bellabeat is a successful small company, but they have the potential to become a larger player in the global smart device market. Urška Sršen, cofounder and Chief Creative Officer of Bellabeat, believes that analyzing smart device fitness data could help unlock new growth opportunities for the company. The goal of this task is to focus on one of Bellabeat’s products and analyze smart device data to gain insight into how consumers are using their smart devices. The insights you discover will then help guide marketing strategy for the company. You will present your analysis to the Bellabeat executive team along with your high-level recommendations for Bellabeat’s marketing strategy

## 1. Ask
💡 **BUSINESS TASK: Analyze Fitbit data to gain insight and help guide marketing strategy for Bellabeat to grow as a global player.**

Primary stakeholders: Urška Sršen and Sando Mur, executive team members.

Secondary stakeholders: Bellabeat marketing analytics team.

## 2. Prepare 
Data Source: 30 participants FitBit Fitness Tracker Data from Mobius: https://www.kaggle.com/arashnic/fitbit


- Reliability: The data is from 30 FitBit users who consented to the submission of personal tracker data and generated by from a distributed survey via Amazon Mechanical Turk. 
- Original: The data is from 30 FitBit users who consented to the submission of personal tracker data.
- Comprehensive: Data minute-level output for physical activity, heart rate, and sleep monitoring. While the data tracks many factors in the user activity and sleep, but the sample size is small and most data is recorded during certain days of the week. 
- Current: Data is from March 2016 to May 2016. Data is not current so the users habit may be different now. 
- Cited: Unknown. 

⛔ The dataset has limitations due to its size:

- Upon further investigation with ```n_distinct()``` to check for unique user Id, the set has 33 user data from daily activity, 24 from sleep and only 8 from weight. There are 3 extra users and some users did not record their data for tracking daily activity and sleep. 
- For the 8 user data for weight, 5 users manually entered their weight and 3 recorded via a connected wifi device (eg: wifi scale).
- Most data is recorded from Tuesday to Thursday, which may not be comprehensive enough to form an accurate analysis. 

## 3. Process

Analyze Data from Fitbit to analyze smart device usage 
Gain insight into how people are already using smart devices

Load Packages
```
library(plotly)
library(tidyverse) 
library(dplyr) 
library(lubridate)  #wrangle date attributes
library(skimr) #get summary data
library(ggplot2) #visualize data
library(cowplot) #grid the plot
library(readr) #save csv 
library(plotly) #pie chart
```

Import and create dataframe
```
daily_activity <- read_csv('./data/dailyActivity_merged.csv')
daily_calories <- read_csv('./data/dailyCalories_merged.csv')
daily_intensities <- read_csv('./data/dailyIntensities_merged.csv')
daily_steps <- read_csv('./data/dailySteps_merged.csv')
daily_sleep <- read_csv('./data/sleepDay_merged.csv')
weight_log <- read_csv('./data/weightLogInfo_merged.csv')
```

Clean up and look for repetition
```
Display dimensions
dim(daily_sleep)
Missing values
sum(is.na(daily_sleep))
Sum of duplicated rows
sum(duplicated(daily_sleep))
Remove duplicate row
daily_sleep <- daily_sleep[!duplicated(daily_sleep), ]
```

Repeat on other dataframes
```
sum(is.na(daily_activity))
sum(is.na(daily_calories))
sum(is.na(daily_sleep))
The NA is coming from the "Fat" column.
sum(is.na(weight_log))
```
```
sum(duplicated(daily_activity))
sum(duplicated(daily_calories))
sum(duplicated(daily_sleep))
sum(duplicated(weight_log))
```

Check to see the number of unique users
```
unique_users_activity <- daily_activity %>%
  distinct(Id) %>%
  n_distinct()

unique_users_calories <- daily_calories %>% 
  distinct(Id) %>% 
  n_distinct()

unique_users_intensities <- daily_intensities %>% 
  distinct(Id) %>% 
  n_distinct()

unique_users_steps <- daily_steps %>% 
  distinct(Id) %>% 
  n_distinct()

unique_users_sleep <- daily_sleep %>% 
  distinct(Id) %>% 
  n_distinct()

unique_users_weight <- weight_log %>% 
  distinct(Id) %>% 
  n_distinct()
```

The output here tells me that data for sleep
and weight, for example are even less than the
expected 30, but 24 and 8 respectively

At this point, thinking about focusing more on activity and sleep

Add a column for weekday for easier analysis
```
daily_activity <- daily_activity %>% mutate( Weekday = weekdays(as.Date(ActivityDate, "%m/%d/%Y")))
```

Merge Data
```
merge_1 <- merge(daily_activity,daily_sleep,by = c("Id"), all=TRUE)
merged_data <- merge(merge_1, weight_log, by = c("Id"), all=TRUE)
```
Order from M-Sun
```
merged_data$Weekday <- factor(merged_data$Weekday, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

merged_data[order(merged_data$Weekday), ]
```

Save merged_data to a new .csv file
```
write_csv(merged_data, "merged_data.csv")
```

## 4. Analyze
