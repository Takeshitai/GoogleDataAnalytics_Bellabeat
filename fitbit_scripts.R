# Analyze Data from Fitbit
library('dplyr')
library('tidyverse')
library('here')
library('skimr')
library('janitor')

# Import and create dataframe
daily_activity <- read_csv('dailyActivity_merged.csv')
daily_calories <- read_csv('dailyCalories_merged.csv')
daily_intensities <- read_csv('dailyIntensities_merged.csv')
daily_steps <- read_csv('dailySteps_merged.csv')
daily_sleep <- read_csv('sleepDay_merged.csv')
weight_log <- read_csv('weightLogInfo_merged.csv')

# Clean up and look for repetition
# Display dimensions
dim(daily_sleep)
# Missing values
sum(is.na(daily_sleep))
# Sum of duplicated rows
sum(duplicated(daily_sleep))
# Remove duplicate row
daily_sleep <- daily_sleep[!duplicated(daily_sleep), ]