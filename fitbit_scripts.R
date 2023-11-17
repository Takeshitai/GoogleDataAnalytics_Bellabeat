# Analyze Data from Fitbit
# Load Packages
library(plotly)
library(tidyverse) 
library(dplyr) 
library(lubridate)  #wrangle date attributes
library(skimr) #get summary data
library(ggplot2) #visualize data
library(cowplot) #grid the plot
library(readr) #save csv 
library(plotly) #pie chart

# Import and create dataframe
daily_activity <- read_csv('./data/dailyActivity_merged.csv')
daily_calories <- read_csv('./data/dailyCalories_merged.csv')
daily_intensities <- read_csv('./data/dailyIntensities_merged.csv')
daily_steps <- read_csv('./data/dailySteps_merged.csv')
daily_sleep <- read_csv('./data/sleepDay_merged.csv')
weight_log <- read_csv('./data/weightLogInfo_merged.csv')

# Clean up and look for repetition
# Display dimensions
dim(daily_sleep)
# Missing values
sum(is.na(daily_sleep))
# Sum of duplicated rows
sum(duplicated(daily_sleep))
# Remove duplicate row
daily_sleep <- daily_sleep[!duplicated(daily_sleep), ]

# Repeat on other dataframes
sum(is.na(daily_activity))
sum(is.na(daily_calories))
sum(is.na(daily_sleep))
# The NA is coming from the "Fat" column.
sum(is.na(weight_log))

sum(duplicated(daily_activity))
sum(duplicated(daily_calories))
sum(duplicated(daily_sleep))
sum(duplicated(weight_log))


# Check to see the number of unique users
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

# The output here tells me that data for sleep
# and weight, for example are even less than the
# expected 30, but 24 and 8 respectively

# At this point, thinking about focusing more on activity
# and sleep

# Add a column for weekday for easier analysis
daily_activity <- daily_activity %>% mutate( Weekday = weekdays(as.Date(ActivityDate, "%m/%d/%Y")))

# Merge Data
merge_1 <- merge(daily_activity,daily_sleep,by = c("Id"), all=TRUE)
merged_data <- merge(merge_1, weight_log, by = c("Id"), all=TRUE)

# Order from M-Sun
merged_data$Weekday <- factor(merged_data$Weekday, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

merged_data[order(merged_data$Weekday), ]


# Save merged_data to a new .csv file
write_csv(merged_data, "merged_data.csv")

weight_log %>% 
  filter(IsManualReport == "True") %>% 
  group_by(Id) %>% 
  summarise("Manual Weight Report"=n()) %>%
  distinct()

# Viewing the summary of merged_data to take some notes
summary(merged_data)

# Order M-Sun for Avg Results
# Define the custom order of weekdays
custom_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Apply the custom order to the "Weekday" column
daily_activity$Weekday <- factor(daily_activity$Weekday, levels = custom_order)

# Calculate average calories by day of the week and ID
daily_activity_avgCal <- daily_activity %>%
  group_by(Id, Weekday) %>%
  summarize(AvgCalories = mean(Calories))

# View the result
View(daily_activity_avgCal)

# Create a bar plot with the custom order
library(ggplot2)
ggplot(daily_activity_avgCal, aes(x = Weekday, y = AvgCalories)) +
  geom_bar(stat = "identity", position = "dodge", fill = "lightblue") +
  labs(title = "Average Calories by Day of the Week",
       x = "Weekday",
       y = "Average Calories") +
  theme_minimal()



