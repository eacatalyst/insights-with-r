# 09/22/2020

# Data Aggregation

library(tidyverse)

# Preview Data
head(nlsy)

# Number of observations by year, sex, and ethnicity
summary_data1 <- aggregate(id ~ year + sex + ethnicity,
                           data = nlsy,
                           FUN = length)

summary_data1

# Average weight by year, sex, and ethnicity
summary_data2 <- aggregate(weight ~ year + sex + ethnicity,
                           data = nlsy,
                           FUN = mean)

summary_data2

# Custom Function to find the mode
get_mode <-  function(var) {
  unique_var <- unique(var)
  unique_var[which.max(tabulate(match(var, unique_var)))]
}

# Apply the custome function to find the mode
summary_data3 <- aggregate(cbind(health, weight) ~ year + sex + ethnicity,
                           data = nlsy,
                           FUN = get_mode)

summary_data3

# dplyr

head(nlsy)

# Define Funciton to find the mode

get_mode <-  function(var) {
  unique_var <- na.omit(unique(var))
  unique_var[which.max(tabulate(match(var, unique_var)))]
}

summary_data <- nlsy %>%
  group_by(year,sex, ethnicity) %>%
  summarize(count = length(id),
            weight_mean = mean(weight, na.rm = TRUE),
            weight_mode = get_mode(weight),
            health_mode = get_mode(health)) %>%
  as.data.frame()

summary_data
































