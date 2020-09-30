# 09/29/2020

# 3 - Data Mangling

library(tidyverse)

vehicles <- read_csv(file = 'vehicles.csv', col_types = "nnnfnfffffnn")
glimpse(vehicles)

head(vehicles)

summary(vehicles)

summary(select(vehicles, class, cylinders))

table(select(vehicles, class))
table(select(vehicles,class))
colnames(vehicles)
table(select(vehicles, make))


head(vehicles$class)
prop.table(table(select(vehicles,class)))

vehicles %>%
  select(class) %>%
  table() %>%
  prop.table()

View(vehicles)

table(vehicles$drive)

vehicles %>%
  filter(drive == "2-Wheel Drive") %>%
  select(co2emissions) %>%
  summary()

vehicles %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = class, y = co2emissions), fill = "red") +
  labs(title = "Boxplot of CO2 Emissions by Vehicle Class", x = "class", y = "C02")

vehicles %>%
  ggplot() +
  geom_point(mapping = aes(citympg, co2emissions), color = "blue", size = 2 ) +
  labs(title = "Scatterplot of C02 Emissions vs. City Miles per Gallon", x = "City MPG", y = "CO2 Emissions")

vehicles %>%
  ggplot() +
  geom_histogram(mapping = aes(x = co2emissions), bins = 30, fill = "yellow", color = "black") +
  labs(title = "Histogram of C02 Emissions", x = "C02 Emissions", y = "Frequency")

vehicles %>%
  ggplot() +
  geom_bar(aes(x = year, fill = drive), color = "black") + 
  labs(title= "Stacked Bar Chart of Drive Type Composition by Year", x = "Model Year", y = "Number of Cars") + 
  coord_flip()

vehicles %>%
  select(citympg, displacement, highwaympg) %>%
  summary()

vehicles <- vehicles %>%
  mutate(citympg = ifelse(is.na(citympg), median(citympg, na.rm = TRUE), citympg)) %>%
  mutate(highwaympg = ifelse(is.na(highwaympg), median(highwaympg, na.rm = TRUE), highwaympg))

vehicles <- vehicles %>%
  mutate(displacement = ifelse(
    is.na(displacement),
    mean(displacement, na.rm = TRUE),
    displacement
  ))

vehicles %>%
  select(citympg, displacement, highwaympg) %>%
  summary()

vehicles %>%
  select(co2emissions) %>%
  summary()

vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_d = co2emissions / (10^4)) %>%
  summary()

vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_z = (co2emissions - mean(co2emissions)) / sd(co2emissions)) %>%
  mutate(co2emissions_sz = scale(co2emissions))

vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_n = 
    ((co2emissions - min(co2emissions))
     / (max(co2emissions) - min(co2emissions))) * (1 - 0) + 0
  ) %>%
  summary()

vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_b = log10(co2emissions)) %>%
  summary()

vehicles %>%
  select(drive) %>%
  summary()

vehicles2 <- vehicles %>%
  mutate(drive2 = recode(drive, "2-Wheel Drive" = "Front-Wheel Drive")) %>%
  mutate(drive2 = recode(drive2, "4-Wheel Drive" = "All-Wheel Drive")) %>%
  select(drive, drive2)

head(vehicles2)

summary(vehicles2)

vehicles2 <- data.frame(vehicles2)

library(dummies)
vehicles2 <- dummy.data.frame(data = vehicles2, names = "drive2", sep="_")
head(vehicles2)

set.seed(1234)
sample(100, 20, replace = TRUE)

set.seed(1234)
sample_set <- sample(nrow(vehicles), nrow(vehicles) * 0.75, replace = FALSE)
vehicle_train <- vehicles[sample_set,]
vehicle_train

vehicle_test <- vehicles[-sample_set,]
vehicle_test

library(caTools)

vehicles %>%
  select(drive) %>%
  table() %>%
  prop.table()

set.seed(1234)

sample_set <- sample(nrow(vehicles), nrow(vehicles) * 0.01, replace = FALSE)
class(sample_set)

vehicles_simple <- vehicles[sample_set,]

vehicles_simple %>%
  select(drive) %>%
  table() %>%
  prop.table()

library(caTools)
sample_set <- sample.split(vehicles$drive, SplitRatio = 0.01)
class(sample_set)

vehicles_stratified <- subset(vehicles, sample_set == TRUE)

vehicles_stratified %>%
  select(drive) %>%
  table() %>%
  prop.table()

# Exercises
# 1
# For all manual transmission vehicles in the vehicles data set - 
#list descriptive statistics for drive, model, and class

vehicles %>%
  select(drive,make,model,class) %>%
  filter(vehicles$transmissiontype == 'Manual') %>%
  summary()

# 2
# Normalize the values of the co2emissions variable in the vehicles dataset
# So that they fall between the values of 1 and 10
# Show the descriptive statistics


vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_n = ((co2emissions - min(co2emissions)) / (max(co2emissions) - min(co2emissions))) 
         * (10 -1) + 1) %>%
  summary()

# 3
# Discretize the co2emissions variable 

vehicle_emissions <- vehicles %>%
  mutate(emission_level = ifelse(co2emissions >= 500, "High", "Low"))

vehicle_emissions %>%
  select(emission_level) %>%
  table() %>%
  prop.table()

sample_set <- sample.split(vehicle_emissions$emission_level, SplitRatio = 0.02)
vehicle_emissions_stratified <-  subset(vehicle_emissions, sample_set == TRUE)

vehicle_emissions_stratified %>%
  select(emission_level) %>%
  table() %>%
  prop.table()



































