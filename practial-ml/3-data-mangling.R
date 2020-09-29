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














