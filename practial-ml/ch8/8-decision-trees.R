# 10/09/2020

# Decision Tress


# Import Data
library(tidyverse)
permits <- read_csv("Student/Data/permits.csv", col_types = "ffffffnnnnfffff")

glimpse(permits)

# Data Understanding

summary(permits)


# Data Preparation

permits <- permits %>%
  mutate(valuation = ifelse(valuation < 1, NA, valuation)) %>%
  mutate(floorArea = ifelse(floorArea < 1, NA, floorArea)) %>%
  mutate(numberUnits = ifelse(numberUnits < 1, NA, numberUnits)) %>%
  mutate(stories = ifelse(stories < 1, NA, stories)) %>%
  mutate(stories = ifelse(stories > 73, NA, stories))

summary(select(permits, valuation, floorArea, numberUnits, stories))

permits %>%
  select(valuation, floorArea, numberUnits, stories) %>%
  summary()

permits <- permits %>%
  select(
    permitType,
    permitSubtype,
    initiatingOffice,
    permitCategory
  )

set.seed(1234)

sample_set <- sample(nrow(permits), round(nrow(permits) * 0.80), replace = FALSE)

permits_train <- permits[sample_set, ]
permits_test <- permits[-sample_set, ]

# Review label distribution

permits %>%
  select(permitCategory) %>%
  table() %>%
  prop.table() %>%
  round(2)


permits_test %>%
  select(permitCategory) %>%
  table() %>%
  prop.table() %>%
  round(2)

permits_train %>%
  select(permitCategory) %>%
  table() %>%
  prop.table() %>%
  round(2)

# Training the Model

library(rpart)
permits_mod <- 
  rpart(
    permitCategory ~ .,
    method = "class", 
    data = permits_train
  )

# Evaluate the Model

library(rpart.plot)
rpart.plot(permits_mod)


permits_pred <- predict(permits_mod, permits_test, type = "class")
permits_pred_table <- table(permits_test$permitCategory, permits_pred)
permits_pred_table

sum(diag(permits_pred_table)) / nrow(permits_test)
































