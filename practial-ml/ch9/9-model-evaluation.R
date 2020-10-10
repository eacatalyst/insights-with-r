# 10/10/2020

# 9 - Evaluating Performance

library(tidyverse)
income <- read_csv("Student/Data/income.csv", col_types = "nffnfffffnff")
glimpse(income)


# Partition the Data

library(caret)
set.seed(1234)
sample_set <- createDataPartition(y = income$income, p = 0.75, list = FALSE)
class(sample_set)

income_train <- income[sample_set, ]
income_test <- income[-sample_set, ]

head(income_test)
head(income_train)

# Balance Training Data with SMOTE

library(DMwR)
set.seed(1234)
income_train <- 
  SMOTE(
    income ~ .,
    data.frame(income_train),
    perc.over = 100,
    perc.under = 200
  )

# Training the Model
library(rpart)
set.seed(1234)
income_mod <- train(
  income ~ .,
  data = income_train,
  metric = "Accuracy",
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5)
)

# Review Performance Results
income_mod$resample %>%
  arrange(Resample) %>%
  summarise(AvgAccuracy = mean(Accuracy))


# Implement with LOOCV

library(rpart)
set.seed(1234)
income_mod <- train(
  income ~ .,
  data = income_train,
  metric = "Accuracy",
  method = "rpart",
  trControl = trainControl(method = "LOOCV")
)

income_mod$resample %>%
  arrange(Resample) %>%
  summarise(AvgAccuracy_loocv = mean(Accuracy))

# Implement random cross-validation

library(rpart)
set.seed(1234)
imcome_mod <- train(
  income ~ . ,
  data = income_train,
  metric = "Accuracy",
  method = "rpart",
  trControl = trainControl(method = "LGOCV", p= .1, number = 10)
)

income_mod$resample %>%
  arrange(Resample)

library(rpart)
set.seed(1234)

income_mod <- train(
  income ~ . ,
  data = income_train,
  metric = "Accuracy",
  method = "rpart",
  trControl = trainControl(method = "boot632", number = 4)
)

income_mod$resample %>%
  arrange(Resample)


load("Student/Data/spam.RData")

spam_matrix <- 
  confusionMatrix(email_pred,email_test$message_label, positive = "spam")
spam_matrix

spam_accurcy <- as.numeric(spam_matrix$overall["Accuracy"])
spam_accurcy

spam_kappa <- as.numeric(spam_matrix$overall["Kappa"])
spam_kappa

































