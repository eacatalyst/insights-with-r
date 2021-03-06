# 10/17/2020

# CH-10 Improving Performance

library(caret)
library(rpart)
modelLookup("rpart")

library(tidyverse)

income <- read_csv("Student/Data/income.csv", col_types = "nffnfffffnff")
glimpse(income)

set.seed(1234)

sample_set <- createDataPartition(y = income$income, p = .75, list = FALSE)
head(sample_set)
income_train <- income[sample_set, ]
income_test <-  income[-sample_set, ]
head(income_train)
head(income_test)

# Balance the dataset

set.seed(1234)
library(DMwR)
income_train <- 
  SMOTE(income ~ .,
        data.frame(income_train),
        perc.over = 100,
        perc.under = 200
        )

# Build and Train a Model

set.seed(1234)
income_mod <- train (
  income ~ .,
  data = income_train,
  metric = "Accuracy",
  method = "rpart",
  trControl = trainControl(method = "boot632", number = 3)
)

income_mod

# Evaluate The Model

income_pred <- predict(income_mod, income_test)
confusionMatrix(income_pred, income_test$income, positive = "<=50K")

# Customized Parameter Tuning
# tuneLength argument

set.seed(1234)
income_mod <- train(
  income ~ .,
  data = income_train,
  metric = "Accuracy",
  method = "rpart",
  trControl = trainControl(method = "boot632", number = 3),
  tuneLength = 20
)

income_mod

expand.grid(
  .alpha = c(1,2,3),
  .beta = c(TRUE, FALSE),
  .gamma = seq(from = 4, to = 5, by = 0.5)
)

expand.grid(.cp = seq(from = 0.0001, to = 0.002, by = 0.0001))

# Use tuneGrid argument of the train() function

set.seed(1234)
income_mod <- train (
  income ~ . ,
  data = income_train,
  metric = "Accuracy",
  method = "rpart",
  trControl = trainControl(method = "boot632", number = 3),
  tuneGrid = expand.grid(.cp = seq(from = 0.0001, to = 0.002, by = 0.0001))
)

income_mod

income_pred <- predict(income_mod, income_test)
confusionMatrix(income_pred, income_test$income, positive = "<=50K")

# Ensemble Methods

library(randomForest)
modelLookup("rf")

set.seed(1234)
rf_mod <- train(
  income ~ . ,
  data = income_train,
  metric = "Accuracy",
  method = "rf",
  trControl = trainControl(method = "none"),
  tuneGrid = expand.grid(.mtry = 3)
)

rf_pred <- predict(rf_mod, income_test)
confusionMatrix(rf_pred, income_test$income, positive = "<=50K")

# Boosting

library(xgboost)
modelLookup("xgbTree")


set.seed(1234)
xgb_mod <- train(
  income ~ . , 
  data = income_train,
  metric = "Accuracy",
  method = "xgbTree",
  trControl = trainControl(method = "none"),
  tuneGrid = expand.grid(
    nrounds = 100,
    max_depth = 6,
    eta = 0.3,
    gamma = 0.01,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
)

xgb_pred <- predict(xgb_mod, income_test)
confusionMatrix(xgb_pred, income_test$income, positive = "<=50K")

# Stacking

library(tidyverse)
library(DMwR)

income <- income %>%
  mutate(income = as.factor(recode(income, "<=50K" = "Below", ">50K" = "Above")))

income %>%
  select(income) %>%
  table()

library(caret)
set.seed(1234)

sample_set <- createDataPartition(y = income$income, p = 0.75, list = FALSE)

income_train <- income[sample_set, ]
income_test <- income[-sample_set, ]

# Balance the data

set.seed(1234)
income_train <- 
  SMOTE(
    income ~ . ,
    data.frame(income_train),
    perc.over = 100,
    perc.under = 200
  )

library(caretEnsemble)

ensembleLearners <- c("rpart", "glm", "knn")

library(rpart)
library(stats)
library(class)

models <- caretList(
  income ~ . ,
  data = income_train,
  metric = "Accuracy",
  methodList = ensembleLearners,
  trControl = trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    savePredictions = "final",
    classProbs = TRUE
  )
)

results <-  resamples(models)
summary(results)

modelCor(results)

library(randomForest)

stack_mod <- caretStack(
  models,
  method = "rf",
  metric = "Accuracy",
  trControl = trainControl(
    method = "repeatedcv",
    number = 10,
    savePredictions = "final",
    classProbs = TRUE
  )
)

# Stopping point ----
stack_pred <- predict(stack_mod, income_test)
confusionMatrix(stack_pred, income_test$income, positive = "Below")





















































