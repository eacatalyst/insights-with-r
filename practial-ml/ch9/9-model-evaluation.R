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
  arrange(Resample) %>%
  summarise(AvgAccuracy_lgocv = mean(Accuracy))

# Load R Data Set

load("Student/Data/spam.RData")

spam_matrix <- 
  confusionMatrix(email_pred, email_test$message_label, positive = "spam")
spam_matrix

spam_accuracy <- as.numeric(spam_matrix$overall["Accuracy"])
spam_accuracy

spam_kappa <- as.numeric(spam_matrix$overall["Kappa"])
spam_kappa

spam_sensitivity <- 
  sensitivity(email_pred, email_test$message_label, positive = "spam")
spam_sensitivity

spam_specificity <- 
  specificity(email_pred, email_test$message_label, negative = "ham")
spam_specificity

spam_precision <- 
  posPredValue(email_pred, email_test$message_label, positive="spam")
spam_precision

spam_recall <- spam_sensitivity
spam_recall

load("Student/Data/spam.RData")

library(e1071)
email_pred_prod <- predict(email_mod, email_test, type = "raw")
head(email_pred_prod)

library(ROCR)

roc_pred <- 
  prediction(
    predictions = email_pred_prod[,"spam"],
    labels = email_test$message_label
  )

roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")

plot(roc_perf, main = "ROC Curve", col = "green", lwd = 3)
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)

auc_perf <- performance(roc_pred, measure = "auc")

spam_auc <-  unlist(slot(auc_perf, "y.values"))
spam_auc































