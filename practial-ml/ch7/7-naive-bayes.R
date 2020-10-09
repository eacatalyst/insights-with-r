# 10/09/2020

# CH 7 - Naive Bayes

library(tidyverse)

email <- read_csv("Student/Data/email.csv")
nrow(email)
head(email)

# convert predict feature to factor

email %>%
  select(message_label) %>%
  table() %>%
  prop.table() * 100 %>%
  round(4)

email <- email %>%
  mutate(message_label = as.factor(message_label))

# Convert data set to narrow data set
# Top used words
email %>%
  gather(word, count, -message_index, -message_label) %>%
  group_by(word) %>%
  summarise(occurrence = sum(count)) %>%
  arrange(desc(occurrence)) %>%
  slice(1:10)

# Most occuring words in legit emails
email %>%
  filter(message_label == 'ham') %>%
  gather(word, count, -message_index, -message_label) %>%
  group_by(word) %>%
  summarise(occurrence = sum(count)) %>%
  arrange(desc(occurrence)) %>%
  slice(1:10)

email %>%
  filter(message_label == 'spam') %>%
  gather(word, count, -message_index, -message_label) %>%
  group_by(word) %>%
  summarise(occurrence = sum(count)) %>%
  arrange(desc(occurrence)) %>%
  slice(1:10)

# Splitting the Data

set.seed(1234)
sample_set <- sample(nrow(email), round(nrow(email) * .75), replace = FALSE)
class(sample_set)

email_train <- email[sample_set, ]
email_test <- email[-sample_set, ]

email %>%
  select(message_label) %>%
  table() %>%
  prop.table() %>%
  round(2)

email_test %>%
  select(message_label) %>%
  table() %>%
  prop.table() %>%
  round(2)

email_train %>%
  select(message_label) %>%
  table() %>%
  prop.table() %>%
  round(2)

library(e1071)

# Training the model
email_mod <- 
  naiveBayes(message_label ~ . -message_index, data = email_train, laplace = 1)



# Evaluating the Model

email_pred <- predict(email_mod, email_test, type = "raw")
head(email_pred)

email_pred <- predict(email_mod, email_test, type = "class")
head(email_pred)

email_pred_table <- table(email_test$message_label, email_pred)
email_pred_table

sum(diag(email_pred_table)) / nrow(email_test)
















