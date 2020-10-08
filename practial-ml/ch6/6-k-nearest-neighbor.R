# 10/07/2020

# CH 6 - k-Nearest Neighbors

library(tidyverse)

# Import heat data set
heart <- read_csv("ch6/data/heart.csv", col_types = "nffnnffnfnfnff")
glimpse(heart)

summary(heart)

# remove missing data

heart <- heart %>%
  filter(
      !is.na(restingBP) &
      !is.na(cholesterol) &
      !is.na(highBloodSugar) &
      !is.na(restingECG) &
      !is.na(restingHR) &
      !is.na(exerciseAngina) &
      !is.na(STdepression) &
      !is.na(STslope) &
      !is.na(coloredVessels) &
      !is.na(defectType)
  )

summary(heart)

# Normalize the data to reduce the Euclidean distance

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

heart <- heart %>%
  mutate(age = normalize(age)) %>%
  mutate(restingBP = normalize(restingBP)) %>%
  mutate(cholesterol = normalize(cholesterol)) %>%
  mutate(restingHR = normalize(restingHR)) %>%
  mutate(STdepression = normalize(STdepression)) %>%
  mutate(coloredVessels = normalize(coloredVessels))


# Dealing with Categorical Features
summary(heart)

heart <- data.frame(heart)

colnames(heart)

heart_labels <-  heart %>% select(heartDisease)
heart_labels

heart <-  heart %>% select(-heartDisease)
colnames(heart)

# Convert Categorical variables to dummy variables

library(dummies)

heart <- dummy.data.frame(data = heart, sep="_")

colnames(heart)

# Splitting the Data

set.seed(1234)
sample_index <- sample(nrow(heart), round(nrow(heart) * .75), replace = FALSE)
sample_index
heart_train <- heart[sample_index,]
heart_test <- heart[-sample_index,]

# Splitting the class labels

heart_train_labels <- as.factor(heart_labels[sample_index, ])
heart_test_labels <- as.factor(heart_labels[-sample_index, ])

# Classifying Unlabeled Data

library(class)

heart_train_labels

heart_pred1 <- 
  knn(
    train = heart_train,
    test = heart_test,
    cl = heart_train_labels,
    k = 15
  )

head(heart_pred1)


# Evaluating the Model

heart_pred1_table <- table(heart_test_labels,heart_pred1)
heart_pred1_table

sum(diag(heart_pred1_table)) / nrow(heart_test)

heart_pred2 <- 
  knn(
    train = heart_train,
    test = heart_test,
    cl = heart_train_labels,
    k =1
  )

heart_pred2_table <- table(heart_test_labels, heart_pred2)

sum(diag(heart_pred2_table)) / nrow(heart_test)


heart_pred3 <- 
  knn(
    train = heart_train,
    test = heart_test,
    cl = heart_train_labels,
    k = 40
  )

heart_pred3_table <- table(heart_test_labels, heart_pred3)
sum(diag(heart_pred3_table)) / nrow(heart_test)















