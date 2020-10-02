# 10/01/2020

# 5 - Logistic Regression

library(tidyverse)
donors <- read_csv("donors.csv", col_types = "nnffnnnnnnnnffffffffff")

nrow(donors)

glimpse(donors)

donors %>%
  keep(is.factor) %>%
  summary()

# Data Understanding
# EDA - Distribution of Income Rating
head(donors)
donors %>%
  select(incomeRating) %>%
  table(exclude = NULL) %>%
  prop.table()

prop.table(table(donors$incomeRating, exclude = NULL))


# Data Preparation

donors <- donors %>%
  mutate(incomeRating = as.character(incomeRating)) %>%
  mutate(incomeRating = as.factor(ifelse(is.na(incomeRating), 'UNK', incomeRating)))

donors %>%
  select(incomeRating) %>%
  table() %>%
  prop.table()

donors <- donors %>% mutate(wealthRating = as.character(wealthRating)) %>% 
  mutate(wealthRating = as.factor(ifelse(is.na(wealthRating), 'UNK', wealthRating))) %>% 
  mutate(urbanicity = as.character(urbanicity)) %>% 
  mutate(urbanicity = as.factor(ifelse(is.na(urbanicity), 'UNK', urbanicity))) %>% 
  mutate(socioEconomicStatus = as.character(socioEconomicStatus)) %>% 
  mutate(socioEconomicStatus = as.factor(ifelse(is.na(socioEconomicStatus), 'UNK', socioEconomicStatus))) %>% 
  mutate(isHomeowner = as.character(isHomeowner)) %>% 
  mutate(isHomeowner = as.factor(ifelse(is.na(isHomeowner), 'UNK', isHomeowner))) %>% 
  mutate(gender = as.character(gender)) %>% 
  mutate(gender = as.factor(ifelse(is.na(gender), 'UNK', gender)))

donors %>%
  keep(is.factor) %>%
  summary()

# Data Preparation of continous features

donors %>%
  keep(is.numeric) %>%
  summary()

# Impute missing age values by gender
donors <- donors %>%
  group_by(gender) %>%
  mutate(age = ifelse(is.na(age), mean(age, na.rm = TRUE), age)) %>%
  ungroup()

donors %>%
  select(age) %>%
  summary()

# Impute number of childern with mode

donors <- donors %>%
  mutate(numberChildren = ifelse(is.na(numberChildren),
                                 median(numberChildren, na.rm = TRUE),
                                 numberChildren))

donors %>%
  select(numberChildren) %>%
  summary()

# Identify Outliers
glimpse(donors)
donors %>%
  select(averageGiftAmount,largestGiftAmount,mailOrderPurchases,numberGifts,smallestGiftAmount,totalGivingAmount) %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot () +
  geom_histogram(mapping = aes(x=value,fill=key), color = "black") +
  facet_wrap(~ key, scales = "free") +
  theme_minimal()

# Remove Outliers

donors <- donors %>% 
  mutate(max1 = quantile(mailOrderPurchases, .75) + (1.5 * IQR(mailOrderPurchases))) %>% 
  mutate(max2 = quantile(totalGivingAmount, .75) + (1.5 * IQR(totalGivingAmount))) %>% 
  mutate(max3 = quantile(numberGifts, .75) + (1.5 * IQR(numberGifts))) %>% 
  mutate(max4 = quantile(smallestGiftAmount, .75) + (1.5 * IQR(smallestGiftAmount))) %>% 
  mutate(max5 = quantile(largestGiftAmount, .75) + (1.5 * IQR(largestGiftAmount))) %>% 
  mutate(max6 = quantile(averageGiftAmount, .75) + (1.5 * IQR(averageGiftAmount))) %>%    
  filter(mailOrderPurchases <= max1) %>%    filter(totalGivingAmount <= max2) %>%    
  filter(numberGifts <= max3) %>%    filter(smallestGiftAmount <= max4) %>%    
  filter(largestGiftAmount <= max5) %>%    filter(averageGiftAmount <= max6) %>%    
  select(-max1,-max2,-max3,-max4,-max5,-max6)

donors %>%
  keep(is.numeric) %>%
  summary()

# Review data after removing outliers

donors %>%
  select(averageGiftAmount,largestGiftAmount,mailOrderPurchases,numberGifts,smallestGiftAmount,totalGivingAmount) %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot () +
  geom_histogram(mapping = aes(x=value,fill=key), color = "black") +
  facet_wrap(~ key, scales = "free") +
  theme_minimal()

# Modeling
# Splitting the data

set.seed(1234)
sample_set <- sample(nrow(donors), round(nrow(donors) * .75), replace = FALSE)
donors_train <- donors[sample_set, ]
donors_test <- donors[-sample_set,]

donors %>%
  select(respondedMailing, exclude = NULL)  %>%
  table() %>%
  prop.table() 
  
donors_train %>%
  select(respondedMailing, exclude = NULL)  %>%
  table() %>%
  prop.table() 

donors_test %>%
  select(respondedMailing, exclude = NULL)  %>%
  table() %>%
  prop.table() 

library(DMwR)
set.seed(1234)
donors_train <- SMOTE(respondedMailing ~ ., data.frame(donors_train), perc.over = 100, perc.under = 200)

# Data Preparation
# Tranform TRUE/FALSE to 1/0

donors <- donors %>%
  mutate(respondedMailing = as.factor(ifelse(respondedMailing == TRUE, 1, 0)))
donors_train <- donors_train %>%
  mutate(respondedMailing = as.factor(ifelse(respondedMailing == TRUE, 1, 0)))
donors_test <- donors_test %>%
  mutate(respondedMailing = as.factor(ifelse(respondedMailing == TRUE, 1, 0)))

summary(donors)

donors %>%
  select(respondedMailing) %>%
  table() 
 


donors_test %>%
  select(respondedMailing) %>%
  table()

donors_train %>%
  select(respondedMailing) %>%
  table()

rs_mod1 <- glm(data=donors_train, family = binomial, formula = respondedMailing ~.)

summary(rs_mod1)








