# 10/19/2020

# CH 11 - Association Rules

library(arules)
supermart <- read.transactions("Student/Data/retail.txt", sep = "")

summary(supermart)

inspect(supermart[1:5])

itemFrequency(supermart[,"39"])

library(tidyverse)
supermart_frequency <- 
  tibble(
    Items = names(itemFrequency(supermart)),
    Frequency = itemFrequency(supermart)
  )

head(supermart_frequency)

supermart_frequency %>%
  arrange(desc(Frequency)) %>%
  slice(1:10)

supermartrules <- 
  apriori(
    supermart,
    parameter = list (
      support = 0.0085,
      confidence = 0.5,
      minlen = 2
    )
  )

summary(supermartrules)

inspect(supermartrules[1:10])

supermartrules %>%
  sort(by = "lift") %>%
  head(n=10) %>%
  inspect()

supermartrules %>%
  subset(items %in% "41") %>%
  inspect()

supermartrules %>%
  subset(items %in% "41") %>%
  sort(by = "lift") %>%
  head(n = 10) %>%
  inspect()



























