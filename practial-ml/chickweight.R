# 09/29/2020

# 2 - IDE

library(tidyverse)

ggplot(data = ChickWeight) + 
  geom_smooth(mapping = aes(x = Time, y = weight, color = Diet, SE = FALSE))


names <- c('Mike', 'Renee', 'Richard', 'Mathew', 'Christopher')
scores <- c(85, 92, 95, 97, 96)

names[1]
names[2]
scores[3]

mean(scores)
median(scores)
min(scores)
max(scores)
sum(scores)

mixed <-  c('Mike', 85, 'Renee', 92, 'Richard', 95, 'Mathew', 97, 'Christopher')
mixed

testResults <- data.frame(names,scores)
testResults

mean(testResults$scores)

x <- TRUE
y <- 1
z <- 'Mike Chapple'


class(x)
class(y)
class(z)

productCategories <- c('fruit', 'vegetable', 'fruit', 'fruit', 'dry goods', 'dry goods', 'vegetable')
class(productCategories)

productCategories <- factor(productCategories)

class(productCategories)

productCategories

length(x)
length(productCategories)

is.numeric(x)
x
is.logical(x)

yint <- 1L

class(yint)

is.numeric(yint)
is.integer(yint)

as.numeric("1.5")
as.integer("1.5")



















