# 09/21/2020

# 14 - Pipes with magrittr

library(magrittr)
library(tidyverse)
install.packages("pryr")
library(pryr)

diamonds <- ggplot2::diamonds
diamonds2 <- diamonds %>%
  dplyr::mutate(price_per_carat = price / carat)

pryr::object_size(diamonds)

object_size(diamonds2)
object_size(diamonds,diamonds2)

diamonds$carat[1] <- NA
object_size(diamonds)
object_size(diamonds2)
object_size(diamonds,diamonds2)

rnorm(100) %>%
  matrix(ncol = 2) %>%
  plot() %>%
  str()

rnorm(100) %>%
  matrix(ncol = 2) %T>%
  plot() %>%
  str()

mtcars %$%
  cor(disp,mpg)










