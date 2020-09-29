# 09/18/2020

# 7 - tibble

library(tidyverse)

as_tibble(iris)

tibble(
  x = 1:5,
  y = 1,
  z = x ^ 2 + y
)

tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)

nycflights13::flights %>%
  print(n = 10, width = Inf)

nycflights13::flights %>%
  View()


df <- tibble(
  x = runif(5),
  y = rnorm(5)
)

df %>%
  View()

df
df$x

df[["x"]]

df[[1]]

df %>% .$x

df %>% .[["x"]]










