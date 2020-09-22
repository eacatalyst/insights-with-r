# 09/22/2020

# 17 - Iterations with purrr

# install.packages("tidyverse")
library(tidyverse)

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

mean(df$a)
mean(df$b)
mean(df$c)
mean(df$d)

output <- vector("double", ncol(df))
output

for (i in seq_along(df)) {
  output[[i]] <- median(df[[i]])
}

output

y <- vector("double", 0)
seq_along(y)
1:length(y)

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}

df

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

output <- vector("double", length(df))
for (i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}

output

col_mean <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- mean(df[[i]])
  }
  output
}

col_median <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- median(df[[i]])
  }
  output
}

col_sd <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- sd(df[[i]])
  }
  output
}

col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}

col_summary(df, sd)

col_summary(df, median)

map_dbl(df,mean)
map_dbl(df,median)
map_dbl(df,sd)

df %>% map_dbl(mean)
df %>% map_dbl(median)
df %>% map_dbl(sd)


map_dbl(df, mean, trim=0.5)

models <- mtcars %>%
  split(.$cyl) %>%
  map(function(df) lm(mpg ~ wt, data = df))

models

models <- mtcars %>%
  split(.$cyl) %>%
  map(~lm(mpg ~ wt, data = .))

models %>%
  map(summary) %>%
  map_dbl(~.$r.squared)

models %>%
  map(summary) %>%
  map_dbl("r.squared")

x <- list(list(1,2,3), list(4,5,6), list(7,8,9))

x %>% map_dbl(2)

x1 <- list(  c(0.27, 0.37, 0.57, 0.91, 0.20),  
             c(0.90, 0.94, 0.66, 0.63, 0.06),  
             c(0.21, 0.18, 0.69, 0.38, 0.77)
)

x2 <- list(  c(0.50, 0.72, 0.99, 0.38, 0.78),  
             c(0.93, 0.21, 0.65, 0.13, 0.27),  
             c(0.39, 0.01, 0.38, 0.87, 0.34)
)

x1
x2

threshold <- function(x, cutoff = 0.8) x[x > cutoff]
x1 %>% sapply(threshold) %>% str()

x2 %>% sapply(threshold) %>% str()

safe_log <- safely(log)
str(safe_log(10))

str(safe_log(a))

x <- list(1,10, "a")
y <- x %>% map(safely(log))
str(y)

y <- y %>% transpose()
str(y)

is_ok <- y$error %>% map_lgl(is_null)
x[!is_ok]

y$result[is_ok] %>% flatten_dbl()

x <- list(1,10,"a")
x %>% map_dbl(possibly(log, NA_real_))

x <- list(1, -1)
x %>% map(quietly(log)) %>% str()

mu <- list(5,10,-3)
mu
mu %>%
  map(rnorm, n = 5) %>%
  str()

sigma <- list(1,5,10)
seq_along(mu) %>%
  map(~rnorm(5, mu[[.]], sigma[[.]])) %>%
  str()

map2(mu, sigma, rnorm, n = 5) %>% str()

map2 <- function(x, y, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_long(x)) {
    out[[i]] <- f(x[[i]], y[[i]], ...)
  }
  out
}

n <- list(1,3,5)
args1 <- list(n, mu, sigma)
args1 %>%
  pmap(rnorm) %>%
  str()

args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>%
  pmap(rnorm) %>%
  str()

params <- tribble(  ~mean, ~sd, ~n,    
                    5,  1,  1,
                    10, 5,  3, 
                    -3, 10,  5
)

params %>%
  pmap(rnorm)

f <- c("runif", "rnorm", "rpois")
f
param <- list(
  list(min = -1, max = 1),
  list(sd = 5),
  list(lambda = 10)
)

invoke_map(f, param, n = 5) %>% str()

sim <- tribble(
  ~f,      ~params,
  "runif", list(min= -1, max =1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)

sim %>%
  mutate(sim = invoke_map(f, params, n=10))

x <- list(1, "a", 3)

x
x %>%
  walk(print)

library(ggplot2)
plots <- mtcars %>%
  split(.$cyl) %>%
  map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")

pwalk(list(paths, plots), ggsave, path=tempdir())

iris %>%
  keep(is.factor) %>%
  str()

iris %>%
  discard(is.factor) %>%
  str()

x <- list(1:5, letters, list(10))

x %>%
  some(is_character)

x %>%
  every(is_vector)

x <- sample(10)
x
x %>%
  detect(~ . > 5)
x %>%
  detect_index(~ . < 5)

x %>% 
  head_while(~ . > 5)

x %>%
  tail_while(~ . > 5)

dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Marry"), sex = c("M", "F")),
  trt = tibble(name = "Marry", treatment = "A")
)

dfs %>% reduce(full_join)

vs <- list(  c(1, 3, 5, 6, 10),  
             c(1, 2, 3, 7, 8, 10),  
             c(1, 2, 3, 4, 8, 9, 10)
)

vs %>% reduce(intersect)

x <- sample(10)
x
x %>% accumulate(`+`)






















