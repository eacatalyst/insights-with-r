# 09/21/2020

# 15 - Functions
library(tidyverse)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x -rng[1] / (rng[2] - rng[1]))
}

rescale01(8)

rescale01(c(0,4,10))

# Compute confidence Interval around
# mean using normal approximation

mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 1, 1 - alpha /2))
}

x <- runif(100)
mean_ci(x)
mean_ci(x, conf = 0.99)

sample(10) + 100

?runif
runif(10) > .5

1:10 + 1:2


1:10 + 1:3

tibble(x = 1:4, y = 1:2)

tibble(x=1:4, y = rep(1:2, 2))

c(x = 1, y = 2, z = 4)
set_names(1:3, c("a", "b", "c"))

x <-  c("one", "two", "three", "four", "five")
x
x[c(3,2,5)]

x[c(1,1,5,5,5,2)]

x[c(-1, -3, -5)]

x[c(1,-1)]

x[0]

x <- c(10,3,NA,5,8,1,NA)

x[!is.na(x)]

x[x %% 2 == 0]

x <- c(abc=1, def=2, xyz=5)
x[c("xyz", "def")]

x[]
x

x <- list(1,2,3)

x
str(x)

y <- list("a", 1L, 1.5, TRUE)

z <- list(list(1,2), list(3,4))
str(z)

a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
a
str(a)

str(a[1:2])
str(a[4])

str(y[[1]])
str(y[[4]])

a$a
a$d

x <- 1:10
x
attr(x, "greeting")
attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"

as.Date

methods("as.Date")

getS3method("as.Date", "default")

getS3method("as.Date", "numeric")

x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))

typeof(x)
attributes(x)

x <- as.Date("1971-01-01")
unclass(x)
typeof(x)
attributes(x)

x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x)
typeof(x)
attributes(x)

attr(x, "tzone") <- "US/Pacific"
x
attr(x, "tzone") <- "US/Eastern"
x

y <- as.POSIXlt(x)
typeof(y)
attributes(y)

tb <- tibble::tibble(x=1:5, y = 5:1)
typeof(tb)
attributes(tb)

df <- data.frame(x=1:5, y = 5:1)
typeof(df)
attributes(df)





















