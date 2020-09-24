# 09/24/2020

# Model Basics with modelr

library(tidyverse)

library(modelr)
options(na.action = na.warn)

head(sim1)

sim1 %>%
  ggplot(aes(x,y)) +
  geom_point()

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

?runif

ggplot(sim1, aes(x,y)) +
  geom_abline(
    aes(intercept = a1, slope = a2),
    data = models, alpha = 1/4
  ) +
  geom_point()

model1 <- function(a, data) {
  a[1] + data$x * a[2]
}

model1(c(7,1.5), sim1)

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}

measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1,a2) {
  measure_distance(c(a1, a2), sim1)
}


models <- models %>%
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

models

ggplot(sim1, aes(x,y)) + 
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(models, rank(dist) <= 10)
  )

ggplot(models, aes(a1, a2)) + 
  geom_point(
    data = filter(models, rank(dist) <= 10),
    size = 4, color = "red"
  ) +
  geom_point(aes(color = -dist))

grid <-  expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1,3, length = 25)
) %>%
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>%
  ggplot(aes(a1, a2)) +
  geom_point(
    data = filter(grid, rank(dist) <= 10),
    size = 4, color = "red"
  ) +
  geom_point(aes(color = -dist))

ggplot(sim1, aes(x,y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(grid, rank(dist) <= 10)
  )

best <- optim(c(0,0), measure_distance, data = sim1)
head(best)
best$par

sim1 %>%
  ggplot(aes(x,y)) + 
  geom_point(size = 2, color = "grey30") +
  geom_abline(intercept = best$par[1], slope = best$par[2])

sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

grid <- sim1 %>%
  data_grid(x)

grid

grid <-  grid %>%
  add_predictions(sim1_mod)
grid

ggplot(sim1, aes(x)) +
  geom_point(aes(y=y)) + 
  geom_line(
    aes(y = pred),
    data = grid,
    color = "red",
    size = 1
  )

sim1 <- sim1 %>%
  add_residuals(sim1_mod)
sim1

ggplot(sim1, aes(resid)) +
  geom_freqpoly(binwidth = 0.5)

ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h=0) +
  geom_point()

df <- tribble(
  ~y,  ~x1,  ~x2,
  4,  2,  5,
  5,  1,  6
)
df

model_matrix(df, y ~ x1)

model_matrix(df, y ~ x1 -1)

model_matrix(df, y ~ x1 + x2)

df <- tribble(
  ~ sex, ~ response,
  "male", 1,
  "female", 2,
  "male", 1
)

model_matrix(df, response ~ sex)

sim2

sim2 %>%
  ggplot() +
  geom_point(aes(x,y))


mod2 <- lm(y ~ x, data = sim2)

grid <-  sim2 %>%
  data_grid(x) %>%
  add_predictions(mod2)

grid

ggplot(sim2, aes(x)) +
  geom_point(aes(y = y)) +
  geom_point(
    data = grid,
    aes(y = pred),
    color = "red",
    size = 4
  )

tibble(x = "e") %>%
  add_predictions(mod2)

sim3

ggplot(sim3, aes(x1, y)) +
  geom_point(aes(color = x2))

mod1 <-  lm(y ~ x1 + x2, data = sim3)
mod2 <-  lm(y ~ x1 * x2, data = sim3)

grid <- sim3 %>%
  data_grid(x1, x2) %>%
  gather_predictions(mod1, mod2)

grid

ggplot(sim3, aes(x1, y, color = x2)) +
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(~ model)

sim3 <- sim3 %>%
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, color = x2)) +
  geom_point() +
  facet_grid(model ~ x2)

mod1 <-  lm(y ~ x1 + x2, data = sim4)
mod2 <-  lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>%
  data_grid(
    x1 = seq_range(x1,5),
    x2 = seq_range(x2, 5)
  ) %>%
  gather_predictions(mod1, mod2)

grid

library(splines)

df <- tribble(  
  ~x, ~y,  
  1, 2.2,  
  2, NA,  
  3, 3.5,  
  4, 8.3,  
  NA, 10)


df

mod <- lm(y ~ x, data = df)

mod <- lm(y ~ x, data =df, na.action = na.exclude)

nobs(mod)























