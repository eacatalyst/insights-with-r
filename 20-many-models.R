# 09/25/2020

# Many Models with purrr and broom

library(modelr)
library(tidyverse)

install.packages("gapminder")
library(gapminder)

gapminder
nrow(gapminder)

gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

nz <- filter(gapminder, country == "New Zealand")
nz

nz %>%
  ggplot(aes(year, lifeExp)) +
  geom_line() +
  ggtitle("Full data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)
summary(nz_mod)

nz %>%
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) +
  geom_line() +
  ggtitle("Linear trend + ")

nz_pred <- add_predictions(nz, nz_mod)
nz_pred_resid <- add_residuals(nz_pred, nz_mod)

nz
nz %>%
  add_residuals(nz_mod) %>%
  ggplot(aes(year, resid)) +
  geom_hline(yintercept = 0, color = "white", size = 3) +
  geom_line() +
  ggtitle("Remaining pattern")

nz_pred_resid

nrow(gapminder)
by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest()
nrow(by_country)
gapminder
by_country

by_country$data[[1]]

country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

models <- map(by_country$data, country_model)

by_country <- by_country %>%
  mutate(model = map(data, country_model))

by_country

by_country %>%
  filter(continent == "Europe")

by_country %>%
  arrange(continent, country)

by_country <- by_country %>%
  mutate(
    resids = map2(data, model, add_residuals)
  )

by_country <- by_country %>%  mutate(    resids = map2(data, model, add_residuals)  )
by_country

resids <- unnest(by_country,resids)

nz_mod
broom::glance(nz_mod)

by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance)


glance <- by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = TRUE)

glance

glance %>%
  arrange(r.squared)

glance %>%
  ggplot(aes(continent, r.squared)) +
  geom_jitter(width = 0.5)

bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>%
  semi_join(bad_fit, by = "country") %>%
  ggplot(aes(year, lifeExp, color = country)) +
  geom_line()

data.frame(x = list(1:3, 3:5))
data.frame(
  x = I(list(1:3, 3:5)),
  y = c("1, 2", "3, 4, 5")
)

gapminder %>%
  group_by(country, continent) %>%
  nest()

?arrange
gapminder %>%
  arrange(desc(gdpPercap))

mtcars

mtcars %>%
  group_by(cyl) %>%
  summarise(q = quantile(mpg))

probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)
mtcars %>%
  group_by(cyl) %>%
  summarize(p = list(probs), q = list(quantile(mpg, probs))) %>%
  unnest()

x <- list(
  a = 1:5,
  b = 3:4,
  c = 5:6
)

df <- enframe(x)
df

df <- tribble(
  ~x,
  letters[1:5],
  1:3,
  runif(5)
)

df

df %>% mutate(
  type = map_chr(x, typeof),
  lenght = map_int(x, length)
)

df <- tribble(
  ~x, 
  list(a = 1, b = 2),
  list(a = 2, c = 4)
)

df %>% mutate (
  a = map_dbl(x, "a"),
  b = map_dbl(x, "b", .null = NA_real_)
)

tibble(x = 1:2, y = list(1:4, 1)) %>% unnest(y)



















