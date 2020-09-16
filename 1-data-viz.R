# R for Data Science
# 1 - Data Viz
install.packages("tidyverse")
library(tidyverse)
tidyverse_update()

install.packages("jsonlite")

install.packages(c("nycflights13", "gapminder", "Lahman"))

dput(mtcars)

mpg

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color = class))

ggplot(data = mpg)

nrow(mpg)
ncol(mpg)

?mpg

ggplot(data = mpg) +
  geom_point(mapping = aes(x=hwy, y = cyl))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=class, y = drv))












