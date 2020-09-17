# R for Data Science
# 1 - Data Viz
install.packages("tidyverse")
library(tidyverse)
tidyverse_update()

install.packages("jsonlite")
install.packages(c("dplyr", "jsonlite", "tidyr"))

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

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, alpha = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, shape = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy), color="blue")

?mpg

ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  facet_grid(~ class,nrow=2)

ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  facet_grid(. ~ fl)

ggplot(data=mpg) +
  geom_point(mapping = aes(x=drv,y=cyl)) +
  facet_grid(~ class, nrow=2)

?facet_wrap

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  geom_smooth(mapping = aes(x=displ, y=hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x=displ, y=hwy,linetype = drv)) +
  geom_point(mapping = aes(x=displ, y=hwy, color=drv))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x=displ, y=hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x=displ, y=hwy,group = drv))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x=displ, y=hwy,color = drv),
              show.legend = F)

ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ,y=hwy)) +
  geom_smooth(mapping = aes(x=displ, y=hwy))

ggplot(data = mpg, mapping = aes(x=displ, y=hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(
    data = filter(mpg, class == "subcompact"), se = FALSE
  )

ggplot (
  data = mpg,
  mapping = aes(x=displ, y=hwy, color=drv)
) +
  geom_point() +
  geom_smooth(se = FALSE)

?geom_smooth

ggplot(data = mpg, mapping = aes(x=displ,y=hwy)) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(data = mpg, mapping = aes(x=displ, y=hwy)) +
  geom_smooth(data = mpg, mapping = aes(x=displ, y=hwy))

ggplot(data = mpg, mapping = aes(x=displ, y = hwy))+
  geom_point() +
  geom_smooth(se=F)

ggplot(data = mpg, mapping = aes(x=displ, y = hwy))+
  geom_point() +
  geom_smooth(mapping = aes(group = drv),se=F)

ggplot(data = mpg, mapping = aes(x=displ, y = hwy))+
  geom_point(mapping = aes(color = drv)) +
  geom_smooth(mapping = aes(group = drv, color = drv),se=F)


ggplot(data = mpg, mapping = aes(x=displ, y = hwy))+
  geom_point(mapping = aes(color = drv)) +
  geom_smooth(se=F)

ggplot(data = mpg, mapping = aes(x=displ, y = hwy))+
  geom_point(mapping = aes(color = drv)) 
  
head(mpg)

?diamonds

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x=cut))

ggplot(data = diamonds) +
  stat_count(mapping = aes(x=cut))


demo <- tribble(~a,      ~b,
                "bar_1", 20,
                "bar_2", 30,
                "bar_3", 40)
demo

ggplot(data = demo) +
  geom_bar(mapping = aes(x = a, y = b), stat = "identity")


ggplot(data = diamonds) +
  geom_bar(mapping = aes(x=cut, y=..prop.., group =1))

ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x=cut, y=depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x=cut, color = cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x=cut, fill=cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x=cut, fill = clarity))


ggplot(
  data = diamonds,
  mapping = aes(x=cut, fill = clarity)
) +
  geom_bar(alpha = 1/5, position = "identity")

ggplot(
  data = diamonds,
  mapping = aes(x = cut, color = clarity)
) +
  geom_bar(fill = NA, position = "identity")

ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x=cut, fill = clarity), position = "fill"
  )

ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x=cut, fill=clarity), position = "dodge"
  )

ggplot(data = mpg) +
  geom_point(
    mapping = aes(x = displ, y = hwy),
    position = "jitter"
  )

ggplot(data = mpg, mapping = aes(x=class, y=hwy)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x=class, y=hwy)) + 
  geom_boxplot() +
  coord_flip()

nz <- map_data("nz")
nz

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color="black") +
  coord_quickmap()

bar <- ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x=cut, fill=cut),show.legend = FALSE, width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x=NULL, y=NULL)

bar

bar + coord_flip()
bar + coord_polar()






































