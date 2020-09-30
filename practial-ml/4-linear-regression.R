# 09/30/2020

# 4 - Linear Regression

library(tidyverse)
library(dplyr)

bikes <- read_csv("bikes.csv")
head(bikes)

bikes %>%
  ggplot(aes(humidity, rentals)) +
  geom_point(color = "blue")

bikes %>%
  ggplot(aes(windspeed, rentals)) +
  geom_point(color = "red")

bikes %>%
  ggplot(aes(temperature, rentals)) +
  geom_point(color = "green")

cov(bikes$humidity, bikes$rentals)

sd(bikes$humidity)
sd(bikes$rentals)


cor(bikes$humidity, bikes$rentals)

cor(bikes$windspeed, bikes$rentals)

cor(bikes$temperature, bikes$rentals)

cor(bikes$humidity, bikes$windspeed)

library(corrplot)

head(bikes)

bikenumeric <- bikes %>%
  select(-date)

bike_correlations <- cor(bikenumeric)

bike_correlations

corrplot(bike_correlations)

corrplot(bike_correlations, type = "upper")

corrplot.mixed(bike_correlations)

B1 <- cov(bikes$temperature, bikes$rentals) / var(bikes$temperature)
B1
B0 <- mean(bikes$rentals) - B1 * mean(bikes$temperature)
B0

bikes_mod1 <-  lm(data = bikes, rentals ~ temperature)
bikes_mod1

bikes_mod1 %>%
  summary()

library(stats)
bikes_mod2 <- lm(data = bikes, rentals ~ humidity + windspeed + temperature)
bikes_mod2

summary(bikes_mod2)

mean(bikes_mod2$residuals)

library(olsrr)

ols_plot_resid_hist(bikes_mod2)

ols_plot_resid_fit(bikes_mod2)

library(car)
durbinWatsonTest(bikes_mod2)

library(olsrr)

ols_plot_cooksd_chart(bikes_mod2)

ols_plot_cooksd_chart(bikes_mod2)$outliers
cooks_outliers <- ols_plot_cooksd_chart(bikes_mod2, print_plot = F)$outliers
cooks_outliers
help("ols_plot_cooksd_chart")


arrange(cooks_outliers, desc(cooks_distance))

bikes[69, c("rentals", "humidity", "windspeed", "temperature")]
summary(bikes[69, c("rentals", "humidity", "windspeed", "temperature")])

outlier_index <- as.numeric(unlist(cooks_outliers[,"observation"]))
outlier_index

summary(bikes[outlier_index, c("rentals", "humidity", "windspeed", "temperature")])

summary(bikes[-outlier_index, c("rentals", "humidity", "windspeed", "temperature")])

summary(bikes[, c("rentals", "humidity", "windspeed", "temperature")])

bikes2 <- bikes[-outlier_index,]
rows_removed = nrow(bikes) - nrow(bikes2)
rows_removed

bikes_mod1
ols_vif_tol(bikes_mod2)

bikes2 <- bikes2 %>%
  mutate(humidity2 = humidity^2) %>%
  mutate(windspeed2 = windspeed^2) %>%
  mutate(temperature2 = temperature^2)
head(bikes2)

bikes_mod3 <- 
  lm(data = bikes2,
     rentals ~ humidity + windspeed + temperature +
       humidity2 + windspeed2 + temperature2
     )
bikes_mod3

summary(bikes_mod3)

# windspeed2 is not significant - being removed
bikes_mod3 <- 
  lm(data = bikes2,
     rentals ~ humidity + windspeed + temperature +
       humidity2 + temperature2
  )

summary(bikes_mod3)

summary(bikes2[,c("season","holiday","weekday","weather")])

head(bikes2)

bikes2 <- bikes2 %>%  
  mutate(season=revalue(as.factor(season), c("1"="Winter", "2"="Spring", "3"="Summer", "4"="Fall"))) %>%  
  mutate(holiday=revalue(as.factor(holiday), c("0"="No", "1"="Yes"))) %>%  
  mutate(weekday=revalue(as.factor(weekday), c("0"="Sunday", "1"="Monday", "2"="Tuesday", "3"="Wednesday", "4"="Thursday", "5"="Friday", "6"="Saturday"))) %>%  
  mutate(weather=revalue(as.factor(weather), c("1"="Clear", "2"="Light precipitation", "3"="Heavy precipitation")))

bikes_mod4 <- 
  lm(data = bikes2,
     rentals ~ humidity + windspeed + temperature + humidity2 +
     temperature2 + season)
summary(bikes_mod4)

bikes_mod5 <- 
  lm(
    data = bikes2,
    rentals ~ humidity + temperature + humidity2 +
      temperature2 + season + windspeed * weather
  )

summary(bikes_mod5)

library(lubridate)

bikes2 <- bikes2 %>%
  mutate(day = as.numeric(date - min(date))) %>%
  mutate(month = as.factor(month(date))) %>%
  mutate(year = as.factor(year(date))) %>%
  select (-date)

head(bikes2)

ols_step_both_p(
  model = lm(
    data = bikes2,
    rentals ~ humidity + weekday + holiday +
      temperature + humidity2 + temperature2 + season +
      windspeed * weather + realfeel + day + month + year
  ),
  pent = 0.2,
  prem = 0.01,
  details = TRUE
)






































