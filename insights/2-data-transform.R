# 09/17/2020

# 2- Workflow

sin(pi/2)

library(nycflights13,tidyverse)

flights
nrow(flights)

filter(flights, year == 2013)
filter(flights, month == 1)

flights.jan1 <- filter(flights, month ==1 , day == 1)
flights.jan1

near(sqrt(2) ^ 2,2)
1/49 * 49 == 1

filter(flights, month == 1 | month == 12)

nov_dec <- filter(flights, month %in% c(11,12))

filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 20)

df <-  tibble(x=c(1,NA,3))
df

filter(df, x>1)
filter(df, is.na(x) | x >= 1)

?flights

filtered.flights <- filter(
  flights,
       arr_delay >= 120 & dest %in% c("IAH","HOU") & month %in% c(7,8,9) 
  )

filtered.flights

arrange(flights, year, month, day)

arrange(flights, desc(arr_delay))

df <- tibble(x=c(5,2,NA))
arrange(df,x)

is.na(flights)

select(flights, year, month, day)

select(flights, year:day)

select(flights, -(year:day))

rename(flights,tail_num=tailnum)

select(flights, time_hour, air_time, everything())

select(flights, time_hour, time_hour)

flights

select(flights, contains("Time"))

flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time
)

mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / (air_time * 60),
       hours = air_time / 60,
       gain_per_hour = gain / hours
)

View(flights_sml)

transmute(flights,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours)

transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100)

x <- 1:10
x
lag(x)
lead(x)

cumsum(x)
cummean(x)

summarize(flights, delay = mean(dep_delay, na.rm=T))

by_day <- group_by(flights, year, month, day)
by_day
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

by_dest <- group_by(flights,dest)
delay <- summarize(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = T),
                   delay = mean(arr_delay, na.rm = T)
                   )

delay <- filter(delay, count > 20, dest != "HNL")

ggplot(data = delay, mapping = aes(x=dist, y=delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

delays <- flights %>%
  group_by(dest) %>%
  summarize(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count > 20, dest != "HNL")

flights %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay, na.rm=TRUE))

not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(mean=mean(dep_delay))


delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 10)

ggplot(data = delays, mapping = aes(x=n, y=delay)) +
  geom_point(alpha = 1/10)

delays %>%
  filter(n > 25) %>%
  ggplot(mapping = aes(x=n,y=delay)) +
  geom_point(alpha = 1/10)

?Lahman::Batting

batting <- as_tibble(Lahman::Batting)

batters <- batting %>%
  group_by(playerID) %>%
  summarize(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

?arrange

batters %>%
  filter(ab > 100) %>%
  ggplot(mapping = aes(x=ab, y=ba)) +
  geom_point() +
  geom_smooth(se = FALSE)

batters %>%
  arrange(desc(ab)) 

not_cancelled %>%
  group_by(year,month,day) %>%
  summarize (
    # average delay:
    avg_delay1 = mean(arr_delay),
    # average positive delay:
    avg_delay2 = mean(arr_delay[arr_delay >0])
  )

not_cancelled %>%
  group_by(dest) %>%
  summarize(distance_sd = sd(distance)) %>%
  arrange(desc(distance_sd))

not_cancelled %>%
  group_by(year,month, day) %>%
  summarize(
    first = min(dep_time),
    last = max(dep_time)
  )

not_cancelled %>%
  group_by(year,month,day) %>%
  summarize(
    first_dep = first(dep_time),
    last_dep = last(dep_time)
  )

not_cancelled %>%
  group_by(year, month, day) %>%
  mutate(r = min_rank(desc(dep_time))) %>%
  filter(r %in% range(r))

not_cancelled %>%
  group_by(dest) %>%
  summarize(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))

not_cancelled %>%
  count(dest)

not_cancelled %>%
  count(tailnum, wt = distance)


not_cancelled %>%
  group_by(year,month,day) %>%
  summarize(n_early = sum(dep_time < 500))

not_cancelled %>%
  group_by(year,month, day) %>%
  summarize(hour_perc = mean(arr_delay > 60))

daily <- group_by(flights, year, month, day)
(per_day <- summarize(daily, flights = n()))

(per_month <- summarise(per_day, flights = sum(flights)))

(per_year <- summarize(per_month,flights = sum(flights)))

month.flights <-  (per_month <- summarise(per_day, flights = sum(flights)))
month.flights

ggplot(data = month.flights) +
  geom_bar(mapping = aes(x=month,y=flights), stat="identity", posiiton = "dodge")

?geom_bar

daily %>%
  ungroup() %>%
  summarize(flights = n())

flights_sml %>%
  group_by(year,month,day) %>%
  filter(rank(desc(arr_delay)) <10)

popular_dests <- flights %>%
  group_by(dest) %>%
  filter(n() > 365)

popular_dests


popular_dests %>%
  filter(arr_delay > 0) %>%
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>%
  select(year:day, dest, arr_delay, prop_delay)















