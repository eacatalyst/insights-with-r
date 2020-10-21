# 10/20/2020

# CH 12 Grouping Data with Clustering

library(tidyverse)
college <- read_csv("Student/Data/college.csv", col_types = "nccfffffnnnnnnnnn")
glimpse(college)

maryland_college <- college %>%
  filter(state == "MD") %>%
  column_to_rownames(var = "name")

head(maryland_college)
glimpse(maryland_college)

maryland_college %>%
  select(admission_rate, sat_avg) %>%
  summary()

maryland_college_scaled <- maryland_college %>%
  select(admission_rate, sat_avg) %>%
  scale()

maryland_college_scaled %>%
  summary()

library(stats)
set.seed(1234)

k_3 <- kmeans(maryland_college_scaled, centers = 3, nstart = 25)
k_3
k_3$size


k_3$centers

library(factoextra)
fviz_cluster(k_3, data=maryland_college_scaled, repel = TRUE)

class(k_3)
k_3$cluster

maryland_college %>%
  mutate(cluster = k_3$cluster) %>%
  select(cluster,
         undergrads,
         tuition,
         faculty_salary_avg,
         loan_default_rate,
         median_debt
         ) %>%
  group_by(cluster) %>%
  summarise_all("mean")

fviz_nbclust(maryland_college_scaled, kmeans, method = "wss")

fviz_nbclust(maryland_college_scaled, kmeans, method = "silhouette")

fviz_nbclust(maryland_college_scaled, kmeans, method = "gap_stat")

k_4 <- kmeans(maryland_college_scaled, centers = 4, nstart = 25)
fviz_cluster(
  k_4,
  data = maryland_college_scaled,
  main = "Maryland Colleges Segmented by SAT Score and Admission Rates",
  repel = TRUE
)













