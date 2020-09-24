# 09/23/2020

# tidyr

library(tidyr)
head(pisa)

scores <- pisa[,1:7]
scores

scores_long <- gather(data = scores,
                      key = "subject",
                      value = "score",
                      math, reading, science)

head(scores_long)

nrow(scores)
nrow(scores_long)

times <- pisa[, c(1, 8:10)]

# Preview the times data
head(times)

times_long <- gather(data = times,
                     key = "subject",
                     value = "time",
                     -id)  

# Preview the long data
head(time_long)
  
times_long$subject <- ifelse(time_long$subject == "minutes_math", "math",
                             ifelse(times_long$subject =="minutes_reading", "reading",
                                    "science"))

head(times_long)  

pisa_long <- merge(score_long,
                   time_long,
                   by = c("id", "subject"))  
head(pisa_long)  

head(nlsy)

# Reshape the data to wide
nlsy_wide <- reshape(data = nlsy,
                     v.names = c("weight", "health"),
                     timevar = "year",
                     idvar = c("id", "sex", "ethnicity"),
                     direction = "wide"
  
)
# Preview the data
head(nlsy_wide)  


# Preview number of rows
nrow(nlsy)
nrow(nlsy_wide)

# Using reshape2

head(nlsy)

# Part 1
weight <- nlsy[, 1:5]
head(weight)

# Reshape the data to wide
weight_wide <- dcast(data = weight,
                     formula = id + sex + ethnicity ~ year,
                     value.var = "weight")

# Preview Wide Data
head(weight_wide)

# Rename the year columns
colnames(weight_wide)[4:6] <- paste0("weight.", colnames(weight_wide)[4:6])

# Preview the final data
head(weight_wide)

# PART II
health <- nlsy[, c(1:4, 6)]

# Preview the health data
head(health)

# Reshape Health data
health_wide <- dcast(data = health,
                     formula = id + sex + ethnicity ~ year,
                     value.var = "health")

# Preview the wide data
head(health_wide)

# Rename Columns
colnames(health_wide)[4:6] <- paste0("weight.", colnames(health_wide)[4:6])
head(health_wide)

# Part III Merge
nlsy_wide <- merge(weight_wide,
                   health_wide,
                   by = c("id", "sex", "ethnicity"))

# Sort the final data by id and preview it
nlsy_wide <- nlsy_wide[order(nlsy_wide$id),]
head(nlsy_wide)

# tidyr

head(nlsy)

weight <- nlsy[, 1:5]
head(weight)

weight_wide <- spread(data = weight,
                key = year,
                value = weight)

head(weight_wide)

colnames(weight_wide)[4:6] <- paste0("weight.", colnames(weight_wide)[4:6])
head(weight_wide)

# PART II
health <- nlsy[,c(1:4, 6)]

head(health)

health_wide <- spread(data = health,
                      key = year,
                      value = health)

head(health_wide)

colnames(health_wide)[4:6] <- paste0("health.", colnames(health_wide)[4:6])
head(health_wide)

# PART III
nlsy_wide <- merge(weight_wide,
                   health_wide,
                   by = c("id", "sex", "ethnicity"))
head(nlsy_wide)
nrow(nlsy)
nrow(nlsy_wide)














































































