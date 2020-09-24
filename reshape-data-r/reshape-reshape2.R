# 09/23/2020

# Reshape with reshape2

# Preview the data
head(pisa)

library("reshape2")

# Part 1: Reshape scores to long format ------
scores <- pisa[, 1:7]

# Preview the score data
head(scores)

score_long <- melt(data = scores,
                   id.vars = c("id", "gender", "age", "grade"),
                   measure.vars = c("math", "reading", "science"),
                   variable.name = "subject",
                   value.name = "score")

# Preview the long data
head(scores_long)

nrow(scores)
nrow(scores_long)

# Reshape times to long format
times <- pisa[, c(1, 8:10)]
head(times)

# Reshape times to long
times_long <-  melt(data = times, 
                    id.vars = c("minutes_math", "minutes_reading", "minutes_science"),
                    variable.name = "subject",
                    value.name = "time")

# Preview the long data
head(times_long)

nrow(times_long)
nrow(times)

head(time_long)
# Merge data sets

times_long$subject <- ifelse(times_long$subject=="minutes_math", "math",
                             ifelse(times_long$subject=="minutes_reading", "reading",
                                    "science"))

head(times_long)














