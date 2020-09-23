# 09/23/2020

# Reshape Wide to Long

install.packages("reshape2")
library("reshape2")
library(tidyr)

head(pisa)

# Part 1: Reshape scores to long format ----
scores <- pisa[,1:7]
# Preview Data
head(scores)

#Reshape the data to long
list(names(scores)[5:7])
scores_long <- reshape(scores,
                       varying = list(names(scores)[5:7]),
                       times = c("math", "reading", "science"),
                       timevar = "subject",
                       direction = "long")
# Preview the long data
head(scores_long,8)

# Rename the last colum
colnames(scores_long)[6] <- "score"
head(scores_long)

# Remove lengthy row name
row.names(scores_long) <- NULL
head(scores_long)

nrow(scores)
nrow(scores_long)

# Reshape times to long format
times <- pisa[, c(1,8:10)]
head(times)

# Reshape the data to long
list(names(times)[2:4])

time_long <- reshape(data = times,
                     varying = list(names(times)[2:4]),
                     times = c("math", "reading", "science"),
                     timevar = "subject",
                     direction = "long")

head(time_long)
# Remove long row id
row.names(time_long) <- NULL
head(time_long)

colnames(time_long)[3] <- "time"
head(time_long)

# Merge data sets
pisa_long <- merge(
  scores_long,
  time_long,
  by = c("id", "subject")
)

head(pisa_long)
nrow(pisa_long)







