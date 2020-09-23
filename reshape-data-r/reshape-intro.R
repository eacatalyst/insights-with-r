# 09/23/2020
getwd()

# Load pisa data from csv file
pisa <- read.csv("pisa.csv", header = TRUE)

# Preview the data
head(pisa)

# load nlsy data from csv file
nlsy <- read.csv("nlsy.csv", header = TRUE)
head(nlsy)

# pisa data
table(pisa$id)


pisa_id <- as.data.frame(table(pisa$id))
pisa_id
colnames(pisa_id) <- c("id", "frequency")
head(pisa_id)
#nlsy data
nlsy_id <- as.data.frame(table(nlsy$id))
colnames(nlsy_id) <- c("id", "frequency")
head(nlsy_id)


# Method 2

nrow(pisa)
nrow(nlsy)

# Count number of unique ID
length(unique(pisa$id))
length(unique(nlsy$id))






