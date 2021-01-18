## Task 1
getwd()
dff <- read.csv("/Users/admin/Desktop/Data Science/R Programming/Programing Assignment 3/outcome-of-care-measures.csv")
head(dff)
str(dff)
dff[ , 11] <- as.numeric(dff[, 11])
hist(dff[ , 11])
## ploting the 30-day mortality rates for heart attack