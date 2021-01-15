getwd()
df <- read.csv("/Users/admin/Desktop/Data Science/R Programming/Programing Assignment 3/hospital-data.csv")
head(df)
str(df)

dff <- read.csv("/Users/admin/Desktop/Data Science/R Programming/Programing Assignment 3/outcome-of-care-measures.csv")
head(dff)
str(dff)

## Task 1
dff[ , 11] <- as.numeric(dff[, 11])
hist(dff[ , 11])
## ploting the 30-day mortality rates for heart attack

## Task 2
best <- function(state, outcome) {
        data <- read.csv("/Users/admin/Desktop/Data Science/R Programming/Programing Assignment 3/outcome-of-care-measures.csv", colClasses = "character")
        df <- as.data.frame(cbind(data[ , 2], data[ , 7], data[ , 11], data[ , 17], data[ , 23]), stringsAsFactors=FALSE)
        colnames(df) <- c('hospital', 'state', "heart attack", "heart failure", "pneumonia")
        
        if(!state %in% df[, "state"]) {
                stop("invalid state")
        } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop("invalid outcome")
        } else {
                x <- which(df[, "state"] == state)
                states <- df[x, ]
                y <- as.numeric(states[, eval(outcome)])
                minv <- min(y, na.rm=TRUE)
                best_hospital <- states[ , "hospital"] [which(y==minv)]
                output <- best_hospital[order(best_hospital)]
        }
        return(output)
}
                
## Task3