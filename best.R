## Task 2 
best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("/Users/admin/Desktop/Data Science/R Programming/Programing Assignment 3/outcome-of-care-measures.csv", colClasses = "character")
        df <- as.data.frame(cbind(data[ , 2], data[ , 7], data[ , 11], data[ , 17], data[ , 23]), stringsAsFactors=FALSE)
        colnames(df) <- c('hospital', 'state', "heart attack", "heart failure", "pneumonia")
        
        ## Check that state and outcome are valid
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
        ## Return hospital name in that state with lowest 30-day death rate
}
## finding a hospital with the lowest mortality from disease by state