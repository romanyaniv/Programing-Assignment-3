## Task3

rankhospital <- function(state, outcome, rank = "best"){
        ## Read outcome data
        data <- read.csv("/Users/admin/Desktop/Data Science/R Programming/Programing Assignment 3/outcome-of-care-measures.csv", colClasses = "character",header=TRUE)
        df <- as.data.frame(cbind(data[, 2], data[, 7], data[, 11], data[, 17], data[, 23]), stringsAsFactors = FALSE)
        colnames(df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        df[,3:5] <- lapply(df[,3:5], function(x) suppressWarnings(as.numeric(x)))
        
        ## Check that state and outcome are valid
        if(!(state %in% df[, "state"])){
                stop('invalid state')
        } else if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
                stop('invalid outcome')
        } else if(is.numeric(rank)){
                interested_data <- df[which(df[, "state"] == state),]
                output <- interested_data[order(interested_data[[outcome]],interested_data[["hospital"]]),][,"hospital"][rank]
        } else if(!is.numeric(rank)){
                if (rank == "best") {
                        output <- best(state, outcome)
                } else if (rank == "worst"){
                        interested_data <- df[which(df[, "state"] == state),]
                        output <- interested_data[order(interested_data[[outcome]],interested_data[["hospital"]], decreasing = TRUE),][,"hospital"][1]
                } else {
                        stop('invalid rank')
                }
                ## Return hospital name in that state with the given rank 30-day death rate
        }
        return(output)
}
## The function returns a hospital of the specified number in a rate of mortality from disease by state