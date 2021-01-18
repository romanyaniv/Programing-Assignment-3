## Task 4
rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("/Users/admin/Desktop/Data Science/R Programming/Programing Assignment 3/outcome-of-care-measures.csv", colClasses = "character",header=TRUE)
        df <- as.data.frame(cbind(data[, 2], data[, 7], data[, 11], data[, 17], data[, 23]), stringsAsFactors = FALSE)
        colnames(df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        df[,3:5] <- lapply(df[,3:5], function(x) (suppressWarnings(as.numeric(x))))
        
        ## Check that state and outcome are valid
        if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
                stop('invalid outcome')
        } else if(is.numeric(num) | num=="best"){
                if (num=="best"){num <- 1}
                output <- data.frame(hospital=character(0), state=character(0))
                states <- unique(df[,2])
                for (state in states){
                        state_data <- df[which(df[, "state"] == state),]
                        hosp <- state_data[order(state_data[[outcome]],state_data[["hospital"]]),][,"hospital"][num]
                        output <- rbind(output, data.frame(hospital=hosp, state=state))
                }
        } else if(!is.numeric(num)){
                if (num == "worst") {
                        output <- data.frame(hospital=character(0), state=character(0))
                        states <- unique(df[,2])
                        for (state in states){
                                state_data <- df[which(df[, "state"] == state),]
                                hosp <- state_data[order(state_data[[outcome]],state_data[["hospital"]],decreasing = TRUE),][,"hospital"][1]
                                output <- rbind(output, data.frame(hospital=hosp, state=state))
                        }
                }
                ## Return a data frame with the hospital names and the (abbreviated) state name
        }
        return(output[order(output[["state"]]),])
}
## The function returns a data frame containing the hospital in each state that has the specified ranking.

