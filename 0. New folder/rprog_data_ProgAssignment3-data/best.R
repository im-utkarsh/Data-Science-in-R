best <- function(state, outcome){
    
    ## Read outcome data
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    
    ## Check that state and outcome are valid
    
    if (! state %in% data$State) stop("invalid state")
    if (! outcome %in% c('heart attack', 'heart failure', 'pneumonia')) 
        stop("invalid outcome")

    s <- paste(toupper(substring(strsplit(outcome,' ')[[1]], 1,1)), 
          substring(strsplit(outcome,' ')[[1]], 2), sep="", collapse=".")
    outcome_col <- paste('Hospital.30.Day.Death..Mortality..Rates.from', 
          s, sep = '.')
    
    
    ## Return hospital name in that state with lowest 30-day death rate
    
    data_use <- data[data$State == state,]
    row <- which(as.numeric(data_use[[outcome_col]]) == min(as.numeric(data_use[[outcome_col]]), na.rm = TRUE))
    name <- data_use[row,2]
    name <- name[order(name)]
    name
    
}