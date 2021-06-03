rankall <- function(outcome, num = "best"){
    
    h_name <- c()
    s_name <- c()
    
    ## Read outcome data
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    
    if (! outcome %in% c('heart attack', 'heart failure', 'pneumonia')) 
        stop("invalid outcome")
    
    s <- paste(toupper(substring(strsplit(outcome,' ')[[1]], 1,1)), 
               substring(strsplit(outcome,' ')[[1]], 2), sep="", collapse=".")
    outcome_col <- paste('Hospital.30.Day.Death..Mortality..Rates.from', 
                         s, sep = '.')
    
    states <- unique(data$State)
    states <- states[order(states)]
    
    
    ## Return hospital name in that state with the given rank 30-day death rate
    
    for (state in states) {
        data_use <- data[data$State == state,]
        data_use <- data_use[!(data_use[[outcome_col]] == 'Not Available'),]
        data_use <- data_use[order(as.numeric(data_use[[outcome_col]]), data_use[[2]]),]
        
        if(identical(num,'best')){
            name <- data_use[1,2]
        }
        else if(identical(num,'worst')){
            name <- tail(data_use,n=1)[1,2]
        }
        else{
            name <- data_use[as.numeric(num),2] 
        }
        
        h_name <- c(h_name, name)
        s_name <- c(s_name, state)
    }
    
    df <- data.frame(hospital = h_name, state = s_name)
}