complete <- function(directory, id=1:332){
    n <- c()
    for (i in id) {
        data <- read.csv(paste(as.character(sprintf("%03d", i)),'csv',sep = '.'))
        good <- sum(complete.cases(data))
        n <- c(n,good)
    }
    df <- data.frame(id = id, nobs = n)
    print(df)
}