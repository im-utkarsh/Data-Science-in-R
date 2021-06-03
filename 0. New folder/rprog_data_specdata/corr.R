corr <- function(directory, threshold = 0){
    co <- numeric()
    for (i in 1:332) {
        data <- read.csv(paste(as.character(sprintf("%03d", i)),'csv',sep = '.'))
        good <- sum(complete.cases(data))
        if(good > threshold){
            co <- c(co, cor(data$sulfate, data$nitrate, use = "complete.obs"))
        }
    }
    print(co)
    co
}