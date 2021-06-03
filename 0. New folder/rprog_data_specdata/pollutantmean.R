pollutantmean <- function(directory, pollutant, id=1:332){
    add <- 0
    len <- 0
    for (i in id) {
        data <- read.csv(paste(as.character(sprintf("%03d", i)),'csv',sep = '.'))
        usableData <- data[!is.na(data[[pollutant]]), ][[pollutant]]
        add <- add + sum(usableData)
        len <- len + length(usableData)
    }
    m <- add/len
    print(m)
}