library(lubridate)

data <- read.table('household_power_consumption.txt',sep = ';',header = TRUE)
data$Date <- dmy(data$Date)
data_use <- data[(data$Date == ymd('2007-02-01') | data$Date == ymd('2007-02-02')),]

Global_active_power <- as.numeric(data_use$Global_active_power)

png("plot1.png", width=480, height=480)
hist(Global_active_power, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
dev.off()