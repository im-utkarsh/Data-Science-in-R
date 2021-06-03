library(lubridate)

data <- read.table('household_power_consumption.txt',sep = ';',header = TRUE)
data$Date <- dmy(data$Date)
data_use <- data[(data$Date == ymd('2007-02-01') | data$Date == ymd('2007-02-02')),]

date_time <- strptime(paste(data_use$Date,data_use$Time,sep = ' '),"%Y-%m-%d %H:%M:%S")
subMetering1 <- as.numeric(data_use$Sub_metering_1)
subMetering2 <- as.numeric(data_use$Sub_metering_2)
subMetering3 <- as.numeric(data_use$Sub_metering_3)

png("plot3.png", width=480, height=480)
plot(date_time, subMetering1, type="l", ylab="Energy Sub metering", xlab="")
lines(date_time, subMetering2, type="l", col="red")
lines(date_time, subMetering3, type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, lwd=2.5, col=c("black", "red", "blue"))
dev.off()