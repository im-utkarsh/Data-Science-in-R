library(lubridate)

data <- read.table('household_power_consumption.txt',sep = ';',header = TRUE)
data$Date <- dmy(data$Date)
data_use <- data[(data$Date == ymd('2007-02-01') | data$Date == ymd('2007-02-02')),]

Global_active_power <- as.numeric(data_use$Global_active_power)
date_time <- strptime(paste(data_use$Date,data_use$Time,sep = ' '),"%Y-%m-%d %H:%M:%S")
voltage <- as.numeric(data_use$Voltage)
subMetering1 <- as.numeric(data_use$Sub_metering_1)
subMetering2 <- as.numeric(data_use$Sub_metering_2)
subMetering3 <- as.numeric(data_use$Sub_metering_3)
Global_reactive_power <- as.numeric(data_use$Global_reactive_power)

png("plot4.png", width=480, height=480)
par(mfrow=c(2,2))
plot(date_time,Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
plot(date_time,voltage, type="l", xlab="datetime", ylab="Voltage")
plot(date_time, subMetering1, type="l", ylab="Energy Sub metering", xlab="")
lines(date_time, subMetering2, type="l", col="red")
lines(date_time, subMetering3, type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, lwd=2.5, col=c("black", "red", "blue"),bty = 'n')
plot(date_time,Global_reactive_power, type="l", xlab="datetime")
dev.off()