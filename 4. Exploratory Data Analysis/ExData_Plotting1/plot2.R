library(lubridate)

data <- read.table('household_power_consumption.txt',sep = ';',header = TRUE)
data$Date <- dmy(data$Date)
data_use <- data[(data$Date == ymd('2007-02-01') | data$Date == ymd('2007-02-02')),]

Global_active_power <- as.numeric(data_use$Global_active_power)
date_time <- strptime(paste(data_use$Date,data_use$Time,sep = ' '),"%Y-%m-%d %H:%M:%S")

png("plot2.png", width=480, height=480)
plot(date_time,Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
dev.off()