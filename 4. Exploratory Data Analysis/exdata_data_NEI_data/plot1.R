library(dplyr)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

emission_by_year <- NEI %>% group_by(year) %>%
    summarize(total_emissions = sum(Emissions))

png(filename = "plot1.png")
with(emission_by_year, barplot(total_emissions, names.arg = year, xlab = "Year",
                               ylab = "PM2.5 Emissions (tons)",
                               main = "Total PM2.5 Emissions from All Sources"))
dev.off()