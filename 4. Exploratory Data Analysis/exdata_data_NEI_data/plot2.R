library(dplyr)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

emission_by_year <- NEI[(NEI$fips == "24510"),] %>% group_by(year) %>%
    summarize(total_emissions = sum(Emissions))

dev.copy(png,'plot2.png')
with(emissions_by_year, barplot(total_emissions, names.arg = year, xlab = "Year",
                                ylab = "PM2.5 Emissions (tons)",
                                main = "Total PM2.5 Emissions from Baltimore City Sources"))
dev.off()