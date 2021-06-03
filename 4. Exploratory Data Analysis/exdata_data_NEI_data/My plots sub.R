## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Plot 1
tble<-group_by(NEI,as.factor(year)) %>% summarise(sum(Emissions))
tble <- rename(tble,year = `as.factor(year)`,Emissions=`sum(Emissions)`)
barplot(tble$Emissions, names.arg = tble$year,
        ylab = 'PM2.5 Emission (tons)', xlab = 'year',
        main = 'Total PM2.5 Emission from all Sources')

## Plot 2
tble <- subset(NEI,fips=='24510') %>%
    group_by(year) %>%
    summarise(sum(Emissions)) %>%
    rename(Emissions=`sum(Emissions)`)
barplot(tble$Emissions, names.arg = tble$year,
        ylab = 'PM2.5 Emission (tons)', xlab = 'year',
        main = 'Total PM2.5 Emission from all Sources in Baltimore City')

## Plot 3
tble <- subset(NEI,fips=='24510') %>%
    group_by(year,type) %>%
    summarise(sum(Emissions)) %>%
    rename(Emissions=`sum(Emissions)`)
g <- ggplot(data = tble, aes(as.factor(year),Emissions))
g + facet_grid(.~type) + geom_col() +
    labs(x = 'Year', y = expression(PM[2.5]*' Emissions (tons)'),
         title = expression('Baltimore City '*PM[2.5]*' Emission by type'))


## Plot 4
subs <- (SCC %>% filter(grepl('Coal',EI.Sector, ignore.case=TRUE)))$SCC
tble <- subset(NEI,SCC %in% subs) %>%
    group_by(year) %>%
    summarise(sum(Emissions)) %>%
    rename(Emissions=`sum(Emissions)`)
barplot(tble$Emissions, names.arg = tble$year,
        ylab = expression(PM[2.5]*' Emissions (tons)'), xlab = 'Year',
        main = expression(PM[2.5]*' Emissions from vehicle sources'))

## Plot 5
tble <- subset(NEI,type == 'ON-ROAD' & fips=='24510') %>%
    group_by(year) %>%
    summarise(sum(Emissions)) %>%
    rename(Emissions=`sum(Emissions)`)
barplot(tble$Emissions, names.arg = tble$year,
        ylab = expression(PM[2.5]*' Emissions (tons)'), xlab = 'Year',
        main = expression(PM[2.5]*' Emissions from motor vehicle sources'))

## Plot 6
tble <- subset(NEI,type == 'ON-ROAD' & (fips=='24510' | fips=='06037')) %>%
    group_by(year,fips) %>%
    summarise(sum(Emissions)) %>%
    rename(Emissions=`sum(Emissions)`)
g <- ggplot(data = tble, aes(as.factor(year),Emissions))
g + geom_col(position = 'dodge' ,aes(fill=fips)) +
    labs(x = 'Year', y = expression(PM[2.5]*' Emissions (tons)'),
         title = expression('Los Angeles Country and Baltimore City '*PM[2.5]*' Emission')) +
    guides(fill=guide_legend(title=NULL)) +
    scale_fill_discrete(labels = c('Los Angeles',"Baltimore"))