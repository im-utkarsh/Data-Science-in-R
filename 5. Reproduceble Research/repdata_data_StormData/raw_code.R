library(dplyr)
library(ggplot2)

if ( !file.exists("repdata_data_StormData.csv.bz2") ) {
    download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2',
                  'repdata_data_StormData.csv.bz2')
}

data <- read.csv('repdata_data_StormData.csv.bz2')
data <- mutate(data, BGN_DATE = as.Date(BGN_DATE,'%m/%d/%Y', tz = TIME_ZONE),
               EVTYPE = trimws(EVTYPE))
hist(as.numeric(format(data$BGN_DATE,'%Y')),breaks=61, main = 'Disasters per Year',
     xlab = 'Years', col='#e0e0e0')
abline(v=1995,lwd=3,col='#62acb5')
data <- select(data, BGN_DATE, EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG,
               CROPDMGEXP, FATALITIES, INJURIES) %>%
    filter(format(BGN_DATE,'%Y') > 1995) %>%
    filter(FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0)


## It may take some time.

data$PROPDMGEXP[(data$PROPDMGEXP == '')] <- 1
data$PROPDMGEXP[(data$PROPDMGEXP == 'K')] <- 10^3
data$PROPDMGEXP[(data$PROPDMGEXP == 'M')] <- 10^6
data$PROPDMGEXP[data$PROPDMGEXP == 'B'] <- 10^9
data$PROPDMGEXP <- as.numeric(data$PROPDMGEXP)

data$CROPDMGEXP[(data$CROPDMGEXP == '')] <- 1
data$CROPDMGEXP[(data$CROPDMGEXP == 'K')] <- 10^3
data$CROPDMGEXP[(data$CROPDMGEXP == 'M')] <- 10^6
data$CROPDMGEXP[(data$CROPDMGEXP == 'B')] <- 10^9
data$CROPDMGEXP <- as.numeric(data$CROPDMGEXP)


health <- group_by(data,EVTYPE) %>% 
  summarise(FATALITIES = sum(FATALITIES, na.rm = TRUE), 
            INJURIES = sum(INJURIES, na.rm = TRUE)) %>%
  filter(FATALITIES > 0 | INJURIES > 0) %>% 
  arrange(desc(FATALITIES+INJURIES))


h<-rbind(select(health[1:27,],EVTYPE,FATALITIES) %>% rename(HEALTH = FATALITIES) %>%
           mutate(TYPE='fatalities'),
         select(health[1:27,],EVTYPE,INJURIES) %>% rename(HEALTH = INJURIES) %>%
           mutate(TYPE='injuries'))


g <- ggplot(h, aes(x = factor(EVTYPE,levels = health$EVTYPE[1:27],ordered=TRUE),
                   y = log(HEALTH), fill = TYPE)) + geom_bar(stat = "identity")

g + geom_text(stat="count", y=0.6, hjust=0, angle = 90,col='black',
              label=c(rbind(h[1:27,]$EVTYPE,rep('',27)))) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) + labs(x='Weather Event') + ggtitle("Weather event types impact on public health")





economy <- group_by(data,EVTYPE) %>% 
  summarise(PROPDMG = sum(PROPDMG*PROPDMGEXP, na.rm = TRUE), 
            CROPDMG = sum(CROPDMG*CROPDMGEXP, na.rm = TRUE)) %>%
  filter(PROPDMG > 0 | CROPDMG > 0) %>%
  arrange(desc(PROPDMG+CROPDMG))

e<-rbind(select(economy[1:27,],EVTYPE,PROPDMG) %>% rename(DAMAGE = PROPDMG) %>%
           mutate(TYPE='Property Damage'),
         select(economy[1:27,],EVTYPE,CROPDMG) %>% rename(DAMAGE = CROPDMG) %>%
           mutate(TYPE='Crop Damage'))

g <- ggplot(e, aes(x = factor(EVTYPE,levels = economy$EVTYPE[1:27],ordered=TRUE),
                     y = log(DAMAGE), fill = TYPE)) + geom_bar(stat = "identity")

g + geom_text(stat="count", y=0.6, hjust=0, angle = 90,col='black',
              label=c(rbind(e[1:27,]$EVTYPE,rep('',27)))) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) + labs(x='Type')






