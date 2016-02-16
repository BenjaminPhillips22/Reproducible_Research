



# set working directory
setwd("~/R/Hopkins_Data_Science_Specialization/Reproducible_Research/Course_Project_2")

# original file URL
downloadUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

# file check
if (!file.exists("storm_data.csv.bz2") ) {
    print("downloading data from website")
    download.file(downloadUrl, destfile = "storm_data.csv.bz2")
}

#read in data or not if it's already loaded
if(!"storm_data" %in% ls()){
    print("loading data")
    storm_data <- read.csv("storm_data.csv.bz2")
}

# Information about the variables can be found here
# https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf


# download packages
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)

names(storm_data)
# We are interested in EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP

## Across the United States, which types of events (as indicated in the EVTYPE variable)
# are most harmful with respect to population health?


# get the sum of fatalities for each event type
event_fatalities <- with(storm_data, aggregate(FATALITIES, by = list(EVTYPE), sum))
names(event_fatalities) <- c("event","total.fatalities")

# re-order and use the top 10
event_fatalities <- event_fatalities[order(event_fatalities$total.fatalities, decreasing = TRUE),]
event_fatalities <- event_fatalities[1:10,]

#graph
g1 <- ggplot(data = event_fatalities, aes(x = reorder(event, -total.fatalities), total.fatalities)) + 
    geom_bar(stat = "identity") +
    ggtitle("Total Fatalities per Event Type") +
    xlab("Event") +
    ylab("total fatalities") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
    
print(g1)
# Tornado has the most fatalities of any event


# get the sum of injuries for each event type
event_injuries <- with(storm_data, aggregate(INJURIES, by = list(EVTYPE), sum))
names(event_injuries) <- c("event","total.injuries")

# re-order and use the top 10
event_injuries <- event_injuries[order(event_injuries$total.injuries, decreasing = TRUE),]
event_injuries <- event_injuries[1:10,]

#graph
g2 <- ggplot(data = event_injuries, aes(x = reorder(event, -total.injuries), total.injuries)) + 
    geom_bar(stat = "identity") +
    ggtitle("Total Injuries per Event Type") +
    xlab("Event") +
    ylab("total injuries") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

print(g2)
# Tornado has the most injuries of any event


grid.arrange(g1,g2, nrow = 1)
# Tornados are responsible for the most injuries and fatalities.




## Across the United States, which types of events have the greatest economic consequences?

# Tidy up the data by making PROPDMG, PROPDMGEXP and CROPDMG, CROPDMGEXP one column each
economic_data <- storm_data[,c("EVTYPE", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

unique(economic_data$PROPDMGEXP)
unique(economic_data$CROPDMGEXP)

exps <- c(K=1e3, k=1e3, M=1e6, B=1e9, m=1e6, h=1e2, H=1e2, "?"=1, "-"=1, "+"=1, "0"=1, "1"=1, "2"=1e2, "3"=1e3, "4"=1e4, "5"=1e5, "6"=1e6, "7"=1e7, "8"=1e8)

# make it not a factor, get rid of "" and turn into a number
economic_data$PROPDMGEXP <- as.character(economic_data$PROPDMGEXP)
economic_data$PROPDMGEXP[economic_data$PROPDMGEXP==""] <- 1
economic_data$PROPDMGEXP  <- exps[economic_data$PROPDMGEXP]

#
economic_data$CROPDMGEXP <- as.character(economic_data$CROPDMGEXP)
economic_data$CROPDMGEXP[economic_data$CROPDMGEXP==""] <- 1
economic_data$CROPDMGEXP  <- exps[economic_data$CROPDMGEXP]


costs_data <- mutate(economic_data, PROPDMG=PROPDMG*PROPDMGEXP, CROPDMG=CROPDMG*CROPDMGEXP)
costs_data <- costs_data[,c("EVTYPE","PROPDMG","CROPDMG")]

# make PROPDMG and CROPDMG a factor, not a variable
costs_data_new <- gather(data = costs_data, key = damage.type, cost, c(PROPDMG,CROPDMG))

#get top 10 costly events
a <- with(costs_data_new, aggregate(cost, by = list(EVTYPE), sum))
a <- a[order(a$x, decreasing = TRUE),]
top10_events <- a[1:10,1]

# filter costs_data_new for events just in the top 10
costs_data_top10 <- costs_data_new[costs_data_new$EVTYPE %in% top10_events, ]

# aggregate cost on event type and damage type
costs_total <- aggregate( cost~EVTYPE+damage.type, data = costs_data_top10 ,FUN=sum)

# plot it
g3 <- ggplot(data = costs_total, aes(x=reorder(EVTYPE,-cost), y=cost, fill=damage.type)) +
    geom_bar(stat="identity") +
    ggtitle("Total Cost per Event Type") +
    xlab("Event") +
    ylab("total cost") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
print(g3)

# Property Damage far outweighs crop damage for all the top ten most costly events,
# except for drought, which is the most costly event in terms of crop damage

