



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



# download packages
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)

names(storm_data)
# We are interested in EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP


## Across the United States, which types of events (as indicated in the EVTYPE variable)
# are most harmful with respect to population health?


health_data <- storm_data %>% 
    tbl_df() %>%
    select(EVTYPE, FATALITIES, INJURIES) %>%
    group_by(EVTYPE) %>%
    summarise(fatalities = sum(FATALITIES), injuries = sum(INJURIES))

fatalities_top10 <- health_data %>%
    select(EVTYPE, fatalities) %>%
    arrange(desc(fatalities)) %>%
    head(10)


injuries_top10 <- health_data %>%
    select(EVTYPE, injuries) %>%
    arrange(desc(injuries)) %>%
    head(10)

g1 <- ggplot(data = fatalities_top10, aes(x = reorder(EVTYPE, -fatalities), fatalities)) +
    geom_bar(stat = "identity") +
    ggtitle("Total Fatalities per Event Type") +
    xlab("Event") +
    ylab("total fatalities") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

print(g1)    

g2 <- ggplot(data = injuries_top10, aes(x = reorder(EVTYPE, -injuries), injuries)) +
    geom_bar(stat = "identity") +
    ggtitle("Total injuries per Event Type") +
    xlab("Event") +
    ylab("total injuries") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

#print(g2)    

# make the plots the same height
gA <- ggplotGrob(g1)
gB <- ggplotGrob(g2)
maxHeight = grid::unit.pmax(gA$heights[2:5], gB$heights[2:5])
gA$heights[2:5] <- as.list(maxHeight)
gB$heightss[2:5] <- as.list(maxHeight)

grid.arrange(gA, gB, nrow=1) 



## Across the United States, which types of events have the greatest economic consequences?


economic_data <- storm_data %>%
    tbl_df() %>%
    select(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)



unique(economic_data$PROPDMGEXP)
unique(economic_data$CROPDMGEXP)

# Tidy up the PRODMGEXP and CROPDMGEXP by making them numeric and
# make PROPDMG and CROPDMG a factor, not a variable

exps <- c(K=1e3, k=1e3, M=1e6, B=1e9, m=1e6, h=1e2, H=1e2, "?"=1, "-"=1, "+"=1,
          "0"=1, "1"=1, "2"=1e2, "3"=1e3, "4"=1e4, "5"=1e5, "6"=1e6, "7"=1e7, "8"=1e8)

economic_data$PROPDMGEXP <- as.character(economic_data$PROPDMGEXP)
economic_data$PROPDMGEXP[economic_data$PROPDMGEXP==""] <- 1
economic_data$PROPDMGEXP  <- exps[economic_data$PROPDMGEXP]

economic_data$CROPDMGEXP <- as.character(economic_data$CROPDMGEXP)
economic_data$CROPDMGEXP[economic_data$CROPDMGEXP==""] <- 1
economic_data$CROPDMGEXP  <- exps[economic_data$CROPDMGEXP]

costs_data <- economic_data %>%
    mutate(PROPDMG=PROPDMG*PROPDMGEXP, CROPDMG=CROPDMG*CROPDMGEXP) %>%
    select(EVTYPE,PROPDMG,CROPDMG) %>%
    gather(key = damage.type, value = cost, c(PROPDMG,CROPDMG)) %>%
    group_by(EVTYPE)

costs_top10_events <- costs_data  %>%
    summarise(total.cost = sum(cost)) %>%
    arrange(desc(total.cost)) %>%
    select(EVTYPE) %>%
    head(10)

costs_data <- costs_data %>%
    filter(EVTYPE %in% unlist(costs_top10_events))  %>%
    group_by(EVTYPE, damage.type) %>%
    summarise(cost = sum(cost))


g3 <- ggplot(data = costs_data, aes(x=reorder(EVTYPE,-cost), y=cost, fill=damage.type)) +
    geom_bar(stat="identity") +
    ggtitle("Total Cost per Event Type") +
    xlab("Event") +
    ylab("total cost") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
print(g3)










