
# Reproducible Research: Peer Assessment 1

```{r}
echo = TRUE
```

## Loading and preprocessing the data
```{r}
# original file URL
downloadUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

# file check
if (!file.exists("activity.csv") ) {
    download.file(downloadUrl, destfile = "data.zip")
    unzip("data.zip")
    file.remove("data.zip")
}

#read in data or not if it's already loaded
if(!"activity.csv" %in% ls()){
    activity <- read.csv("activity.csv")
}

```


## What is mean total number of steps taken per day?

remove NAs
```{r}
act <- activity[complete.cases(activity),] 
```
some days have no complete cases

* Calculate the total number of steps taken per day
```{r}
steps_per_day <- with(act, tapply(steps, date, sum) )
```

* Make a histogram of the total number of steps taken each day
```{r}
library(ggplot2)
ggplot(as.data.frame(steps_per_day), aes(steps_per_day)) + 
    geom_histogram() + 
    labs(xlab = "number of steps", title = "Steps per Day")
```

* Calculate and report the mean and median of the total number of steps taken per day
```{r}
summary(steps_per_day)
```
mean is 10770 steps per day
median is 10760 steps per day


## What is the average daily activity pattern?

* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)
```{r}
steps_per_interval <- with(act, tapply(steps, interval, mean) )
qplot(x = unique(act$interval),
      y = steps_per_interval,
      type = "l",
      xlab = "interval",
      ylab = "step count",
      main = "Steps per Interval"
      )
```

* Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?
```{r}
unique(act$interval)[which.max(steps_per_interval)]
# [1] 835
max(steps_per_interval)
# [1] 206.1698
```
The interval 835 has the highest average number of steps, 206.1698


## Imputing missing values

* Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)
```{r}
sum(is.na(activity))
# [1] 2304
```
Number of NA's in the dataset. All in the steps column

* Devise a strategy for filling in all of the missing values in the dataset.
 - steps NA's will be replaced with the mean for that 5-minute interval.

* Create a new dataset that is equal to the original dataset but with the missing
data filled in.
```{r}
imputed_activity <- activity
for(i in 1:length(imputed_activity$steps)){
    if( is.na(imputed_activity$steps[i]) ){
        imputed_activity$steps[i] <- steps_per_interval[as.character(imputed_activity$interval[i])]
    }
}
```

* Make a histogram of the total number of steps taken each day and
Calculate and report the mean and median total number of steps taken per day.

Do these values differ from the estimates from the first part of the assignment?
What is the impact of imputing missing data on the estimates of the total daily 
number of steps?
```{r}
imputed_steps_per_day <- with(imputed_activity, tapply(steps, date, sum) )
library(ggplot2)
ggplot(as.data.frame(imputed_steps_per_day), aes(imputed_steps_per_day)) + 
    geom_histogram() + 
    labs(xlab = "number of steps", title = "Imputed Steps per Day")
```

Summary
```{r}
summary(imputed_steps_per_day)
```
Mean is 10770 steps per day
Median is 10770 steps per day
Mean is unchanged. Median has slightly increaded

Compare the total daily number of steps for the activity data before and after it was imputed
```{r}
g1 <- ggplot(as.data.frame(steps_per_day), aes(steps_per_day)) + 
    geom_histogram() + 
    labs(xlab = "number of steps", title = "Steps per Day") +
    ylim(0,14)
g2 <- ggplot(as.data.frame(imputed_steps_per_day), aes(imputed_steps_per_day)) + 
    geom_histogram() + 
    labs(xlab = "number of steps", title = "Imputed Steps per Day") + 
    ylim(0,14)
library(gridExtra)
grid.arrange(g1,g2, nrow=1, ncol=2)
```

Imputing the missing data increased the number of steps per day on the histogram


## Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels – “weekday” and “weekend”
indicating whether a given date is a weekday or weekend day.
```{r}
days <- unique(weekdays(as.Date(imputed_activity$date)))
wdays <- days[1:5]
imputed_activity$dayType <- weekdays(as.Date(imputed_activity$date))
for(i in 1:length(imputed_activity$dayType) ){
    if(imputed_activity$dayType[i] %in% wdays){
        imputed_activity$dayType[i] <- "weekday"
    }else{
        imputed_activity$dayType[i] <- "weekend"
    }
}
```

Get average step per interval aggregated for interval and dayType
```{r}
step_per_interval_dayType <- with(imputed_activity, aggregate(steps ~ interval + dayType, FUN = mean)) 
# dim = 576 3
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute
interval (x-axis) and the average number of steps taken, averaged across all  
weekday days or weekend days (y-axis). 
```{r}
g3 <- ggplot(step_per_interval_dayType, aes(interval, steps)) +
    geom_line() + 
    facet_grid(dayType~.)
print(g3)
```






