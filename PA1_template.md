---
title: "Reproducible Research: Peer Assignment 1"
output: 
        html_document:
                keep_md: true
---



## Loading and preprocessing the data

```r
activity_df <- read.csv('activity.csv', header = T, na.strings = 'NA')
```

## What is mean total number of steps taken per day?

```r
# Histogram of total number of steps taken each day 
total_steps <- aggregate(steps ~ date, activity_df, FUN = sum)
hist(total_steps$steps, main = 'Total Steps Taken Each Day', xlab = 'Number of Steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# Calculating mean and median of total number of steps taken per day 
mean(total_steps$steps, na.rm =  T)
```

```
## [1] 10766.19
```

```r
median(total_steps$steps, na.rm = T)
```

```
## [1] 10765
```
Thus, mean of total steps taken per day is 10766.19 steps and the median is 10765 steps.

## What is the average daily activity pattern?

```r
# Time series plot of 5-min interval and mean of steps taken averaged across all days
meanSteps_timeseries <- aggregate(steps ~ interval, activity_df, FUN = mean)
plot(meanSteps_timeseries, type = 'l', main = 'Average Daily Activity Pattern', 
     xlab = 'Time Interval', ylab = 'Average Steps Taken')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# 5-min interval, on average, that contains max number of steps
meanSteps_timeseries[which.max(meanSteps_timeseries$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

## Inputing missing values

```r
# Calculating and report total number of missing values in dataset
sum(is.na(activity_df))
```

```
## [1] 2304
```

```r
# Use mean to fill in missing values and create new data frame
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activity_ndf <- data.frame(
        sapply(
                activity_df,
                function(x) ifelse(is.na(x),
                                   mean(x, na.rm = T),
                                   x)))
activity_ndf$date <- activity_df$date

# Look at dataframe to make sure missing values are filled 
head(activity_ndf, 5)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
```

```r
# Histogram of total steps taken each day
total_nSteps <- aggregate(steps ~ date, activity_ndf, FUN = sum)
hist(total_nSteps$steps, main = 'Total Steps Taken Each Day', xlab = 'Number of Steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# Calculating mean and median of total number of steps taken per day 
mean(total_nSteps$steps, na.rm =  T)
```

```
## [1] 10766.19
```

```r
median(total_nSteps$steps, na.rm = T)
```

```
## [1] 10766.19
```

```r
# Difference between the two datasets
mean(total_nSteps$steps, na.rm =  T) - mean(total_steps$steps, na.rm =  T)
```

```
## [1] 0
```

```r
median(total_nSteps$steps, na.rm = T) - median(total_steps$steps, na.rm = T)
```

```
## [1] 1.188679
```

```r
sum(total_nSteps$steps) - sum(total_steps$steps)
```

```
## [1] 86129.51
```
Thus, both the median and the mean of total steps taken per day is 10766.19 steps for the new dataset. 
The difference between the two medians is 1.188679 and there is a difference of 86129.51 in total steps.

## Are there differences in activity patterns between weekdays and weekends?

```r
library(lattice)

# Separate data between weekdays and weekends
dayType <- function(date) {
        day <- weekdays(date)
        week_day <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
        week_end <- c('Saturday', 'Sunday')
        if (day %in% week_day)
                return ('weekday')
        else if (day %in% week_end)
                return ('weekend')
        else
                stop ('Invalid')
}
activity_ndf$date <- as.Date(activity_ndf$date)
activity_ndf$day <- sapply(activity_ndf$date, FUN = dayType)

# Create lattice plot of 5-min interval and mean of steps taken averaged across all days
mean_nTimeseries <- aggregate(steps ~ interval + day, activity_ndf, FUN = mean)
xyplot(steps ~ interval | day, group = day, data = mean_nTimeseries, type = 'l',
       layout = c(1,2), xlab = 'Interval', ylab = 'Average Number of Steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->







