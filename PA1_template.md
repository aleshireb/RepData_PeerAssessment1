---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
library(tidyverse)
library(lubridate)
```

## Loading and Preprocessing the Data  
##### 1. Load the data (i.e. read.csv())  

```r
if(!file.exists('activity.csv')){
     unzip('activity.zip')
}

activity_data <- read.csv('activity.csv')
```

##### 2. Process/transform the data (if necessary) into a format suitable for your analysis  


```r
# Correct data type for date
activity_data$date <- ymd(activity_data$date)

total_steps_day <- aggregate(steps ~ date, activity_data, sum)
```
## What is mean total number of steps taken per day?  

#### 1. Make a histogram of the total number of steps taken each day  

```r
hist_plot <- ggplot(total_steps_day, aes(x = steps)) +
    geom_histogram(binwidth=500, color = "white", fill = "lightblue") +
    labs(title="Histogram of Number of Steps taken each day", 
         y ="Frequency",  x="Daily Steps") 
hist_plot
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

#### 2. Calculate and report the mean and median total number of steps taken per day  


```r
mean(total_steps_day$steps)
```

```
## [1] 10766.19
```

```r
median(total_steps_day$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?  
#### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  


```r
# Prepare data for ggplot
avg_steps_time <- aggregate(x=list(meanSteps=activity_data$steps),
  by=list(interval=activity_data$interval), FUN=mean, na.rm=TRUE)

# Plot average number of steps by 5-minute interval
plot_step_time <- ggplot(data=avg_steps_time, 
    aes(x=interval, y=meanSteps)) +
    geom_line(color = "slateblue") +
    xlab("5-minute Interval") +
    ylab("Average Number of Steps Taken")

plot_step_time
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
value_most_steps <- which.max(avg_steps_time$meanSteps)

interval_most_steps <- avg_steps_time$interval[value_most_steps]

sprintf("Maximum number of steps is coming from %gth 5-min interval",
        interval_most_steps)
```

```
## [1] "Maximum number of steps is coming from 835th 5-min interval"
```

## Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity_data))
```

```
## [1] 2304
```

#### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
impute_activity <- activity_data

impute_activity$steps[is.na(impute_activity$steps)] <-
    mean(impute_activity$steps, na.rm=TRUE)

impute_activity$steps <- as.numeric(impute_activity$steps)

impute_activity$interval <- as.numeric(impute_activity$interval)

colSums(is.na(impute_activity))
```

```
##    steps     date interval 
##        0        0        0
```

#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
impute_steps_day <- aggregate(steps ~ date, impute_activity, sum)

impute_plot <- ggplot(impute_steps_day, aes(x = steps)) +
    geom_histogram(binwidth=500, color = "white", fill = "slateblue") +
    labs(title="Histogram of Number of Steps taken each day", 
         y ="Frequency",  x="Daily Steps") 
impute_plot
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


```r
mean(impute_steps_day$steps)
```

```
## [1] 10766.19
```

```r
median(impute_steps_day$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

#### 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
impute_activity$day <- ifelse(weekdays(impute_activity$date) %in%
  c("Saturday","Sunday"), "weekday", "weekend")
```

#### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
impute_avg_activity <- aggregate(steps ~ interval + day,
  data=impute_activity, mean)

ggplot(impute_avg_activity, aes(interval, steps)) + 
    geom_line(color = "slateblue") + 
    facet_grid(day ~ .) +
    xlab("5-minute Interval") + 
    ylab("Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
