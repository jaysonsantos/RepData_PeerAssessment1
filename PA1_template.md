---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
unzip('activity.zip')
activity = read.csv('activity.csv', header=T, stringsAsFactors=F)
activity$steps = as.integer(activity$steps)
activity$date = as.Date(strptime(activity$date, '%Y-%m-%d'))
```


## What is mean total number of steps taken per day?
- Make a histogram of the total number of steps taken each day

```r
sumSteps <- aggregate(steps ~ date, activity, sum, na.rm = T)
hist(sumSteps$steps, xlab='Steps', main='Steps per day')
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 
- Calculate and report the mean and median total number of steps taken per day

```r
mean(sumSteps$steps)
```

```
## [1] 10766.19
```

```r
median(sumSteps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
- Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
averageSteps = aggregate(steps ~ interval, activity, mean , na.rm = T)
with(averageSteps, plot(interval, steps, type='l'))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 
- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
averageSteps[which.max(averageSteps$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Inputing missing values
- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
missingValues <- activity[is.na(activity$steps),]
nrow(missingValues)
```

```
## [1] 2304
```

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
- Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newActivity <- activity
for (l in 1:nrow(newActivity)) {
  line <- newActivity[l,]
  if (is.na(line$steps)) {
    newActivity[l,]$steps <- averageSteps[averageSteps$interval == line$interval,]$steps
  }
}
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
newSumSteps <- aggregate(steps ~ date, activity, sum, na.rm = T)
hist(newSumSteps$steps, xlab='Steps', main='Steps per day (filled)')
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
mean(newSumSteps$steps)
```

```
## [1] 10766.19
```

```r
median(newSumSteps$steps)
```

```
## [1] 10765
```

## Are there differences in activity patterns between weekdays and weekends?
- Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.

```r
activity$weekday <- as.factor(ifelse(weekdays(activity$date) %in% c('Sunday', 'Saturday'), 'weekend', 'weekday'))
```
- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
data = aggregate(steps ~ interval + weekday, activity, mean, rm.na=T)
library(lattice)
xyplot(steps ~ interval | weekday, data=data, type='l', layout=c(1,2))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 
