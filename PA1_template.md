# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

Unzip the zip file downloaded and load the resulting csv file.


```r
# unzip the zip file and load in the corresponding csv
unzip("repdata_data_activity.zip")
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

Use the dplyr package to simplify this. A histogram is plotted of the number of steps per day, then the mean and median of the number of steps per day is listed.


```r
# load the dplyr package
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
# determine the total no of steps for each day
days <- group_by(activity, date)
dailysum <- summarise(days, sum(steps, na.rm=TRUE))
colnames(dailysum) <- c("date", "sumsteps")
# plot a histogram of the total no of steps per day
hist(dailysum$sumsteps)
```

![](PA1_template_files/figure-html/sum steps-1.png)<!-- -->

```r
# determine the mean and median of the total no of steps per day (original data containing NAs)
mean(dailysum$sumsteps)
```

```
## [1] 9354.23
```

```r
median(dailysum$sumsteps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

The average number of steps is calculated for each interval. The interval where the obvious peak is, representing the maximum average number of steps is also listed.


```r
# determine average of each interval
intervals <- group_by(activity, interval)
intervalsavg <- summarise(intervals, mean(steps, na.rm=TRUE))
colnames(intervalsavg) <- c("interval", "averagesteps")
# plot timeseries of average steps vs interval
plot(intervalsavg$interval, intervalsavg$averagesteps, type="l", xlab="Interval", ylab="Average No of steps", main="Daily activity pattern")
```

![](PA1_template_files/figure-html/daily pattern-1.png)<!-- -->

```r
# determine the interval that contains the average maximum no of steps
filter(intervalsavg, averagesteps==max(intervalsavg$averagesteps))
```

```
## Source: local data frame [1 x 2]
## 
##   interval averagesteps
##      (int)        (dbl)
## 1      835     206.1698
```

## Imputing missing values

A very simplistic method is used to impute the missing (NA) values. The newly resulting histogram is then plotted alongside the original, and the new mean and median listed.


```r
# determine the no of rows with NAs
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
# replace all NAs in steps with average steps
# NB not really sure how to improve on this just now
# first duplicate original dataset
activity.noNA <- activity
activity.noNA$steps[which(is.na(activity$steps))] <- mean(activity.noNA$steps, na.rm=TRUE)

# with this adjusted dataset now
# determine the total no of steps for each day
days.noNA <- group_by(activity.noNA, date)
dailysum.noNA <- summarise(days.noNA, sum(steps, na.rm=TRUE))
colnames(dailysum.noNA) <- c("date", "sumsteps")

# plot a histogram of the total no of steps per day and compare with original
par(mfcol = c(1, 2))
hist(dailysum$sumsteps, main="Original")
hist(dailysum.noNA$sumsteps, main="NAs Imputed")
```

![](PA1_template_files/figure-html/add missing values and redo calcs-1.png)<!-- -->

```r
# determine the mean and median of the total no of steps per day (original data containing NAs)
mean(dailysum.noNA$sumsteps)
```

```
## [1] 10766.19
```

```r
median(dailysum.noNA$sumsteps)
```

```
## [1] 10766.19
```

Its obvious that imputing the missing values has had a significant effect on the mean and the median, and in fact has made them equal.

## Are there differences in activity patterns between weekdays and weekends?

To do this we split the dataset containing the imputed values into two separate datasets based on them being weekdays or weekends. Two timeseries plots are then plotted alongside each other so that weekdays and weekends can be compared.


```r
# create factor for weekday (True/False)
activity.noNA <- mutate(activity.noNA, weekday = !weekdays(as.POSIXlt(date,'%Y-%m-%d')) %in% c("Saturday", "Sunday"))

# split into two datasets - weekdays and weekends
activity.weekdays <- filter(activity.noNA, activity.noNA$weekday==TRUE)
activity.weekends <- filter(activity.noNA, activity.noNA$weekday==FALSE)

# determine average of each interval
intervals.weekdays <- group_by(activity.weekdays, interval)
intervalsavg.weekdays <- summarise(intervals.weekdays, mean(steps, na.rm=TRUE))
colnames(intervalsavg.weekdays) <- c("interval", "averagesteps")

intervals.weekends <- group_by(activity.weekends, interval)
intervalsavg.weekends <- summarise(intervals.weekends, mean(steps, na.rm=TRUE))
colnames(intervalsavg.weekends) <- c("interval", "averagesteps")

# plot two graphs in rows, weekdays then weekends
par(mfcol = c(2, 1))
plot(intervalsavg.weekdays$interval, intervalsavg.weekdays$averagesteps, type="l", xlab="Interval", ylab="Average No of steps", main="Weekdays")
plot(intervalsavg.weekends$interval, intervalsavg.weekends$averagesteps, type="l", xlab="Interval", ylab="Average No of steps", main="Weekends")
```

![](PA1_template_files/figure-html/weekdays-1.png)<!-- -->

We can see that the activity over the weekends is spread out more across the day.
