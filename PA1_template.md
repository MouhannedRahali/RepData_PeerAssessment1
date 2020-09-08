---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
library(ggplot2)
library(data.table)
knitr::opts_chunk$set(echo = TRUE,include = TRUE)
```
## Loading and preprocessing the data

```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
act <- read.csv("activity.csv",header = TRUE,sep = ",")
str(act)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
act$date <- as.Date(act$date, format="%Y-%m-%d")
act$interval <- as.factor(act$interval)
str(act)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```


## What is mean total number of steps taken per day?


```r
steps_by_date <- aggregate(steps ~ date, data = act, FUN = sum)
colnames(steps_by_date) <- c("date", "steps")
hist(steps_by_date$steps,xlab = "steps per day", ylab = "frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
mean_step <- mean(steps_by_date$steps,na.rm = TRUE)
print(mean_step)
```

```
## [1] 10766.19
```

```r
median_step <- median(steps_by_date$steps,na.rm = TRUE)
print(median_step)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
steps_by_interval <- aggregate(steps ~ interval,data = act, FUN = mean,na.rm = TRUE)
steps_by_interval$interval<-as.integer(levels(steps_by_interval$interval)[steps_by_interval$interval])
colnames(steps_by_interval) <- c("interval", "steps")
head(steps_by_interval, 10)
```

```
##    interval     steps
## 1         0 1.7169811
## 2         5 0.3396226
## 3        10 0.1320755
## 4        15 0.1509434
## 5        20 0.0754717
## 6        25 2.0943396
## 7        30 0.5283019
## 8        35 0.8679245
## 9        40 0.0000000
## 10       45 1.4716981
```

```r
ggplot(steps_by_interval, aes(x = interval, y = steps)) + 
  geom_line(col = "blue", size = 1) + 
  labs(title = "Average Daily Activity Pattern", x = "Interval", y = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
maximum <- steps_by_interval[which.max(steps_by_interval$steps),]
print(maximum)
```

```
##     interval    steps
## 104      835 206.1698
```
## Imputing missing values


```r
missingV <- sum(is.na(act$steps))
missingV
```

```
## [1] 2304
```

```r
enhanced_act <- act
pos_of_nas <- which(is.na(act$steps))
for (i in pos_of_nas) {
  enhanced_act$steps[i] <- with(steps_by_interval, steps[interval=enhanced_act$interval[i]]) 
}

totalstepsperday <- aggregate(steps ~ date, data = enhanced_act, FUN = sum, na.rm = TRUE)
hist(totalstepsperday$steps, 
    main = "Total Steps per Day (no-NA)", 
    xlab = "Number of Steps per Day", 
    ylab = "Interval",
    col="blue",
    breaks=50)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
new_mean <- mean(totalstepsperday$steps)
new_mean
```

```
## [1] 10766.19
```

```r
new_median <- median(totalstepsperday$steps)
new_median
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?


```r
dt <- data.table(enhanced_act)
dt[, weekday := ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")]
dt$weekday <- as.factor(dt$weekday)
dt$interval <- as.integer(levels(dt$interval)[dt$interval])

steps_per_weekday <- aggregate(steps ~ interval+weekday, data = dt, FUN = mean)
ggplot(steps_per_weekday, aes(x = interval, y = steps)) + 
  geom_line(col = "blue", size = 1) + 
  facet_wrap(~ weekday, nrow=2, ncol=1) + 
  labs(x = "Interval", y = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


