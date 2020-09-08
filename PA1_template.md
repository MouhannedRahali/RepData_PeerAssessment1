---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---
'''{r}

library(ggplot2)
library(data.table)
opts_chunk$set(echo = TRUE, results = 'hold')

'''
## Loading and preprocessing the data
'''{r include = TRUE}

if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
act <- read.csv("activity.csv",header = TRUE,sep = ",")
str(act)

act$date <- as.Date(act$date, format="%Y-%m-%d")
act$interval <- as.factor(act$interval)
str(act)

'''


## What is mean total number of steps taken per day?

'''{r}

steps_by_date <- aggregate(steps ~ date, data = act, FUN = sum)
colnames(steps_by_date) <- c("date", "steps")
hist(steps_by_date$steps,xlab = "steps per day", ylab = "frequency")

mean_step <- mean(act$steps,na.rm = TRUE)
mean_step
median_step <- median(act$steps,na.rm = TRUE)
median_step

'''

## What is the average daily activity pattern?
'''{r}

steps_by_interval <- aggregate(steps ~ interval,data = act, FUN = mean,na.rm = TRUE)
steps_by_interval$interval<-as.integer(levels(steps_by_interval$interval)[steps_by_interval$interval])
colnames(steps_by_interval) <- c("interval", "steps")

ggplot(steps_by_interval, aes(x = interval, y = steps)) + 
  geom_line(col = "blue", size = 1) + 
  labs(title = "Average Daily Activity Pattern", x = "Interval", y = "Steps")

maximum <- steps_by_interval[which.max(steps_by_interval$steps),]
maximum

'''
## Imputing missing values

'''{r}
missingV <- sum(is.na(act$steps))
missingV

enhanced_act <- act
pos_of_nas <- which(is.na(act$steps))
for (i in pos_of_na) {
  enhanced_act$steps[i] <- with(steps_by_interval, steps[interval,enhanced_act$interval[i]]) 
}

totalstepsperday <- aggregate(steps ~ date, data = enhanced_act, FUN = sum, na.rm = TRUE)
hist(totalstepsperday$steps, 
    main = "Total Steps per Day (no-NA)", 
    xlab = "Number of Steps per Day", 
    ylab = "Interval",
    col="green",
    breaks=50)
    
new_mean <- mean(totalstepsperday$steps)
new_mean
new_median <- median(totalstepsperday$steps)
new_median


'''

## Are there differences in activity patterns between weekdays and weekends?

'''{r}
dt <- data.table(enhanced_act)
dt[, weekday := ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")]
dt$weekday <- as.factor(dt$weekday)
dt$interval <- as.integer(levels(dt$interval)[dt$interval])

steps_per_weekday <- aggregate(steps ~ interval+weekday, data = dt, FUN = mean)
ggplot(steps_per_weekday, aes(x = interval, y = steps)) + 
  geom_line(col = "blue", size = 1) + 
  facet_wrap(~ weekday, nrow=2, ncol=1) + 
  labs(x = "Interval", y = "Number of Steps")


'''


