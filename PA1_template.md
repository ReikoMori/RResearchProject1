---
title: "Reproducibility Project"
author: "axelle.cb"
date: "Sunday, April 19, 2015"
output: html_document
---
**INTRODUCTION**
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

**Loading and preprocessing the data**
 - Load the data 
 - Process/transform the data (if necessary) into a format suitable for your analysis

```r
#Set up workspace and clean up data 
rm(list = ls())
library(ggplot2)
library(plyr)
setwd("C:/Users/Axelle/Documents/R/RD Project 1")

# Loading and pre-processing the data
unzip(zipfile="repdata-data-activity.zip")
```

```
## Warning in unzip(zipfile = "repdata-data-activity.zip"): error 1 in
## extracting from zip file
```

```r
data <- read.csv("activity.csv", sep=",", na.strings="NA", colClasses =c("numeric","Date","numeric"))
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

**What is mean total number of steps taken per day?**
- Calculate the total number of steps taken per day
- Make a histogram of the total number of steps taken each day
- Calculate and report the mean and median of the total number of steps taken per day

```r
# A. What is the total number of steps taken per day
## A.1.Calculate the total number of steps taken per day
daily_steps <- aggregate(steps~date, data=data, FUN=sum, na.rm=T)
```

```
## Error in terms.formula(formula, data = data): 'data' argument is of the wrong type
```

```r
## A.2. Histogram of steps per day:
tot_steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
```

```
## Error in data$date: object of type 'closure' is not subsettable
```

```r
qplot(tot_steps, binwidth=1000, xlab="number of steps", ylab = "Frequency", main="Total number of daily steps",col="hotpink4")+ theme(legend.position="none")
```

```
## Error in eval(expr, envir, enclos): object 'tot_steps' not found
```

```r
## A.3. Mean and Median of total number of steps/day
mean_steps <- round(mean(daily_steps$steps), 2) #[1] 10766.19
```

```
## Error in mean(daily_steps$steps): object 'daily_steps' not found
```

```r
median_steps <- quantile(x = daily_steps$steps, probs = 0.5) #10765 
```

```
## Error in quantile(x = daily_steps$steps, probs = 0.5): object 'daily_steps' not found
```

**What is the average daily activity pattern?**
- Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
# B. What is the average daily activity pattern
## B.1. Time series plot of the 5mnt interval and the average number of steps taken, averaged accross all days
### Find the means of steps for each interval
steps_int <-aggregate(data$steps ~ data$interval, FUN=mean, na.rm=T)
```

```
## Error in data$steps: object of type 'closure' is not subsettable
```

```r
colnames(steps_int) <- c("Interval","Mean")
```

```
## Error in colnames(steps_int) <- c("Interval", "Mean"): object 'steps_int' not found
```

```r
### Create time series plot from above calculations
plot(steps_int$Interval, steps_int$Mean, type="l", col="darkred", lwd=2, xlab="time interval", ylab="average number of steps", main="Time Series Plot")
```

```
## Error in plot(steps_int$Interval, steps_int$Mean, type = "l", col = "darkred", : object 'steps_int' not found
```

```r
##B.2. Find the interval with the maximum number of steps:
steps_int[which.max(steps_int$Mean),] #Interval 835, Mean 206.1698
```

```
## Error in eval(expr, envir, enclos): object 'steps_int' not found
```

**Imputing missing values**
-Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
- Create a new dataset that is equal to the original dataset but with the missing data filled in.
- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
#C. Inputing Missing Values
##C.1. Find total number of missing values in dataset
missing_values <- is.na(data$steps)
```

```
## Error in data$steps: object of type 'closure' is not subsettable
```

```r
nbr_missing <-sum(as.numeric(missing_values))
```

```
## Error in eval(expr, envir, enclos): object 'missing_values' not found
```

```r
nbr_missing # [1] 2304
```

```
## Error in eval(expr, envir, enclos): object 'nbr_missing' not found
```

```r
##C.2. Devise a strategy for filling in missing values
### The insertion strategy will consist of replacing the missing value by the mean value of steps for its interval
     
##C.3. Fillin in missing values
### Create a new data set
data2 <- data 

### Again, find the means of steps for each interval
steps_int <- aggregate(data$steps ~ data$interval, data, FUN=mean, na.rm=T)
```

```
## Error in terms.formula(formula, data = data): 'data' argument is of the wrong type
```

```r
colnames(steps_int) <- c("Interval","Mean")
```

```
## Error in colnames(steps_int) <- c("Interval", "Mean"): object 'steps_int' not found
```

```r
###Plug in the new values
int_mean <-tapply(data$steps, data$interval,mean, na.rm=TRUE)
```

```
## Error in data$interval: object of type 'closure' is not subsettable
```

```r
for (i in which(is.na(data2)))
    {
    data2[i,1] <- int_mean[((i-1)%%288)+1]
    }
```

```
## Warning in is.na(data2): is.na() applied to non-(list or vector) of type
## 'closure'
```

```r
###Check that the number of NA = 0
missing_values2 <- is.na(data2$steps)
```

```
## Error in data2$steps: object of type 'closure' is not subsettable
```

```r
nbr_missing2 <-sum(as.numeric(missing_values2))
```

```
## Error in eval(expr, envir, enclos): object 'missing_values2' not found
```

```r
nbr_missing2
```

```
## Error in eval(expr, envir, enclos): object 'nbr_missing2' not found
```

```r
#[1] 0

##C.4. New histogram, mean, and median of total steps/day
###New Total of steps per day
daily_steps2 <- aggregate(steps~date, data=data2, FUN=sum, na.rm=T)
```

```
## Error in terms.formula(formula, data = data): 'data' argument is of the wrong type
```

```r
### new mean and median
mean_steps2 <- round(mean(daily_steps2$steps), 2)#[1] 10766.19
```

```
## Error in mean(daily_steps2$steps): object 'daily_steps2' not found
```

```r
median_steps2 <- quantile(x = daily_steps2$steps, probs = 0.5)# 10766.19 
```

```
## Error in quantile(x = daily_steps2$steps, probs = 0.5): object 'daily_steps2' not found
```

```r
###New histogram 
tot_steps2 <- tapply(data2$steps, data$date, FUN=sum, na.rm=TRUE)
```

```
## Error in data$date: object of type 'closure' is not subsettable
```

```r
qplot(tot_steps2, binwidth=1000, xlab="number of steps", ylab = "Frequency", main="Total number of daily steps, w/o NAs",col="hotpink4")+ theme(legend.position="none")
```

```
## Error in eval(expr, envir, enclos): object 'tot_steps2' not found
```

**Are there differences in activity patterns between weekdays and weekends?**
-Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
-Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
#D. Are there differences between weekdays and weekends?
##D.1. Create a new factor variable indicating day type
data2<- data.frame(date=data2$date, weekday=weekdays(data2$date),steps=data2$steps, interval=data2$interval)
```

```
## Error in data2$date: object of type 'closure' is not subsettable
```

```r
data2<-cbind(data2,type_day=ifelse(data2$weekday=="Saturday"|data2$weekday=="Sunday","WE","WD"))
```

```
## Error in data2$weekday: object of type 'closure' is not subsettable
```

```r
##D.2. Create a time series plot for weekends and weekdays
ave_type <- ddply(data2, .(interval,type_day), summarize, steps = mean(steps, na.rm=TRUE))
```

```
## Error in if (empty(.data)) return(.data): missing value where TRUE/FALSE needed
```

```r
ggplot(data=ave_type, aes(x=interval, y=steps, group=type_day)) + geom_line(aes(color=type_day))+ facet_wrap(~ type_day, nrow=2)
```

```
## Error in ggplot(data = ave_type, aes(x = interval, y = steps, group = type_day)): object 'ave_type' not found
```



