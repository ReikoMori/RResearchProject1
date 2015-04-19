
#Set up workspace and clean up data 

rm(list = ls())
library(ggplot2)
library(plyr)

setwd("/Users/alexanderzempolich/Documents/AxelleR")

# Loading and pre-processing the data
unzip(zipfile="repdata-data-activity.zip")
data <- read.csv("activity.csv", sep=",", na.strings="NA", colClasses =c("numeric","Date","numeric"))


# A. What is the total number of steps taken per day
## A.1.Calculate the total number of steps taken per day
daily_steps <- aggregate(steps~date, data=data, FUN=sum, na.rm=T)

## A.2. Histogram of steps per day:
tot_steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(tot_steps, binwidth=1000, xlab="number of steps", ylab = "Frequency", main="Total number of daily steps",col="hotpink4")+ theme(legend.position="none")

## A.3. Mean and Median of total number of steps/day
mean_steps <- round(mean(daily_steps$steps), 2) 
#[1] 10766.19
median_steps <- quantile(x = daily_steps$steps, probs = 0.5) #10765 

# B. What is the average daily activity pattern
## B.1. Time series plot of the 5mnt interval and the average number of steps taken, averaged accross all days
### Find the means of steps for each interval
steps_int <-aggregate(data$steps ~ data$interval, FUN=mean, na.rm=T)
colnames(steps_int) <- c("Interval","Mean")

### Create time series plot from above calculations
plot(steps_int$Interval, steps_int$Mean, type="l", col="darkred", lwd=2, xlab="time interval", ylab="average number of steps", main="Time Series Plot")

##B.2. Find the interval with the maximum number of steps:
steps_int[which.max(steps_int$Mean),] 
#Interval 835, Mean 206.1698

#C. Inputing Missing Values
##C.1. Find total number of missing values in dataset
missing_values <- is.na(data$steps)
nbr_missing <-sum(as.numeric(missing_values))
nbr_missing 
# [1] 2304

##C.2. Devise a strategy for filling in missing values
### The insertion strategy will consist of replacing the missing value by the mean value of steps for its interval
     
##C.3. Fillin in missing values
### Create a new data set
data2 <- data 

### Again, find the means of steps for each interval
steps_int <- aggregate(data$steps ~ data$interval, data, FUN=mean, na.rm=T)
colnames(steps_int) <- c("Interval","Mean")

###Plug in the new values
int_mean <-tapply(data$steps, data$interval,mean, na.rm=TRUE)

for (i in which(is.na(data2)))
    {
    data2[i,1] <- int_mean[((i-1)%%288)+1]
    }

###Check that the number of NA = 0
missing_values2 <- is.na(data2$steps)
nbr_missing2 <-sum(as.numeric(missing_values2))
nbr_missing2
#[1] 0

##C.4. New histogram, mean, and median of total steps/day
###New Total of steps per day
daily_steps2 <- aggregate(steps~date, data=data2, FUN=sum, na.rm=T)

### new mean and median
mean_steps2 <- round(mean(daily_steps2$steps), 2)
#[1] 10766.19
median_steps2 <- quantile(x = daily_steps2$steps, probs = 0.5)
# 10766.19 
###New histogram 
tot_steps2 <- tapply(data2$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(tot_steps2, binwidth=1000, xlab="number of steps", ylab = "Frequency", main="Total number of daily steps, w/o NAs",col="hotpink4")+ theme(legend.position="none")

#D. Are there differences between weekdays and weekends?
##D.1. Create a new factor variable indicating day type
data2<- data.frame(date=data2$date, weekday=weekdays(data2$date),steps=data2$steps, interval=data2$interval)
data2<-cbind(data2,type_day=ifelse(data2$weekday=="Saturday"|data2$weekday=="Sunday","WE","WD"))

##D.2. Create a time series plot for weekends and weekdays
ave_type <- ddply(data2, .(interval,type_day), summarize, steps = mean(steps, na.rm=TRUE))
ggplot(data=ave_type, aes(x=interval, y=steps, group=type_day)) + geom_line(aes(color=type_day))+ facet_wrap(~ type_day, nrow=2)



