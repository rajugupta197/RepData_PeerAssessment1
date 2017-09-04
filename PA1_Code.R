############### Course Project 1 Week-2 of Reproducible Research ############

#### Activity Monitoring (Steps) Data

### Downloading the Data File

setwd("D:\\Coursera\\Data Science - Specialization\\05_Reproducible Research\\RCode\\AssignmentWeek-2")

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

f <- "activity.zip"
if(!file.exists(f)) {
    download.file(url=url,destfile = f, mode = "wb")
}

f1 <- "activity.csv"
if(!file.exists(f1)) {
    unzip(f)
}

###############################################################
#### Read the Data File ********
if(!exists("activity")){
    activity <- read.csv2(file = f1, header = T, sep = ",")
}

#########################
## Calculate the total number of steps taken per day
stepsperday <- aggregate(activity$steps, list(activity$date), FUN=sum, na.rm = T)

names(stepsperday) <- c(names(activity)[2], names(activity)[1])

## histogram of the total number of steps taken each day
png(filename = "1_HistogramBeforeImputing.png")
hist(stepsperday$steps, breaks = 22, ylim = c(0, 20), xlim = c(0,22000), col = rep(2:7), xlab = "Steps Per Day", main = "Histogram of Total Steps taken each Day")
dev.off()
## mean and median of the total number of steps taken per day
MeanStepsPerDay <- mean(stepsperday$steps)
MedianStepsPerDay <- median(stepsperday$steps)
#########################

## For each 5-minute interval calculating the average number of steps taken, averaged across all days
AvgStepsPerInterval <- aggregate(activity$steps, list(activity$interval), FUN=mean, na.rm = T)
names(AvgStepsPerInterval) <- c(names(activity)[3], "AvgSteps")

png(filename = "2_AvgStepsPerIntervalBeforeImputing.png")
op <- par(mfrow = c(1,1))
plot(AvgStepsPerInterval$interval, AvgStepsPerInterval$AvgSteps, type = "l", lwd = 2, main = "Average Number of Steps Per 5-Minute Interval", xlab = "Interval", ylab = "Steps")
par(op)
dev.off()

## Finding the 5-minute interval having maximum number of average steps
MaxInterval <- AvgStepsPerInterval[which.max(AvgStepsPerInterval$AvgSteps),]

#############################

# Total number of missing values in the dataset (i.e. the total number of rows with NAs)
TotalRowsWithNAs <- sum(!complete.cases(activity))

# filling in missing values by choosing the mean for that 5-minute interval
NACases <- !complete.cases(activity)

AvgSteps <- rep(round(AvgStepsPerInterval$AvgSteps),length(levels(activity$date)))

# Creating a new dataset that is equal to the original dataset but with the missing data filled in
activityNew <- activity
activityNew[NACases,][1] <- AvgSteps[NACases]

## Calculate the total number of steps taken per day
stepsperdayNew <- aggregate(activityNew$steps, list(activityNew$date), FUN=sum)

names(stepsperdayNew) <- c(names(activityNew)[2], names(activityNew)[1])

## histogram of the total number of steps taken each day
png(filename = "3_HistogramAfterImputing.png")
hist(stepsperdayNew$steps, breaks = 22, ylim = c(0, 20), xlim = c(0,22000), col = rep(2:7))
dev.off()

## mean and median of the total number of steps taken per day
MeanStepsPerDayNew <- mean(stepsperdayNew$steps)
MedianStepsPerDayNew <- median(stepsperdayNew$steps)

## Create a new factor variable in the dataset with two levels
## -- "weekday" and "weekend"
wkday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") 

for(i in 1:nrow(activityNew)){
    activityNew$day[i] <- if(weekdays(as.Date(activityNew$date[i])) %in% wkday)
        "weekday" 
    else 
        "weekend"
}
activityNew$day <- as.factor(as.character(activityNew$day))

## Make a panel plot containing a time series plot (i.e. type = "l") 
## of the 5-minute interval (x-axis) and the average number of steps 
## taken, averaged across all weekday days or weekend days (y-axis). 

AvgStepsPerIntervalNew <- aggregate(activityNew$steps, list(activityNew$interval, activityNew$day), FUN=mean)
names(AvgStepsPerIntervalNew) <- c(names(activityNew)[3], names(activityNew)[4], "AvgSteps")

library(ggplot2)
png(filename = "4_AvgStepsPerIntervalAfterImputing.png")
ggplot(AvgStepsPerIntervalNew, aes(x = interval, y=AvgSteps, color=day)) +
    geom_line() +
    facet_grid(day ~ .) +
    labs(title = "Average Number of Steps Per 5-Minute Interval after Imputation", x = "interval", y = "steps")
dev.off()