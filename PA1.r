## Download data from Coursera Peer Assessment 1 webpage.

## Loading necessary packages"
library("ggplot2")

## Create a variable 'data' and assign the unzipped file to it using read.table()
setwd("C:/Users/Satvik Gadamsetty/Documents/Classwork/Programming/R/RR")
data <- read.table('activity.csv',sep = ",",header = TRUE, na.strings ="NA",
                   colClasses = c('integer','Date','factor'))

## Removing rows with NA
new.data <- na.omit(data)

total.steps <- tapply(new.data$steps, new.data$date, FUN = sum)

## Mean total number of steps taken per day
plot1 <- ggplot(new.data, aes(date, steps)) + geom_bar(stat = "identity",binwidth = .5) +
        labs(title = "Histogram of Total Number of Steps Taken Each Day",x = "Date", y = "Total Number of Steps")
print(plot1)

mean1 <- mean(total.steps)
median1 <- median(total.steps)

## Average daily activity pattern
averages <- aggregate(new.data$steps, list(interval = as.numeric(as.character(new.data$interval))), FUN = "mean")
names(averages)[2] <- "Avg.Steps"

plot2 <- ggplot(averages, aes(interval, Avg.Steps)) + geom_line(color = "green", size = 0.7) + labs(title = "Time Series Plot of the 5-minute Intervals", x = "5-minute intervals", y = "Average Number of Steps Taken")
print(plot2)

## 5-minute interval containing most steps
averages[averages$Avg.Steps == max(averages$Avg.Steps),]

## Number of missing values
sum(!complete.cases(data))

## Impute missing values
impData <- data 
for (i in 1:nrow(impData)) {
    if (is.na(impData$steps[i])) {
        impData$steps[i] <- averages[which(impData$interval[i] == averages$interval), ]$Avg.Steps
    }
}

head(impData); sum(!complete.cases(impData))

## Graph total number of steps taken each data for imputed data.
plot3 <- ggplot(impData, aes(date, steps)) + geom_bar(stat = "identity",binwidth = .5) +
        labs(title = "Histogram of Total Number of Steps Taken Each Day (Imputed Data)",x = "Date", y = "Total Number of Steps")
print(plot3)

total.steps.impute <- tapply(impData$steps, impData$date, FUN = sum)

mean2 <- mean(total.steps.impute)
median2 <- median(total.steps.impute) ## Comparing means and medians reveals that both new means are the same while the new median is greater than the old median.

## Creating factor variables indicating whether the date is a weekday or weekend.
impData$weekdays <- factor(format(impData$date, "%A"))
levels(impData$weekdays)
levels(impData$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(impData$weekdays)
table(impData$weekdays)

## Graphing panel plot containing a time series plot 
new.averages <- aggregate(impData$steps, 
                      list(interval = as.numeric(as.character(impData$interval)), 
                           weekdays = impData$weekdays),
                      FUN = "mean")
names(new.averages)[3] <- "meanOfSteps"
library(lattice)
plot4 <- xyplot(new.averages$meanOfSteps ~ new.averages$interval | new.averages$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
print(plot4)