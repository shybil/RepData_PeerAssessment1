Peer Assessment Project 1
===========================
##### Read Data

```r
data <- read.csv(unzip("activity.zip","activity.csv"))
# Tidy the date up
#
data$date<-as.Date(data$date, format = '%Y-%m-%d')
#
# Create a datastep with the NA omitted for future use
withOutNAData <- na.omit(data)
```
#### Q1. What is mean total number of steps taken per day?

```r
stepsPerDay <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
colors = c("red", "yellow", "green", "violet", "orange") 
hist(stepsPerDay, main = "Histogram Total Number of Steps Taken Each Day", 
     right = FALSE,     
     xlab = 'Number of Steps', 
     col=colors     # set the color palette 
     )
abline(v=mean(stepsPerDay), lty=2, col="blue")
abline(v=median(stepsPerDay), lty=2, col="brown4")
text(mean(stepsPerDay),26,labels="Mean", pos=1, col="blue")
text(median(stepsPerDay),22,labels="Median", pos=1, col="brown4")
```

![](./PA1_template_files/figure-html/Q1-1.png) 

```r
Q1dmean <- mean(stepsPerDay, na.rm=TRUE)
Q1dmedian <- median(stepsPerDay, na.rm=TRUE)
```

##### The mean is  9354.2295 and the median is  10395.

#### Q2. What is the average daily activity pattern?

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.2
```

```r
averages <- aggregate(x=list(steps=withOutNAData$steps), by=list(interval = as.numeric(as.character(withOutNAData$interval))),  FUN=mean)
plot(averages, type="l", main="Time Series",col="blue",col.axis="red",col.lab="darkslateblue",
    xlab("5-minute interval"),
    ylab("Avg # of steps taken")         
)
```

![](./PA1_template_files/figure-html/Q2-1.png) 

```r
maxInterval<-averages[averages$steps == max(averages$steps), ]
```
##### The 5-minute interval  835 has the max # of steps of 206 

#### Q3. Imputing missing values
Calculate and report the total number of missing values in the dataset

```r
misSteps <- sum(is.na(data$steps))
```
##### The number of missing values is 2304.

I'm replacing each missing value with the mean value of its 5-minute interval.


```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.1.2
```

```r
#
# Create a new data set with imputed data
#
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
newData <- ddply(data, ~ interval, transform, steps = impute.mean(steps))
newData<- newData[order(newData$date),]
imputeStepsPerDay <- tapply(newData$steps, newData$date, FUN=sum, na.rm=TRUE)
hist(imputeStepsPerDay, main = "Histogram Total Number of Steps Taken Each Day", 
     right = FALSE,     
     xlab = 'Number of Steps', 
     col=colors     # set the color palette 
     )
abline(v=mean(imputeStepsPerDay), lty=2, col="blue")
abline(v=median(imputeStepsPerDay), lty=2, col="brown4")
text(mean(imputeStepsPerDay),26,labels="Mean", pos=1, col="blue")
text(median(imputeStepsPerDay),22,labels="Median", pos=1, col="brown4")
```

![](./PA1_template_files/figure-html/Q3-1-1.png) 

```r
imputedMean <- mean(imputeStepsPerDay, na.rm=TRUE)
imputedMedian <- median(imputeStepsPerDay, na.rm=TRUE)

meanDiff <- imputedMean - Q1dmean
medianDiff <- imputedMedian - Q1dmedian
stepDiff <- sum(newData$steps) - sum(withOutNAData$steps)
```

Non-imputed Mean  : `9354.2295`
Non-imputed Median: `10395`
Imputed Mean  : `10766.189`
Imputed Median: `10766.189`

The difference between the non-imputed mean and imputed mean is `1411.959171`
The difference between the non-imputed mean and imputed mean is `371.1886792`
The impact of imputing missing data on the total daily number of steps is a difference of `86129.509`

##### With the imputed values, the mean and median are the same

#### Q4. Are there differences in activity patterns between weekdays and weekends?

```r
library(ggplot2)
# Create a factor variable with two levels - weekday or weekend
dayType <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else 
        return("weekday")
   
}
newData$date <- as.Date(newData$date)
newData$day <- as.factor(sapply(newData$date, FUN=dayType))

averages <- aggregate(steps ~ interval + day, data=newData, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![](./PA1_template_files/figure-html/Q4-1.png) 

##### There are fewer steps per day in the weekday than the weekend on average
