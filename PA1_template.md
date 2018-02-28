knit2html(
---
title: "Reproducible Research Project"
output: html_document
---


# loading and prepocessing the data

```r
# download file from web
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip", mode="wb")
# unzip data and read 
unzip("activity.zip")
stepdata <- read.csv("activity.csv", header = TRUE)
head(stepdata)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```





# Find mean total number of steps each day.


### Calculate the total number of steps taken each day

```r
dailysteps <- aggregate(steps ~ date, data = stepdata, sum)
summary(dailysteps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```


### Make a Histogram of the total number of steps taken each day

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.2
```

```r
dailystepplot <- ggplot(data =  dailysteps, aes(x= date, y= steps))  +            
    geom_bar(stat="identity", position="dodge") + 
    ggtitle("Total Number of Steps Per Day") +
    theme(axis.text.x=element_blank()) +
    xlab("Day") +   
    ylab("Steps") 
dailystepplot
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)


### Calculate and report the mean and median of the total number of steps taken per day

```r
mean(dailysteps$steps) 
```

```
## [1] 10766.19
```

```r
median(dailysteps$steps) 
```

```
## [1] 10765
```





# What are the daily activity patterns?


### Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days 

```r
steptime <- aggregate(steps ~ interval, data = stepdata, mean)

qplot(interval, steps, data = steptime) + geom_line() +
    ggtitle("Mean Number of Steps Per 5 Minute Interval") 
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)


### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
tail(steptime[order(steptime$steps),])
```

```
##     interval    steps
## 101      820 171.1509
## 103      830 177.3019
## 106      845 179.5660
## 107      850 183.3962
## 105      840 195.9245
## 104      835 206.1698
```

```r
# interval 835 has greatest amount of steps (206.2)
```





# Imputing missing values
#### Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.


### Calculate and report the total number of missing values in the dataset 

```r
totaldata <- read.csv("activity.csv", header = TRUE, na.strings = "NA")
valuesonly <-na.omit(totaldata)
nrow(valuesonly) - nrow(totaldata)
```

```
## [1] -2304
```

```r
# 2304 missing values in the dataset.
```


### Devise a strategy for filling in all of the missing values in the dataset. 

### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
meansteps <- aggregate(steps ~ interval, data = stepdata, mean)
cleandata <- totaldata 
for (i in 1:nrow(cleandata)) {
    if (is.na(cleandata$steps[i])) {
        cleandata$steps[i] <- meansteps[which(cleandata$interval[i] == meansteps$interval), ]$steps
    }
}

head(cleandata)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
## no longer see NA values in head in correspondin intervals (see first table)
```


### Make a histogram of the total number of steps taken each day.

```r
cleandailysteps <- aggregate(steps ~ date, data = cleandata, sum)

cleandailyplot <- ggplot(data = cleandailysteps, aes(x= date, y= steps))  +         
    geom_bar(stat="identity", position="dodge") + 
    ggtitle("Total Number of Steps Per Day, Filled in Values.") +
    theme(axis.text.x=element_blank()) +
    xlab("Day") +   
    ylab("Steps") 
cleandailyplot
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)


### Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
mean(cleandailysteps$steps) #Mean steps per = 10766
```

```
## [1] 10766.19
```

```r
median(cleandailysteps$steps) #Median steps per day = 10766
```

```
## [1] 10766.19
```
#### These values do not have any real significant differences from values of first part of assignment.
#### Imputing missing data did not impact the minimum or maximum steps per day, but it did have an impact on it's distribution.
##### The imputed data has quartile values closer to the mean, as you would suspect following the law of large numbers (imputing created a larger dataset)

### Imputed data

```r
summary(cleandailysteps$steps) 
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```


### Original data with missing values

```r
summary(dailysteps$steps) 
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```





# Are there differences in activity patterns between weekdays and weekends?


### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
cleandata$date <- as.Date(cleandata$date, format = "%Y-%m-%d")

cleandata$day <- factor(weekdays(cleandata$date))

levels(cleandata$day) <- list(weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                              weekend = c("Saturday", "Sunday"))
head(cleandata)
```

```
##       steps       date interval     day
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

```r
table(cleandata$day)
```

```
## 
## weekday weekend 
##   12960    4608
```


### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.4.2
```

```r
steptime <- aggregate(steps ~ interval + day, data = cleandata, mean)

xyplot(steptime$steps ~ steptime$interval | steptime$day, 
    layout = c(1, 2), type = "l", main = "Average daily steps, Weekend VS. Weekday (imputed data)",
    xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)
)
