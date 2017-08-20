# Reproducible Research Assignement

## Loading and preprocessing the data

```r
setwd("D:\\Learning\\Coursera\\Data_Scientist\\Assignments\\Month5_Week2")
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zipfile="activity.zip"
download.file(fileURL, destfile=zipfile)
unzip(zipfile, exdir="D:\\Learning\\Coursera\\Data_Scientist\\Assignments\\Month5_Week2")
activitydf <- read.csv("activity.csv")
stepsperday <- with(activitydf,tapply(steps,date,sum,na.rm = T))
stepsperdaydf <- data.frame(date = names(stepsperday),totalsteps = stepsperday)


## Question - What is mean total number of steps taken per day?

## Below is the histogram for depicting the total no of steps taken each day
hist(as.numeric(stepsperdaydf$totalsteps),main="Total Steps Taken each day",xlab="Total No Of Steps Taken Per Day",col="red")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
## Mean of the total number of steps taken per Day
mean(stepsperdaydf$totalsteps)
```

```
## [1] 9354.23
```

```r
## Median of the total number of steps taken per Day
median(stepsperdaydf$totalsteps)
```

```
## [1] 10395
```

```r
## What is the average daily activity pattern?
# Average Number of Steps Taken across all Days
avgstepsperint <- aggregate(steps ~ interval, activitydf, mean)
plot(avgstepsperint$interval,avgstepsperint$steps, type="l", main = "Average Steps Taken Per Interval ",ylab="Average Steps Taken", xlab="Interval") 
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

```r
# find row id of maximum average number of steps in an interval
max_ave_steps_row_id <- which.max(avgstepsperint$steps)

# Get the interval with maximum average number of steps in an interval
avgstepsperint [max_ave_steps_row_id, ]
```

```
##     interval    steps
## 104      835 206.1698
```

```r
## Imputing missing values
# Identification of missing rows
missing <- is.na(activitydf$steps)
# Number of Missing Rows
table(missing)
```

```
## missing
## FALSE  TRUE 
## 15264  2304
```

```r
# Replacing missing values with the mean of that 5 - minute interval from above derived data frame
for (i in 1:nrow(activitydf)){
  if (is.na(activitydf$steps[i])){
    interval_val <- activitydf$interval[i]
    row_id <- which(avgstepsperint$interval == interval_val)
    steps_val <- (avgstepsperint$steps[row_id])
    activitydf$steps[i] <- steps_val
  }
}

# aggregate steps as per date to get total number of steps in a day
stepsperdaydf_imp <- aggregate(steps ~ date, activitydf, sum)

# create histogram of total number of steps in a day
hist(stepsperdaydf_imp$steps, col=1, main="Histogram of total number of steps per day (Imputed)", xlab="Total number of steps in a day")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)

```r
# Mean - After replacing NAs
mean(stepsperdaydf_imp$steps)
```

```
## [1] 10766.19
```

```r
# Median - After replacing NAs
median(stepsperdaydf_imp$steps)
```

```
## [1] 10766.19
```

```r
# Mean - Original Data 
mean(stepsperdaydf$totalsteps)
```

```
## [1] 9354.23
```

```r
# Median - Original Data
median(stepsperdaydf$totalsteps)
```

```
## [1] 10395
```

## The Mean and Median values are higher after imputing the values. This is due to imputation of mean values in the place of missing values


```r
## Question - Are there differences in activity patterns between weekdays and weekends?
## Adding additional variable to identify day of a date is weekday or weekend
activitydf$date = as.Date(activitydf$date, format = '%Y-%m-%d') 

for (i in 1:nrow(activitydf)){
  if ( weekdays(activitydf$date[i]) == "Saturday" | weekdays(activitydf$date[i]) == "Sunday") {
	activitydf$day[i] = "Weekend"
	}	
  else{
	activitydf$day[i] = "Weekday"
   }	
}

# Deriving average of steps on interval and day type
avg_step_imp <- aggregate(steps ~ interval + day, activitydf, mean)
# Loading lattice library
library(lattice)
# Plotting Graph
xyplot(steps ~ interval | day, data = avg_step_imp, type = "l", lwd = 2,
       layout = c(1, 2), 
       xlab = "5-minute interval", 
       ylab = "Average number of steps",
       main = "Average Number of Steps Taken (across all weekday days or weekend days)")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)
