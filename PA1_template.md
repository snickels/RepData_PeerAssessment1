# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

```r
raw.data <- read.csv("activity.csv")
raw.data$date <- as.Date(raw.data$date)
```


## What is mean total number of steps taken per day?

```r
hist(tapply(raw.data$steps, raw.data$date, sum, na.rm = TRUE), main = "Steps per day", 
    ylab = "frequency", xlab = "steps")
```

![plot of chunk first crude analysis](figure/first_crude_analysis.png) 

```r
mean(tapply(raw.data$steps, raw.data$date, sum, na.rm = TRUE))
```

```
## [1] 9354
```

```r
median(tapply(raw.data$steps, raw.data$date, sum, na.rm = TRUE))
```

```
## [1] 10395
```


## What is the average daily activity pattern?

```r
plot(tapply(raw.data$steps, raw.data$interval, mean, na.rm = TRUE), type = "l", 
    main = "Average daily activity pattern", ylab = "n steps (mean)", xlab = "interval")
```

![plot of chunk daily activity](figure/daily_activity.png) 


interval with maximum number of steps:

```r
average <- tapply(raw.data$steps, raw.data$interval, mean, na.rm = TRUE)
average[average == max(average)]
```

```
##   835 
## 206.2
```



## Imputing missing values

# Total number of rows with NAs:

```r
nrow(raw.data) - sum(complete.cases(raw.data))
```

```
## [1] 2304
```


# Replacing missing values with mean per interval

```r

library("reshape2")

molten.data <- melt(raw.data, id = c("interval", "date"))

mean.data <- dcast(molten.data, interval ~ variable, mean, na.rm = TRUE)

impu.data <- merge(raw.data, mean.data, by.x = "interval", by.y = "interval")

impu.data$steps <- impu.data$steps.x
impu.data$steps[is.na(impu.data$steps)] <- impu.data$steps.y[is.na(impu.data$steps)]
colnames(impu.data)[2] <- "rawsteps"
colnames(impu.data)[4] <- "meansteps"
```



# Steps per day:

```r
hist(tapply(impu.data$steps, raw.data$date, sum, na.rm = TRUE), main = "Steps per day (missings imputed)", 
    ylab = "frequency", xlab = "steps")
```

![plot of chunk daily steps imputed](figure/daily_steps_imputed.png) 

After imputation, the number of steps per day was considerably increased, and
the distribution pattern changed to a right-skewed distribution.

# Comparing mean and median (complete case vs. imputed analysis):

```r
mean(tapply(impu.data$rawsteps, impu.data$date, sum, na.rm = TRUE))
```

```
## [1] 9354
```

```r
mean(tapply(impu.data$steps, impu.data$date, sum, na.rm = TRUE))
```

```
## [1] 10766
```

```r

median(tapply(impu.data$rawsteps, impu.data$date, sum, na.rm = TRUE))
```

```
## [1] 10395
```

```r
median(tapply(impu.data$steps, impu.data$date, sum, na.rm = TRUE))
```

```
## [1] 10766
```

Mean and median are increased roughly by 1000 steps after imputation. 


## Are there differences in activity patterns between weekdays and weekends?


```r
impu.data$weekend <- weekdays(impu.data$date) == "Saturday" | weekdays(impu.data$date) == 
    "Sunday"

impu.data$weekday[impu.data$weekend] <- "weekend"
impu.data$weekday[!impu.data$weekend] <- "weekday"
```



```r
par(mfrow = c(2, 1))
plot(tapply(raw.data$steps[impu.data$weekday == "weekday"], raw.data$interval[impu.data$weekday == 
    "weekday"], mean, na.rm = TRUE), type = "l", main = "Average daily activity pattern (weekdays)", 
    ylab = "n steps (mean)", xlab = "interval")
plot(tapply(raw.data$steps[impu.data$weekday == "weekend"], raw.data$interval[impu.data$weekday == 
    "weekend"], mean, na.rm = TRUE), type = "l", main = "Average daily activity pattern (weekend)", 
    ylab = "n steps (mean)", xlab = "interval")
```

![plot of chunk weekend vs weekday](figure/weekend_vs_weekday.png) 

