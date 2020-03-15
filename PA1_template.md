---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
data <- read.csv("activity.csv")  
```
## What is mean total number of steps taken per day?


```r
per_day_steps <- aggregate(steps ~ date, data, sum)

plot1 <- hist(per_day_steps$steps, main = paste("Total Steps per Day"), col="green",xlab="Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
plot1
```

```
## $breaks
## [1]     0  5000 10000 15000 20000 25000
## 
## $counts
## [1]  5 12 28  6  2
## 
## $density
## [1] 1.886792e-05 4.528302e-05 1.056604e-04 2.264151e-05 7.547170e-06
## 
## $mids
## [1]  2500  7500 12500 17500 22500
## 
## $xname
## [1] "per_day_steps$steps"
## 
## $equidist
## [1] TRUE
## 
## attr(,"class")
## [1] "histogram"
```

```r
total_mean <- mean(per_day_steps$steps)
total_mean
```

```
## [1] 10766.19
```

```r
steps_median <- median(per_day_steps$steps)
steps_median
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
steps_for_intv <- aggregate(steps ~ interval, data, mean)

plot2 <- plot(steps_for_intv$interval,steps_for_intv$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average No. of Steps per Day (by Interval)")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
plot2
```

```
## NULL
```

```r
max_int <- steps_for_intv[which.max(steps_for_intv$steps),1]

max_int
```

```
## [1] 835
```

## Imputing missing values


```r
Total_missing <- sum(!complete.cases(data))
Total_missing
```

```
## [1] 2304
```

```r
Av_steps <- aggregate(steps ~ interval, data = data, FUN = mean)

fillNA <- numeric()

for (i in 1:nrow(data)) {
    obs <- data[i, ]
    if (is.na(obs$steps)) {
        steps <- subset(Av_steps, interval == obs$interval)$steps
    } else {
        steps <- obs$steps
    }
    fillNA <- c(fillNA, steps)
}

#Create a new d̥ataset with the missing values

new_dset<- data

new_dset$steps <- fillNA

##histogram of the total number of steps
Total_steps <- aggregate(steps ~ date, data = new_dset, sum, na.rm = TRUE)

hist(Total_steps$steps, main = paste("Total Steps per Day"), col="blue", xlab="No. of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

#Create Histogram to show difference. 


```r
hist(per_day_steps$steps, main = paste("Total Steps per Day"), col="green", xlab="No. of Steps")

legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "green"), lwd=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Mean and median


```r
total_mean <- mean(Total_steps$steps)
total_mean
```

```
## [1] 10766.19
```

```r
total_median <- median(Total_steps$steps)
total_median
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?


```r
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
              
new_dset$dow = as.factor(ifelse(is.element(weekdays(as.Date(new_dset$date)),days), "Weekday", "Weekend"))

Total_steps <- aggregate(steps ~ interval + dow, new_dset, mean)

library(lattice)

xyplot(Total_steps$steps ~ Total_steps$interval|Total_steps$dow, main="Av. Steps per Day (by Interval)",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
