# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
if(!file.exists("activity.csv")) {
    
    url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip";
    download.file(url, "activity.zip");
    unzip("activity.zip");
}

dataset = read.csv("activity.csv");

# construct a POSIXlt time column
dataset$time = as.POSIXlt(paste(dataset$date, " ", dataset$interval %/% 100, "-", dataset$interval %% 100, "-", 0, sep = ""), format = "%Y-%m-%d %H-%M-%S", tz = "GMT");

# cast column to a Date datatype
dataset$date = as.Date(dataset$date, format = "%Y-%m-%d");
```
## What is mean total number of steps taken per day?


```r
# calculate daily sum of steps
tmp = sapply(split(dataset$steps, dataset$date), sum, na.rm = TRUE);

hist(tmp, freq = T, xlab = "Total number of steps", ylab = "Days frequevecy", main = "Histogram of total number of steps");
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# removing temporary
rm("tmp");

# calculate daily mean of steps
tmp = sapply(split(dataset$steps, dataset$date), mean, na.rm = TRUE);
# calculate daily median of steps
tmp1 = sapply(split(dataset$steps, dataset$date), median, na.rm = TRUE);

cnames = names(tmp);
t = as.data.frame(cbind(cnames, unname(tmp), unname(tmp1)));

names(t) = c("Day", "Mean", "Median");

print(t);
```

```
##           Day              Mean Median
## 1  2012-10-01               NaN   <NA>
## 2  2012-10-02            0.4375      0
## 3  2012-10-03  39.4166666666667      0
## 4  2012-10-04  42.0694444444444      0
## 5  2012-10-05  46.1597222222222      0
## 6  2012-10-06  53.5416666666667      0
## 7  2012-10-07  38.2465277777778      0
## 8  2012-10-08               NaN   <NA>
## 9  2012-10-09  44.4826388888889      0
## 10 2012-10-10            34.375      0
## 11 2012-10-11  35.7777777777778      0
## 12 2012-10-12  60.3541666666667      0
## 13 2012-10-13  43.1458333333333      0
## 14 2012-10-14  52.4236111111111      0
## 15 2012-10-15  35.2048611111111      0
## 16 2012-10-16            52.375      0
## 17 2012-10-17  46.7083333333333      0
## 18 2012-10-18  34.9166666666667      0
## 19 2012-10-19  41.0729166666667      0
## 20 2012-10-20          36.09375      0
## 21 2012-10-21  30.6284722222222      0
## 22 2012-10-22  46.7361111111111      0
## 23 2012-10-23  30.9652777777778      0
## 24 2012-10-24  29.0104166666667      0
## 25 2012-10-25  8.65277777777778      0
## 26 2012-10-26  23.5347222222222      0
## 27 2012-10-27  35.1354166666667      0
## 28 2012-10-28  39.7847222222222      0
## 29 2012-10-29  17.4236111111111      0
## 30 2012-10-30          34.09375      0
## 31 2012-10-31  53.5208333333333      0
## 32 2012-11-01               NaN   <NA>
## 33 2012-11-02  36.8055555555556      0
## 34 2012-11-03  36.7048611111111      0
## 35 2012-11-04               NaN   <NA>
## 36 2012-11-05  36.2465277777778      0
## 37 2012-11-06           28.9375      0
## 38 2012-11-07  44.7326388888889      0
## 39 2012-11-08  11.1770833333333      0
## 40 2012-11-09               NaN   <NA>
## 41 2012-11-10               NaN   <NA>
## 42 2012-11-11  43.7777777777778      0
## 43 2012-11-12  37.3784722222222      0
## 44 2012-11-13  25.4722222222222      0
## 45 2012-11-14               NaN   <NA>
## 46 2012-11-15 0.142361111111111      0
## 47 2012-11-16  18.8923611111111      0
## 48 2012-11-17  49.7881944444444      0
## 49 2012-11-18  52.4652777777778      0
## 50 2012-11-19  30.6979166666667      0
## 51 2012-11-20  15.5277777777778      0
## 52 2012-11-21  44.3993055555556      0
## 53 2012-11-22  70.9270833333333      0
## 54 2012-11-23  73.5902777777778      0
## 55 2012-11-24  50.2708333333333      0
## 56 2012-11-25  41.0902777777778      0
## 57 2012-11-26  38.7569444444444      0
## 58 2012-11-27  47.3819444444444      0
## 59 2012-11-28  35.3576388888889      0
## 60 2012-11-29          24.46875      0
## 61 2012-11-30               NaN   <NA>
```

```r
# removing temporary
rm( list = c("tmp", "tmp1", "cnames", "t") );
```

## What is the average daily activity pattern?


```r
# calculate mean for interval
temp = tapply(dataset$steps, dataset$interval, mean, na.rm = TRUE);

cnames = as.integer(names(temp));

x = as.data.frame(cbind(cnames, unname(temp)));
names(x) = c("interval", "mean");

plot(x$mean ~ x$interval, type = "l", xlab = "interval", ylab = "mean", main = "Mean by time frame");
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# the maximum mean of steps:
x[max(x$mean) == x$mean, 1:2];
```

```
##     interval     mean
## 104      835 206.1698
```

```r
# removing temporary
rm(list = c("x", "cnames", "temp"));
```

## Imputing missing values


```r
# report the total number of missing values in the dataset
sum(is.na(dataset$steps));
```

```
## [1] 2304
```

```r
# calculate mean for interval
temp = tapply(dataset$steps, dataset$interval, mean, na.rm = TRUE);

cnames = as.integer(names(temp));

x = as.data.frame(cbind(cnames, unname(temp)));
names(x) = c("interval", "mean");

# for every NA step will use the mean of the interval
for(i in seq_along(dataset$steps)) {
    if(is.na(dataset$steps[i])) {
        dataset$steps[i] = x[x$interval == dataset$interval[i], 2];
    }
}


# removing temporary
rm(list = c("x", "cnames", "temp"));

# calculate daily sum of steps
tmp = sapply(split(dataset$steps, dataset$date), sum, na.rm = TRUE);

hist(tmp, freq = T, xlab = "Total number of steps", ylab = "Days frequevecy", main = "Histogram of total number of steps");
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# removing temporary
rm("tmp");

# calculate daily mean of steps
tmp = sapply(split(dataset$steps, dataset$date), mean, na.rm = TRUE);
# calculate daily median of steps
tmp1 = sapply(split(dataset$steps, dataset$date), median, na.rm = TRUE);

cnames = names(tmp);
t = as.data.frame(cbind(cnames, unname(tmp), unname(tmp1)));

names(t) = c("Day", "Mean", "Median");

print(t);
```

```
##           Day              Mean           Median
## 1  2012-10-01  37.3825995807128 34.1132075471698
## 2  2012-10-02            0.4375                0
## 3  2012-10-03  39.4166666666667                0
## 4  2012-10-04  42.0694444444444                0
## 5  2012-10-05  46.1597222222222                0
## 6  2012-10-06  53.5416666666667                0
## 7  2012-10-07  38.2465277777778                0
## 8  2012-10-08  37.3825995807128 34.1132075471698
## 9  2012-10-09  44.4826388888889                0
## 10 2012-10-10            34.375                0
## 11 2012-10-11  35.7777777777778                0
## 12 2012-10-12  60.3541666666667                0
## 13 2012-10-13  43.1458333333333                0
## 14 2012-10-14  52.4236111111111                0
## 15 2012-10-15  35.2048611111111                0
## 16 2012-10-16            52.375                0
## 17 2012-10-17  46.7083333333333                0
## 18 2012-10-18  34.9166666666667                0
## 19 2012-10-19  41.0729166666667                0
## 20 2012-10-20          36.09375                0
## 21 2012-10-21  30.6284722222222                0
## 22 2012-10-22  46.7361111111111                0
## 23 2012-10-23  30.9652777777778                0
## 24 2012-10-24  29.0104166666667                0
## 25 2012-10-25  8.65277777777778                0
## 26 2012-10-26  23.5347222222222                0
## 27 2012-10-27  35.1354166666667                0
## 28 2012-10-28  39.7847222222222                0
## 29 2012-10-29  17.4236111111111                0
## 30 2012-10-30          34.09375                0
## 31 2012-10-31  53.5208333333333                0
## 32 2012-11-01  37.3825995807128 34.1132075471698
## 33 2012-11-02  36.8055555555556                0
## 34 2012-11-03  36.7048611111111                0
## 35 2012-11-04  37.3825995807128 34.1132075471698
## 36 2012-11-05  36.2465277777778                0
## 37 2012-11-06           28.9375                0
## 38 2012-11-07  44.7326388888889                0
## 39 2012-11-08  11.1770833333333                0
## 40 2012-11-09  37.3825995807128 34.1132075471698
## 41 2012-11-10  37.3825995807128 34.1132075471698
## 42 2012-11-11  43.7777777777778                0
## 43 2012-11-12  37.3784722222222                0
## 44 2012-11-13  25.4722222222222                0
## 45 2012-11-14  37.3825995807128 34.1132075471698
## 46 2012-11-15 0.142361111111111                0
## 47 2012-11-16  18.8923611111111                0
## 48 2012-11-17  49.7881944444444                0
## 49 2012-11-18  52.4652777777778                0
## 50 2012-11-19  30.6979166666667                0
## 51 2012-11-20  15.5277777777778                0
## 52 2012-11-21  44.3993055555556                0
## 53 2012-11-22  70.9270833333333                0
## 54 2012-11-23  73.5902777777778                0
## 55 2012-11-24  50.2708333333333                0
## 56 2012-11-25  41.0902777777778                0
## 57 2012-11-26  38.7569444444444                0
## 58 2012-11-27  47.3819444444444                0
## 59 2012-11-28  35.3576388888889                0
## 60 2012-11-29          24.46875                0
## 61 2012-11-30  37.3825995807128 34.1132075471698
```

```r
# removing temporary
rm( list = c("tmp", "tmp1", "cnames", "t") );
```

Looking at the histograms we notice that the second one(the one without NAs) looks more like a normal distribution.

## Are there differences in activity patterns between weekdays and weekends?



```r
# creating wday column
dataset$wday = weekdays(dataset$date);
bool = grepl("^S", dataset$wday);
dataset$day.type = "Weekday";
dataset$day.type[bool] = "Weekend";
dataset$day.type = as.factor(dataset$day.type);


dataset_weekend = dataset[bool, ];
dataset_weekday = dataset[!bool, ];

# calculate mean for interval
temp = tapply(dataset_weekend$steps, dataset_weekend$interval, mean, na.rm = TRUE);

cnames = as.integer(names(temp));

x = as.data.frame(cbind(cnames, unname(temp)));
names(x) = c("interval", "mean");

temp1 = tapply(dataset_weekday$steps, dataset_weekday$interval, mean, na.rm = TRUE);

cnames1 = as.integer(names(temp1));

y = as.data.frame(cbind(cnames1, unname(temp1)));
names(y) = c("interval", "mean");

par(mfrow = c(2, 1));
plot(x$mean ~ x$interval, type = "l", xlab = "interval", ylab = "mean step", main = "Weekend", ylim = range(y$mean));
plot(y$mean ~ y$interval, type = "l", xlab = "interval", ylab = "mean step", main = "Weekday");
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
# removing temporary
rm(list = c("x", "cnames", "temp"));# 
rm(list = c("y", "cnames1", "temp1"));
rm(list = c("dataset_weekday", "dataset_weekend"));
```

