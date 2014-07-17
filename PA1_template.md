# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

We assume that the data is present in the workspace. If not, the data should be loaded and unzipped into the workspace. We load the data into our environment:


```r
data<-read.csv("activity.csv")
```
 
Let's take a look at the data:


```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
 
We transform the date column into date-types:


```r
data$date<-as.Date(as.character(data$date))
```
 
And check the data again, to verify that our transformation worked:


```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?

We sum the number of steps per day and plot the histogram:


```r
library(plyr)
spd<-ddply(data, .(date), summarize, steps_per_day = sum(steps))
spd_cc<-na.omit(spd)
par(mar=c(4,5,1.5,1.5))
with(spd_cc, plot(date,steps_per_day, type="h", 
                  lwd=5, col="gold", main="Total number of steps per day", 
                  ylab="Steps per day", las=1,mgp=c(3,0.2,0), tck=0.02))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

Calculation of the mean and median total number of steps per day:


```r
df<-ddply(data, .(date), summarize,mean_value = mean(steps, na.rm=TRUE), 
          median_value = median(steps, na.rm=TRUE))
print(df)
```

```
##          date mean_value median_value
## 1  2012-10-01        NaN           NA
## 2  2012-10-02     0.4375            0
## 3  2012-10-03    39.4167            0
## 4  2012-10-04    42.0694            0
## 5  2012-10-05    46.1597            0
## 6  2012-10-06    53.5417            0
## 7  2012-10-07    38.2465            0
## 8  2012-10-08        NaN           NA
## 9  2012-10-09    44.4826            0
## 10 2012-10-10    34.3750            0
## 11 2012-10-11    35.7778            0
## 12 2012-10-12    60.3542            0
## 13 2012-10-13    43.1458            0
## 14 2012-10-14    52.4236            0
## 15 2012-10-15    35.2049            0
## 16 2012-10-16    52.3750            0
## 17 2012-10-17    46.7083            0
## 18 2012-10-18    34.9167            0
## 19 2012-10-19    41.0729            0
## 20 2012-10-20    36.0938            0
## 21 2012-10-21    30.6285            0
## 22 2012-10-22    46.7361            0
## 23 2012-10-23    30.9653            0
## 24 2012-10-24    29.0104            0
## 25 2012-10-25     8.6528            0
## 26 2012-10-26    23.5347            0
## 27 2012-10-27    35.1354            0
## 28 2012-10-28    39.7847            0
## 29 2012-10-29    17.4236            0
## 30 2012-10-30    34.0938            0
## 31 2012-10-31    53.5208            0
## 32 2012-11-01        NaN           NA
## 33 2012-11-02    36.8056            0
## 34 2012-11-03    36.7049            0
## 35 2012-11-04        NaN           NA
## 36 2012-11-05    36.2465            0
## 37 2012-11-06    28.9375            0
## 38 2012-11-07    44.7326            0
## 39 2012-11-08    11.1771            0
## 40 2012-11-09        NaN           NA
## 41 2012-11-10        NaN           NA
## 42 2012-11-11    43.7778            0
## 43 2012-11-12    37.3785            0
## 44 2012-11-13    25.4722            0
## 45 2012-11-14        NaN           NA
## 46 2012-11-15     0.1424            0
## 47 2012-11-16    18.8924            0
## 48 2012-11-17    49.7882            0
## 49 2012-11-18    52.4653            0
## 50 2012-11-19    30.6979            0
## 51 2012-11-20    15.5278            0
## 52 2012-11-21    44.3993            0
## 53 2012-11-22    70.9271            0
## 54 2012-11-23    73.5903            0
## 55 2012-11-24    50.2708            0
## 56 2012-11-25    41.0903            0
## 57 2012-11-26    38.7569            0
## 58 2012-11-27    47.3819            0
## 59 2012-11-28    35.3576            0
## 60 2012-11-29    24.4688            0
## 61 2012-11-30        NaN           NA
```

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
