---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
##Code for reading in the dataset and/or processing the data OK
#Load the necessary package
    library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 4.0.3
```

```
## -- Attaching packages ---------------------------------------- tidyverse 1.3.0 --
```

```
## √ ggplot2 3.3.2     √ purrr   0.3.4
## √ tibble  3.0.3     √ dplyr   1.0.3
## √ tidyr   1.1.2     √ stringr 1.4.0
## √ readr   1.4.0     √ forcats 0.5.1
```

```
## Warning: package 'tidyr' was built under R version 4.0.3
```

```
## Warning: package 'readr' was built under R version 4.0.3
```

```
## Warning: package 'dplyr' was built under R version 4.0.3
```

```
## Warning: package 'forcats' was built under R version 4.0.3
```

```
## -- Conflicts ------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
#Load the data
    activity <- read_csv("activity.csv")
```

```
## 
## -- Column specification ---------------------------------------------------------
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

## What is mean total number of steps taken per day?

```r
total_steps <- activity %>%
    #remove NA data
    drop_na(steps) %>% 
    #aggregate by date
    group_by(date) %>%  
    #calculate the total of steps per group
    summarise(mean_steps_day = sum(steps)) 
total_steps
```

```
## # A tibble: 53 x 2
##    date       mean_steps_day
##  * <date>              <dbl>
##  1 2012-10-02            126
##  2 2012-10-03          11352
##  3 2012-10-04          12116
##  4 2012-10-05          13294
##  5 2012-10-06          15420
##  6 2012-10-07          11015
##  7 2012-10-09          12811
##  8 2012-10-10           9900
##  9 2012-10-11          10304
## 10 2012-10-12          17382
## # ... with 43 more rows
```

```r
##Histogram of the total number of steps taken each day
total_steps_hist <- ggplot(data = total_steps,aes(mean_steps_day) )+
                    geom_histogram(bins = 30, color = "black") +
                    labs(x = "Number of Steps",
                         y = "Date",
                         title = "Total steps per Day(excluding NA datas)")
total_steps_hist
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
##Mean and median number of steps taken each day
mean(total_steps$mean_steps_day)
```

```
## [1] 10766.19
```

```r
median(total_steps$mean_steps_day)
```

```
## [1] 10765
```




## What is the average daily activity pattern?

```r
##Time series plot of the average number of steps taken

daily_activity <- activity %>%
    # remove NA
    drop_na() %>%
    # aggregate by interval
    group_by(interval) %>% 
    #calculate the mean steps per group 
    summarize(interval_steps = mean(steps))

#Creating a plot
    interval_steps_plot <- ggplot(daily_activity,
                                  aes(interval,interval_steps)) +
        geom_line(size = 1.0) +
        labs(x = "Interval", 
             y = "Steps", 
             title = "Average number of Steps per Interval" ) 
    interval_steps_plot
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
##The 5-minute interval that, on average, contains the maximum number of steps
daily_activity[which.max(daily_activity$interval_steps),]
```

```
## # A tibble: 1 x 2
##   interval interval_steps
##      <dbl>          <dbl>
## 1      835           206.
```

## Imputing missing values

```r
#Calculate and report the total number of missing values
data_na <- sum(is.na(activity$steps))
print(data_na)
```

```
## [1] 2304
```

```r
#filling in all of the missing values in the dataset
activity_fill <- activity %>% 
  fill(steps,.direction = "up") %>% 
    fill(steps)

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

#aggregate by date
total_steps2 <- activity_fill %>% 
group_by(date) %>%  
#calculate the total of steps per group
summarise(mean_steps_day = sum(steps)) 
total_steps2
```

```
## # A tibble: 61 x 2
##    date       mean_steps_day
##  * <date>              <dbl>
##  1 2012-10-01              0
##  2 2012-10-02            126
##  3 2012-10-03          11352
##  4 2012-10-04          12116
##  5 2012-10-05          13294
##  6 2012-10-06          15420
##  7 2012-10-07          11015
##  8 2012-10-08              0
##  9 2012-10-09          12811
## 10 2012-10-10           9900
## # ... with 51 more rows
```

```r
##Histogram of the total number of steps taken each day
total_steps_hist2 <- ggplot(data = total_steps2 ,aes(mean_steps_day) )+
                    geom_histogram(bins = 30, color = "black") +
                    labs(x = "Number of Steps",
                         y = "Date",
                         title = "Total steps per Day(filled NA data)")
total_steps_hist
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
##Mean and median number of steps taken each day
mean(total_steps2$mean_steps_day)
```

```
## [1] 9354.23
```

```r
median(total_steps2$mean_steps_day)
```

```
## [1] 10395
```

```r
#Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

#They differs slightly.
#The mean of the first data excluding NA is 10766.19, the median is 10765.
#The mean of the second data filling the missing data is 9354.23, the median is 10395.
```


## Are there differences in activity patterns between weekdays and weekends?

```r
#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 4.0.3
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
week_activity <- activity_fill %>% 
mutate(week_number = wday(date)) 

week_activity <- week_activity %>% 
mutate(key = if_else(week_number == c(1,7),"weekend","weekday"))

week_average <- week_activity %>%
  group_by(key,interval) %>% 
  summarize(average = mean(steps))
```

```
## `summarise()` has grouped output by 'key'. You can override using the `.groups` argument.
```

```r
week_average
```

```
## # A tibble: 576 x 3
## # Groups:   key [2]
##    key     interval average
##    <chr>      <dbl>   <dbl>
##  1 weekday        0  1.72  
##  2 weekday        5  0.340 
##  3 weekday       10  0.132 
##  4 weekday       15  0.151 
##  5 weekday       20  0.0755
##  6 weekday       25  2.09  
##  7 weekday       30  0.528 
##  8 weekday       35  0.868 
##  9 weekday       40  0     
## 10 weekday       45  1.36  
## # ... with 566 more rows
```

```r
#Make a panel plot 
ggplot(week_average, aes(interval, average)) + 
        geom_line() + 
        facet_grid(key ~ .) +
        xlab("5-minute interval") + 
        ylab("avarage number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


