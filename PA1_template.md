Programming Assignemt 1 - Mario Beck
========================================================

## Loading the Workspace
#### Setting the Working Directory

```r
setwd("~/Dropbox/Coursera/Data Science/05 Reproducable Research/PeerAssessment1/")
```
#### Setting the Global Option for Always Echoing the R Code
The following code will set a global option to always show the code chunk, the result (already set by default) and will cache the results to run faster after first compiling.

```r
opts_chunk$set(echo = T, cache = T)
```
  
  
## Loading the Data

```r
# checking if file directory already exists
if(!file.exists("./data")){dir.create("./data")}

# downloading and unziping the file
fileURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL,destfile="./data/ActivityData.zip", method="curl")
unzip("./data/ActivityData.zip")

# reading the file
activityData<-read.csv("./activity.csv")
```


## Processing the Data
### Excluding NA

```r
# filling all corresponding rows where there are NA values with NAs
activityDatanoNA <- activityData[activityData$steps != is.na(activityData$steps),]

# excluding all NA cases and creating a cleaned dataset
activityDatanoNA <- activityDatanoNA[complete.cases(activityDatanoNA$steps),]
```
In the following the activity Data will always be accessible with and without NA values by calling either "activityData" which will return the dataset with NA values or "activityData" which will return the dataset withOUT NA values.


## What is Mean Total Number of Steps Taken per Day?
### Histogram of Total Number of Steps Taken Each Day

```r
# splitting the data according to dates (ignoring NA)
splitOfActivityDatanoNA <- split(activityDatanoNA$steps,activityDatanoNA$date, drop = T)

# calculating the sum of steps for each day (ignoring NA)
sumSteps <- sapply(splitOfActivityDatanoNA,sum)
sumSteps <- as.vector(sumSteps)

# drawing the histogram
par(mfrow=c(1,1))
hist(sumSteps, main = "Number of Steps per Day (NA Ignored)", xlab = "Number of Steps per Day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


### Mean Total Number of Steps Taken per Day (no NA)

```r
# calculating the mean total number of steps taken per day
meanStepsDate <- sapply(splitOfActivityDatanoNA,mean)
meanSteps <- as.vector(meanStepsDate)

# returning the total mean
mean(meanSteps)
```

```
## [1] 129.7
```


### Median Total Number of Steps Taken per Day (no NA)

```r
# calculating the median total number of steps taken per day
medianSteps <- sapply(splitOfActivityDatanoNA,median)
medianSteps <- as.vector(medianSteps)

# returning the total median
median(medianSteps)
```

```
## [1] 56
```


## Inputing Missing Values
### 1. Total Number of Missing Values

```r
table(is.na(activityData$steps))
```

```
## 
## FALSE  TRUE 
## 15264  2304
```
The TRUE value accounts for all the missing values.
### 2. Filling in the NA Values

```r
## Splitting the Data According to Dates (with NA)
# creating identical data.frame
activityDataFilled <- activityData

# splitting the activityData by date
splitOfActivityDataFilled <- split(activityDataFilled$steps,activityDataFilled$date, drop = T)

# total number of NA values
lengthIsNA <- 1:length(is.na(activityData$steps))

# disabling warnings as the warning produced is only due to "meanSteps" being only a single value and not of the same length as the NA vector it is supposed to fill
options(warn = -1)

# filling in the NA values with the corresponding mean of that day
for(i in lengthIsNA[is.na(activityData$steps)]){
      activityDataFilled$steps[i] <- meanSteps # DatewithNA[[activityData$date[1]]]
}

# re-enabling warning to default setup
options(warn = 0)

## checking if it worked:
head(activityData)
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

```r
head(activityDataFilled)
```

```
##   steps       date interval
## 1    63 2012-10-01        0
## 2    63 2012-10-01        5
## 3    63 2012-10-01       10
## 4    63 2012-10-01       15
## 5    63 2012-10-01       20
## 6    63 2012-10-01       25
```


### 3. Histogram of Total Number of Steps Taken Each Day (Filled NA)

```r
# splitting the data according to dates (filled NA)
splitOfActivityDataFilled <- split(activityDataFilled$steps,activityDataFilled$date, drop = T)

# calculating the sum of steps for each day (filled NA)
sumStepswithNA <- sapply(splitOfActivityDataFilled,sum)
sumStepswithNA <- as.vector(sumStepswithNA)

# drawing the histogram
hist(sumStepswithNA, main = "Number of Steps per Day (NA Filled)", xlab = "Number of Steps per Day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 
### Mean Total Number of Steps Taken per Day (Filled NA)

```r
# calculating the mean total number of steps taken per day
meanStepsFilled <- sapply(splitOfActivityDataFilled,mean)
meanStepsFilled <- as.vector(meanStepsFilled)

# returning the total mean
mean(meanStepsFilled)
```

```
## [1] 40.74
```


### Median Total Number of Steps Taken per Day (Filled NA)

```r
# calculating the median total number of steps taken per day
medianStepsFilled <- sapply(splitOfActivityDataFilled,median)
medianStepsFilled <- as.vector(medianStepsFilled)

# returning the total median
median(medianStepsFilled)
```

```
## [1] 0
```


## Time Series Plot
### 1. Plot

```r
# preparing the plot
splitOfActivityDataInterval <- split(activityDataFilled$steps,activityDataFilled$interval, drop = T)
meanStepsInterval <- sapply(splitOfActivityDataInterval,mean)
meanStepsIntervalVector <- as.vector(meanStepsInterval)

# drawing the plot
plot(activityDataFilled[1:288,3], meanStepsIntervalVector, type = "l", main = "Time Series Plot", xlab = "5 min Intervals", ylab = "Number of Steps")

# adding a
abline(v=activityDataFilled[1:288,3][meanStepsIntervalVector==max(meanStepsIntervalVector)], col = "red") # Interval with maximum number of steps
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 
### 2. Which 5 min Interval has the Maximum Number of Steps on Average

```r
activityDataFilled[1:288,3][meanStepsIntervalVector==max(meanStepsIntervalVector)]
```

```
## [1] 835
```


## Creating a New Vector Variable: Weekday/ Weekend

```r
# checking if packages are installed and install.packages if not installed
if(!("data.table" %in% row.names(installed.packages()))){
      install.packages(datatable)
}

# loading package
library(data.table)

# creating vector of 61 dates
activityDataFilledDT <- as.data.table(activityDataFilled)
dates <- as.vector(activityDataFilledDT[,.N,by=date]$date)

# extracting the weekdays of the above vector
datesNames <- strftime(dates, format = "%A", tz = "GMT", usetz = FALSE)

# Creating a reference data.table with dates, workday names and numbering
DT1 <- as.data.table(dates)
DT1$key <- 1:length(dates)
setkey(DT1,key)
DT2 <- as.data.table(datesNames)
DT2$key <- 1:length(dates)
setkey(DT2,key)
x <- c("weekday","weekday","weekday","weekday","weekday","weekend","weekend")
workday <- rep_len(x,length(dates))
workday <- as.data.table(workday)
workday$key <- 1:length(dates)
setkey(workday,key)
DT4 <- merge(DT1,DT2)
DT5 <- merge(workday,DT4)
referenceDT <- subset(DT5, select = -key)

# filling the data.table with corresponding workdays (weekday/ weekend)
activityDataFilledDT <- as.data.table(activityDataFilled)
activityDataFilledDT$workdays <- as.character(activityDataFilledDT$date)
activityDataFilledDT$numbering <- 1:length(activityDataFilledDT$workdays)
for (i in 1:length(referenceDT$dates)){
       subset <- activityDataFilledDT$numbering[activityDataFilledDT$date == referenceDT$dates[i]]
       activityDataFilledDT$workdays[min(subset):max(subset)] <- referenceDT$workday[i]
}

# removing the numbering column
activityDataFilledDT <- subset(activityDataFilledDT, select = -numbering)

# checking if it worked:
head(activityDataFilled)
```

```
##   steps       date interval
## 1    63 2012-10-01        0
## 2    63 2012-10-01        5
## 3    63 2012-10-01       10
## 4    63 2012-10-01       15
## 5    63 2012-10-01       20
## 6    63 2012-10-01       25
```

```r
head(activityDataFilledDT)
```

```
##    steps       date interval workdays
## 1:    63 2012-10-01        0  weekday
## 2:    63 2012-10-01        5  weekday
## 3:    63 2012-10-01       10  weekday
## 4:    63 2012-10-01       15  weekday
## 5:    63 2012-10-01       20  weekday
## 6:    63 2012-10-01       25  weekday
```


## Differences in Activity Between Weekdays and Weekends
### Time Series Plot (Weekdays)

```r
# preparing the plot
splitOfActivityDataWorkdays <- split(activityDataFilledDT,activityDataFilledDT$workdays, drop = T)
splitOfActivityDataWeekday <- splitOfActivityDataWorkdays$weekday
splitOfActivityDataWeekday <- split(splitOfActivityDataWeekday$steps,splitOfActivityDataWeekday$interval, drop = T)
meanStepsIntervalWeekday <- sapply(splitOfActivityDataWeekday,mean)
meanStepsIntervalWeekdayVector <- as.vector(meanStepsIntervalWeekday)

# drawing the plot
par(mfrow = c(2,1))
plot(activityDataFilled[1:288,3], meanStepsIntervalWeekdayVector, type = "l", main = "Time Series Plot (Weekday)", xlab = "5 min Intervals", ylab = "Number of Steps")

# preparing the plot
splitOfActivityDataWorkdays <- split(activityDataFilledDT,activityDataFilledDT$workdays, drop = T)
splitOfActivityDataWeekend <- splitOfActivityDataWorkdays$weekend
splitOfActivityDataWeekend <- split(splitOfActivityDataWeekend$steps,splitOfActivityDataWeekend$interval, drop = T)
meanStepsIntervalWeekend <- sapply(splitOfActivityDataWeekend,mean)
meanStepsIntervalWeekendVector <- as.vector(meanStepsIntervalWeekend)

# drawing the plot
plot(activityDataFilled[1:288,3], meanStepsIntervalWeekendVector, type = "l", main = "Time Series Plot (Weekend)", xlab = "5 min Intervals", ylab = "Number of Steps")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 
