setwd("~/Dropbox/Coursera/Data Science/05 Reproducable Research/PeerAssessment1/")
# getwd()
# View(data)
# attach(data)
# detach(devtools)
# rm(list=ls())

## Setting the Global Option for Always Echoing the R Code
# The following code will set a global option.
# ```{r setoptions, echo = T}
# opts_chunk$set(echo = F, results = "hide")
# ```



## Loading the Data
if(!file.exists("./data")){dir.create("./data")}
fileURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL,destfile="./data/ActivityData.zip", method="curl")
unzip("./data/ActivityData.zip")
activityData<-read.csv("./activity.csv")
head(activityData)



# Processing the Data

## Excluding NA
activityDatanoNA <- activityData[activityData$steps != is.na(activityData$steps),]
activityDatanoNA <- activityDatanoNA[complete.cases(activityDatanoNA$steps),]
# head(activityDatanoNA)
# length(activityDatanoNA$steps)

## Removing Dates 
#### Removing date: "2012-10-02" and "2012-11-15" as the number of steps is too low and biases the analysis (probably step-counter not used properly/ functioned that day)
############### activityDatanoNA <- activityDatanoNA[activityDatanoNA$date!="2012-10-02" & activityDatanoNA$date!="2012-11-15",]
# head(activityDatanoNA)
# length(activityDatanoNA$steps)


# What is Mean Total Number of Steps Taken per Day?
## Histogram of Total Number of Steps Taken Each Day
### Splitting the Data According to Dates (no NA)
splitOfActivityDatanoNA <- split(activityDatanoNA$steps,activityDatanoNA$date, drop = T)

### Calculating the Sum of Steps for Each Day (no NA)
sumSteps <- sapply(splitOfActivityDatanoNA,sum)
sumSteps <- as.vector(sumSteps)
### Mean Total Number of Steps Taken per Day (no NA)
meanStepsDate <- sapply(splitOfActivityDatanoNA,mean)
meanSteps <- as.vector(meanStepsDate)
mean(meanSteps)
### Median Total Number of Steps Taken per Day (no NA)
medianSteps <- sapply(splitOfActivityDatanoNA,median)
medianSteps <- as.vector(medianSteps)
median(medianSteps)
mean(medianSteps)

hist(sumSteps, main = "Number of Steps per Day (NA Ignored)", xlab = "Number of Steps per Day")


# Inputing Missing Values

## Total Number of Missing Values
table(is.na(activityData$steps))

## Filling in the NA Values
### Splitting the Data According to Dates (with NA)
activityDataFilled <- activityData
splitOfActivityDataFilled <- split(activityDataFilled$steps,activityDataFilled$date, drop = T)
# meanStepsFilled <- sapply(splitOfActivityDataFilled,mean)

# head(activityDataFilled)
# head(splitOfActivityDataFilled)
# head(meanStepsFilled)
### Creating Vector lengthIsNA which Contains the Number corresponding to the subset of an NA value
lengthIsNA <- 1:length(is.na(activityData$steps))
# lengthIsNA[is.na(activityData$steps)]
# length(lengthIsNA[is.na(activityData$steps)])


################
### Control: ###
################
head(activityDataFilled)
# max(activityDataFilled$steps)
# 
activityData[800,1]
activityDataFilled[800,1]
activityDataFilled[1,1]
################

# What is Mean Total Number of Steps Taken per Day (Filled NA)?
## Histogram of Total Number of Steps Taken Each Day (Filled NA)
############
# splitOfActivityDatanoNA <- split(activityDatanoNA$steps,activityDatanoNA$date, drop = T)
# 
# ### Calculating the Sum of Steps for Each Day (Filled NA)
# sumSteps <- sapply(splitOfActivityDatanoNA,sum)
# sumSteps <- as.vector(sumSteps)
############

### Splitting the Data According to Dates (Filled NA)
splitOfActivityDataFilled <- split(activityDataFilled$steps,activityDataFilled$date, drop = T)

### Calculating the Sum of Steps for Each Day (Filled NA)
sumStepswithNA <- sapply(splitOfActivityDataFilled,sum)
sumStepswithNA <- as.vector(sumStepswithNA)

hist(sumStepswithNA, main = "Number of Steps per Day (NA Filled)", xlab = "Number of Steps per Day")

### Mean Total Number of Steps Taken per Day (Filled NA)
meanStepsFilled <- sapply(splitOfActivityDataFilled,mean)
meanStepsFilled <- as.vector(meanStepsFilled)
mean(meanStepsFilled)

### Median Total Number of Steps Taken per Day (Filled NA)
medianStepsFilled <- sapply(splitOfActivityDataFilled,median)
medianStepsFilled <- as.vector(medianStepsFilled)
median(medianStepsFilled)



# Time Series Plot
## Plot
splitOfActivityDataInterval <- split(activityDataFilled$steps,activityDataFilled$interval, drop = T)
meanStepsInterval <- sapply(splitOfActivityDataInterval,mean)
meanStepsIntervalVector <- as.vector(meanStepsInterval)
activityDataFilled[1:288,3]

plot(activityDataFilled[1:288,3], meanStepsIntervalVector, type = "l", main = "Time Series Plot", xlab = "5 min Intervals", ylab = "Number of Steps")
abline(v=activityDataFilled[1:288,3][meanStepsIntervalVector==max(meanStepsIntervalVector)], col = "red") # Interval with maximum number of steps

## Which 5 min Interval has the Maximum Number of Steps on Average
activityDataFilled[1:288,3][meanStepsIntervalVector==max(meanStepsIntervalVector)]


# Creating a New Vector Variable: Weekday/ Weekend
## Checking if packages are installed and install.packages if not installed
if(!("data.table" %in% row.names(installed.packages()))){
      install.packages(datatable)
}
library(data.table)
## Creating Vector of 61 Dates
activityDataFilledDT <- as.data.table(activityDataFilled)
dates <- as.vector(activityDataFilledDT[,.N,by=date]$date)
## Extracting the weekdays of the above Vector
datesNames <- strftime(dates, format = "%A", tz = "GMT", usetz = FALSE)
## Creating a reference data.table with dates, workday names and numbering
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
referenceDT
## 
activityDataFilledDT <- as.data.table(activityDataFilled)
activityDataFilledDT$workdays <- as.character(activityDataFilledDT$date)
activityDataFilledDT$numbering <- 1:length(activityDataFilledDT$workdays)
for (i in 1:length(referenceDT$dates)){
       subset <- activityDataFilledDT$numbering[activityDataFilledDT$date == referenceDT$dates[i]]
       activityDataFilledDT$workdays[min(subset):max(subset)] <- referenceDT$workday[i]
}
activityDataFilledDT <- subset(activityDataFilledDT, select = -numbering)
head(activityDataFilledDT)

# 

# Time Series Plot (Weekdays)
splitOfActivityDataWorkdays <- split(activityDataFilledDT,activityDataFilledDT$workdays, drop = T)
splitOfActivityDataWeekday <- splitOfActivityDataWorkdays$weekday
splitOfActivityDataWeekday <- split(splitOfActivityDataWeekday$steps,splitOfActivityDataWeekday$interval, drop = T)
meanStepsIntervalWeekday <- sapply(splitOfActivityDataWeekday,mean)
meanStepsIntervalWeekdayVector <- as.vector(meanStepsIntervalWeekday)
par(mfrow = c(2,1))
plot(activityDataFilled[1:288,3], meanStepsIntervalWeekdayVector, type = "l", main = "Time Series Plot (Weekday)", xlab = "5 min Intervals", ylab = "Number of Steps")

# Time Series Plot (Weekend)
splitOfActivityDataWorkdays <- split(activityDataFilledDT,activityDataFilledDT$workdays, drop = T)
splitOfActivityDataWeekend <- splitOfActivityDataWorkdays$weekend
splitOfActivityDataWeekend <- split(splitOfActivityDataWeekend$steps,splitOfActivityDataWeekend$interval, drop = T)
meanStepsIntervalWeekend <- sapply(splitOfActivityDataWeekend,mean)
meanStepsIntervalWeekendVector <- as.vector(meanStepsIntervalWeekend)
# activityDataFilledDT[1:288,3]
plot(activityDataFilled[1:288,3], meanStepsIntervalWeekendVector, type = "l", main = "Time Series Plot (Weekend)", xlab = "5 min Intervals", ylab = "Number of Steps")
