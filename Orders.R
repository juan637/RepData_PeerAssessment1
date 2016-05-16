---
title: "Week 2-Markdown Homework"
output: html_document
---
str(activity)
summary(activity)

1.Code for reading in the dataset and/or processing the data

```{r first,echo=TRUE}
getwd()
setwd("C:/Users/User/Documents/Reproducible research/First Homework")
active<-read.table("C:/Users/User/Documents/Reproducible research/First Homework/activity.csv",sep=",",header=TRUE)
```

2.Histogram of the total number of steps taken each day
```{r,echo=TRUE}
str(active)
library(ggplot2)
date=as.character(active$date)
summary(active)
aa<-(tapply(active$steps,date,sum))
##Removing na
ab<- aa[!is.na(aa)]
View(ab)
plot(ab)
table=as.table(ab)
plot(table)
```

3.Mean and median number of steps taken each day

```{r,echo=TRUE}
##mean
View(tapply(active$steps,active$date,mean))
##median
View(tapply(active$steps,active$date,median))
```

4.Time series plot of the average number of steps taken
```{r,echo=TRUE}
ac<-(tapply(active$steps,date,mean))
ad<- ac[!is.na(ac)]
average=as.table(ad)
View(average)
plot(average)
```

5.The 5-minute interval that, on average, contains the maximum number of steps

```{r,echo=TRUE}
interval=as.character(active$interval)
##Here I have the data from the time interval with the maximum number of steps
ae<-active[,1]
af<-active[,3]

##Here I put the mean of the time interval
ag<-cbind(ae,af)
ah<-as.character(ae)
ai<-(tapply(af,ah,mean))
aj<-as.table(ai)

dataframe1<-as.data.frame(aj)
colnames(dataframe1)<-c("steps1","interval1")
steps2<-as.numeric(dataframe1$steps1)
interval1<-as.numeric(dataframe1$interval1)
max(steps2,rm.na=TRUE)
subset(dataframe1$interval1,steps2=="617")
```

6.Code to describe and show a strategy for imputing missing data

```{r,echo=TRUE}
##I am going to impute the na with some random number from the same column
randomly <- function (z){
        quantifying.na <- is.na(z)
        n.missing <- sum(quantifying.na)
        a.obs <- z[!quantifying.na]
        imputed <- z
        imputed[quantifying.na] <- sample (a.obs, n.missing, replace=TRUE)
        return (imputed)
}

##Formula for each variable-in this case an invented variable called active
any.variable<- randomly (active)
```

7.Histogram of the total number of steps taken each day after missing values are imputed
```{r,echo=TRUE}
randomly <- function (z){
        quantifying.na <- is.na(z)
        n.missing <- sum(quantifying.na)
        a.obs <- z[!quantifying.na]
        imputed <- z
        imputed[quantifying.na] <- sample (a.obs, n.missing, replace=TRUE)
        return (imputed)
}
##Looking for na´s in the date and steps variables
summary(is.na(active$date))
summary(is.na(active$steps))

##There is na´s only on steps,so
steps.random.na<- randomly (active$steps)

##Total number of steps, just like the second exercise
str(active)
library(ggplot2)
date=as.character(active$date)
ah<-(tapply(steps.random.na,date,sum))

plot(ah)
table1=as.table(ah)
View(table1)
plot(table1)
```

8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
```{r,echo=TRUE}
##Taken out the na´s and replacing it by random numbers
steps.random.na2<- randomly (active$steps)
ap<-(tapply(steps.random.na2,date,mean))
plot(ap)
table2=as.table(ap)
plot(table2)
aq<-as.data.frame(table2)
colnames(aq)<-c("date3","steps3")
##steps day after day, with the substitution of the na´s values
plot(aq)

##Coverting to days of the week
aq$date3<-as.POSIXlt(aq$date3)
day3<-weekdays(aq$date3)

ar<-(tapply(aq$steps3,day3,mean))
table3<-as.table(ar)
as<-as.data.frame(table3)        
colnames(as)<-c("days4","steps4")
##Distribution in the different days of the week
plot(as)
```