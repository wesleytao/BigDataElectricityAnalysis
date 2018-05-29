# structure_electricity_Cleaining
# import:mergeusedata.csv UTF_8  & weather.csv
# export:"Non_peak_users.csv" "merged_peak_users.csv"
#coding UTF-8
rm(list=ls())
setwd("C:/Wesley Tao/database/new")
library(foreign)
library(reshape2)
mydata<-read.csv("mergeusedata.csv")
mydata<-mydata[-1]


md<-melt(mydata,id=c("V1","V2"))
names(md)<-c("ID","date_time","hour","consumption")
# samplemd<-md[sample(1:nrow(md),10000,replace=FALSE)]
samplemd<-md
samplemd$date_time<-as.POSIXlt(samplemd$date_time)
samplemd$hour<-sapply(samplemd$hour,function(x){substr(x,6,7)})
samplemd$date_time$hour<-as.integer(samplemd$hour)-1
summary(samplemd)
# ID             date_time                       hour            consumption     
# Min.   :12240374   Min.   :2016-01-28 00:00:00   Length:229224      Min.   :      0  
# 1st Qu.:13283885   1st Qu.:2016-03-22 13:00:00   Class :character   1st Qu.:      0  
# Median :13722463   Median :2016-04-04 16:00:00   Mode  :character   Median :      0  
# Mean   :13446389   Mean   :2016-04-01 03:36:25                      Mean   :     35  
# 3rd Qu.:13825007   3rd Qu.:2016-04-17 22:00:00                      3rd Qu.:      0  
# Max.   :13975101   Max.   :2016-04-30 23:00:00                      Max.   :1263074   

rm("mydata")
rm("md")
mydata<-na.omit(samplemd)
rm("samplemd")
str(mydata)
?sample
sampledata<-mydata
weather<-read.csv("weather.csv")
weather<-weather[-1]

library(sqldf)
names(weather)
names(sampledata)
str(weather)
str(sampledata)
sampledata$date_time<-as.character(sampledata$date_time)
newdata<-sqldf("select ID, sampledata.date_time, consumption, high_tem,low_tem,tem,rain,pressure,humidity from sampledata left outer join weather on sampledata.date_time=weather.date_time ")
summary(sampledata)
newdata$marked<-1

olddata<-read.csv("W&E_complete.csv")
olddata<-olddata[-1]
olddata$marked<-0
names(olddata)[3]<-"consumption"
olddata$consumption<-olddata$consumption/1000
summary(olddata)
summary(newdata)
newdata$consumption[newdata$consumption>=10]<-NA

newdata<-newdata[!duplicated(newdata),]

sum(duplicated(newdata))
write.csv(olddata,"Non_peak_users.csv")
write.csv(newdata,"merged_peak_users.csv")
# olddata$consumption<-olddata$consumption/10
