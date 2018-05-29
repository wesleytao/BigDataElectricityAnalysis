# identify NA and filled data 
# import: W&E_complete.csv
#

rm(list=ls())
setwd("C:/Wesley Tao/database/new")
library(foreign)
mydata<-read.csv("W&E_complete.csv")
library("VIM")
library("sqldf")
mydata<-mydata[-1]
str(mydata)
mydata$date_time<-as.POSIXct(mydata$date_time)
str(mydata)
# newdata<-sqldf("select * from mydata order by date_time")
Null_data<-sqldf("select * from mydata where tem is NULL")
mean(!complete.cases(mydata))#0.1208509
summary(mydata)
# ?lm
# simple_regression<-lm(Consumption~low_tem+pressure+humidity,mydata)
# summary(simple_regression)
