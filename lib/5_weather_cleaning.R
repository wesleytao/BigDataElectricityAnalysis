#5_weahter cleaning  天气事实数据清洗 & 电力数据合并
# function： statistical description and data cleaning
# import:天气实测数据.csv NA_need_to_be_filled.csv
# export:weather.csv       W&E_complete.csv
# time:2017/6/27
# author: wesleytao
# encoding: UTF-8

rm(list=ls())
setwd("C:/Wesley Tao/database")
library(foreign)
mydata<-read.csv("天气实测数据.csv")
# str(mydata)
# 'data.frame':	229718 obs. of  10 variables:
#   $ station_code  : int  58370 58370 58370 58370 58370 58370 58370 58370 58370 58370 ...
# $ date_time     : Factor w/ 229706 levels "","2012/1/1 0:00",..: 2 3 14 19 20 21 22 23 24 25 ...
# $ tem           : num  1.9 1.8 1.8 1.7 2.8 4.1 4.6 5.4 5.8 5.9 ...
# $ high_tem      : num  2.4 1.9 1.9 1.8 2.8 4.1 4.6 5.4 5.8 5.9 ...
# $ low_tem       : num  1.9 1.6 1.8 1.6 1.7 2.9 4.1 4.5 5.4 5.8 ...
# $ rain          : num  0 0 0 0 0 0 0 0 0 0 ...
# $ wind_direction: int  108 202 240 14 310 200 26 257 53 278 ...
# $ wind_velocity : num  0.6 0.2 0 0.7 0.2 0.6 0.5 1.6 1.4 1.6 ...
# $ pressure      : num  1027 1027 1027 1027 1027 ...
# $ humidity      : int  87 88 89 90 91 91 90 77 68 60 ...

library(sqldf)
sqldf("select station_code from mydata group by station_code")
mydata<-na.omit(mydata) #有13行空行
summary(mydata)

#将所有9999改成NA值

mydata$tem[mydata$tem>=9999]<-NA
mydata$high_tem[mydata$tem>=9999]<-NA
mydata$rain[mydata$rain>=9999]<-NA
mydata$wind_direction[mydata$wind_direction>=9999]<-NA
mydata$wind_velocity[mydata$wind_velocity>=9999]<-NA
mydata$humidity[mydata$humidity>=9999]<-NA
mydata$pressure[mydata$pressure>=9999]<-NA


summary(mydata)
# station_code
# 1           NA
# 2        58370
# testing<-is.na(mydata$station_code)
# sum(testing)
# > sum(testing)
# [1] 13


mydata$id_code<-c(1:229705)

mydata$date_time<-as.POSIXlt(mydata$date_time)
mydata$diff_time<-c(as.numeric(diff(mydata$date_time)),NA)#当前数据与后一位时间的差值
mydata$date_time<-as.character(mydata$date_time)
# sqldf("select min(id_code) from mydata where diff_time<60")
mydata$date_time<-as.POSIXlt(mydata$date_time)
# id_code>=8663
subset(mydata,mydata$id_code>8650&mydata$id_code<8670)

need_aggregate<-subset(mydata,mydata$id_code>=8663)
dont_need_aggregate<-subset(mydata,mydata$id_code<8663)

# 小时段内取平均值
names(need_aggregate)
# 
# [1] "station_code"   "date_time"      "tem"            "high_tem"       "low_tem"        "rain"           "wind_direction"
# [8] "wind_velocity"  "pressure"       "humidity"       "diff_time"      "id_code"
library(lubridate)
str(mydata)
need_aggregate$floor_date<-floor_date(need_aggregate$date_time,unit="hours")
need_aggregate$floor_date<-as.character(need_aggregate$floor_date)
need_aggregate$date_time<-as.character(need_aggregate$date_time)

aggregate_complete<-sqldf("select station_code,
      floor_date as date_time, 
      avg(tem) as tem,
      max(high_tem) as high_tem,
      min(low_tem) as low_tem,
      sum(rain) as rain,
      avg(wind_direction) as wind_direction,
      avg(wind_velocity)as wind_velocity,
      avg(pressure) as pressure,   
      avg(humidity) as humidity
       from need_aggregate group by floor_date")
mylabel<-names(aggregate_complete)
names(dont_need_aggregate)
dont_need_aggregate<-dont_need_aggregate[mylabel]
# str(aggregate_complete)
# str(dont_need_aggregate)
dont_need_aggregate$date_time<-as.character(dont_need_aggregate$date_time)
mydata<-rbind(aggregate_complete,dont_need_aggregate)


str(mydata)

# mydata$date<-as.Date(mydata$date_time)
# mydata$hour<-as.POSIXlt(mydata$date_time)$hour
setwd("C:/Wesley Tao/database/new")
write.csv(mydata,"weather.csv")

setwd("C:/Wesley Tao/database/new")
weather<-read.csv("weather.csv")
weather<-weather[-1]
weather<-mydata
electricity<-read.csv("Merged_part_1_2.csv")

mylabel<-ls()
remove<-setdiff(mylabel,c("weather","electricity"))
rm(list=remove)

names(electricity)
# electricity<-electricity[-1]
electricity<-electricity[-1]

names(electricity)

# [1] "No"          "ID"          "date"        "hour"        "Consumption"
t<-as.POSIXlt(electricity$date)
t$hour<-electricity$hour
electricity$date_time<-as.character(t)
names(electricity)
electricity<-electricity[-2]


# names(weather)
# [1] "station_code"   "date_time"      "tem"            "high_tem"       "low_tem"        "rain"   
# [7] "wind_direction" "wind_velocity"  "pressure"       "humidity"       "date"           "hour" 


sampleweather<-weather
sampleele<-electricity
str(sampleele)
str(sampleweather)
newdata<-sqldf("select ID, sampleele.date_time, Consumption, high_tem,low_tem,tem,rain,pressure,humidity from sampleele left outer join sampleweather on sampleele.date_time=sampleweather.date_time ")

write.csv(newdata,"W&E_complete.csv")
#
summary(newdata)


