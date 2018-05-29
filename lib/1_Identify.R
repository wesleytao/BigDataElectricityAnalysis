#title 电力数据时间规整,
# function：因为时间跨度不一致，有些超过一小时或者短于一小时的时间，先进行初步识???
# date 2017/6/16
# function: identify thoses need normalize: period exceed 1 
# input:rawdata.csv
# output:short_normalize & long_normalize.csv
# encoding UTF-8


library(lubridate)
library(foreign)
rm(list=ls())
setwd("C:/Wesley Tao/database")
rawdata<-read.csv("rawdata.csv")
names(rawdata)[1]<-c("No")
# rawdata<-duplicated(rawdata)
# sum(rawdata) [1] 0
#查找缺失???
isNA<-is.na(rawdata)
sum(isNA)
#
sample<-rawdata

#求出开始时间的ceiling 与结束时间的floor 以及两者之间的时间???
start_ceiling<-sapply(sample$Start_time,FUN=function(x){ceiling_date(as.POSIXct(x,origin="1970/01/01 0:00"),"hour")},simplify = TRUE)#对所有的开始时间处理，开始时间上???

sample$s_up<-as.POSIXlt(start_ceiling,origin = "1970/01/01 0:00")

end_floor<-sapply(sample$End_time,FUN = function(x){floor_date(as.POSIXct(x,origin="1970/01/01 0:00"),"hour")},simplify = TRUE)
sample$e_down<-as.POSIXlt(end_floor,origin = "1970/01/01 0:00")
#duration 是指持续了多少时间，单位小时
sample$duration<-difftime(sample$End_time,sample$Start_time,units="hours")    
sample$duration<-as.numeric(sample$duration)
sample$Consumption<-as.numeric(sample$Consumption)
sample$avg_c<-(sample$Consumption/sample$duration)

#数据规整的进行筛选duration
sample$diff_time<-difftime(sample$e_down,sample$s_up,unit="hours")#diff_time 仅仅是用于筛???
# library(sqldf)
long_normalize<-subset(sample,diff_time>=0 & duration>1.1)
l_No<-long_normalize$No
all_No<-sample$No
S_No<-setdiff(all_No,l_No)
short_normalize<-subset(sample,No %in% S_No)

write.csv(short_normalize,"short_normalize.csv")
write.csv(long_normalize,"long_normalize.csv")
#############

short_normalize<-read.csv("short_normalize.csv")
long_normalize<-read.csv("long_normalize.csv")

mydata<-rbind(short_normalize,long_normalize)
mydata<-mydata[-1]
mydata<-mydata[-1]
 # sample<-mydata[1:10000,]
sample<-mydata
#1 检查是否消费量合理�?
names(sample)
library(sqldf)
# [1] "ID"          "Start_c"     "End_c"       "Consumption" "Start_time"  "End_time"    "s_up"        "e_down"     
# [9] "duration"    "avg_c"       "diff_time"   "newC"  
attach(sample)
cumspt<-End_c-Start_c-Consumption
sample$newC<-cumspt
# sqldf("select Start_c,End_c,Consumption,newC  from sample where newC != '0'")
# sqldf("select count(*) from sample where newC != '0'") 627363
cumspt<-End_c-Start_c
sample$newC<-cumspt
detach(sample)
# summary(sample)
#2 找出所有超�?2小时的数据并剔除
summary(sample$newC)
summary(sample$duration)
# plot(density(sample$duration))
duration<-as.numeric(sample$duration)
a<-data.frame(duration)
a$factor[a$duration<0]<-"lower than 1"
a$factor[a$duration>=0 & a$duration<=1]<-"0-1"
a$factor[a$duration>1 & a$duration<=2]<-"1-2"
a$factor[a$duration>2 & a$duration<10]<-"2-10"
a$factor[a$duration>10]<-"over 10"
sqldf("select * from a where factor ='over 10'")
table(a$factor)
library(Hmisc)
describe(a$duration)
# attributes(sample)
sqldf("select count(*) from sample where duration>2")#7742
##select data which duration is less than 2 hours
sample<-subset(sample,sample$duration<=2)#
# sqldf("select count(*) from sample where duration<=2")#882133
# sqldf("select count(*) from sample where duration>2")#0


#3 查看comsuption的NA值，异常值（负号，超大值等�?
summary(sample$Consumption)
summary(sample$newC)
plot(density(sample$newC))

attach(sample)
a<-data.frame(newC)
names(a)<-"Consumption"
a$factor[a$Consumption<0]<-"lower than 0"
a$factor[a$Consumption>=0& a$Consumption<10]<-"0-10"
a$factor[a$Consumption>=10& a$Consumption<50]<-"10-50"
a$factor[a$Consumption>=50 & a$Consumption<100]<-"50-100"
a$factor[a$Consumption>=100& a$Consumption<1000]<-"over 100"
a$factor[a$Consumption>=1000]<-"over 1000"
table(a$factor)
prop.table(table(a$factor))*100
summary(a$Consumption)
detach(sample)


#4 查看有几个设备，每个设备的记录起止时�?
start_Date<-as.Date(sample$Start_time)
sample$date<-start_Date
str(sample)

see<-sqldf("select min(date) as start_date, max(date)as end_date,ID from sample group by ID")
see$end_date<-as.Date(see$end_date,origin = "1970-01-01")
see$start_date<-as.Date(see$start_date,origin = "1970-01-01")
str(see)
summary(see$start_date)
summary(see$end_date)
see$S<-as.Date("2016-10-30")
see$E<-as.Date("2017-06-09")
#�?2016�?10�?30日以前就有记录的 和在2017�?6�?9日之后也有记录的 
complete<-subset(see,see$start_date<="2016-10-30" & see$end_date>= "2017-06-09")

#5 挑出晚上 6-12点的电消费数�?
str(sample)
my_data<-sqldf("select * from sample where ID in (select ID from complete)")

# 存档
setwd("C:/Wesley Tao/database/new")
sample$No<-(1:882133)
long_normalize<-subset(sample,diff_time>=0 & duration>1.1)
l_No<-long_normalize$No
all_No<-sample$No
S_No<-setdiff(all_No,l_No)
short_normalize<-subset(sample,No %in% S_No)

write.csv(short_normalize,"short_normalize.csv")
write.csv(long_normalize,"long_normalize.csv")

#6 进行消费按照时间密度分布�?

#7 将密度图按照设备来区�? 时间分布图（挑两个）


