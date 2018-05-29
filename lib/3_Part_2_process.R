#电力数据时间规整 part_2
# function：因为时间跨度不一致，有些超过一小时或者短于一小时的时间，进行处理
# date 2017/6/16
# function:  thoses need normalize: period exceed 1 
# input:Part_2_need_edited.csv
# output:Part_2.csv
# encoding UTF-8

library(lubridate)
library(foreign)
rm(list=ls())
setwd("C:/Wesley Tao/database/new")


mydata<-read.csv("Part_2_need_edited.csv")
names(mydata)
mydata<-mydata[-1]
mydata<-mydata[-1]
summary(mydata$duration)
subset(mydata,duration>24)
sample<-mydata

sample$delta_s_time<-difftime(sample$s_up,sample$Start_time,unit="hours")
sample$delta_e_time<-difftime(sample$End_time,sample$e_down,unit="hours")
# options(digits = 7)
# t<-sample$delta_e_time+sample$delta_s_time
# sample$duration-t

sample$newAvgC<-(sample$newC)/(sample$duration)

segment_time<-function(no,ID,s_down,delta_s,delta_e,avg_c,n){
No<-rep(no,n)
ID<-rep(ID,n)
delta_t<-c(delta_s,rep(1,n-2),delta_e)
date<-as.character(as.POSIXct(s_down)+hours(0:(n-1)))
avg_con<-rep(avg_c,n)
res<-data.frame(No,ID,date,avg_con,delta_t)
return(res)
}
#example 
# segment_time(no="1338",ID="80000089",s_down = "2016-12-01 15:00:00",n=3,avg_c =0,delta_s = "0.5166667",delta_e = "0.45" )

sample$n<-sample$diff_time+2
sample$s_down<-floor_date(as.POSIXct(sample$Start_time),unit="hours")
sample$No<-(1:101399)
names(sample)

# [1] "ID"           "Start_c"      "End_c"        "Consumption"  "Start_time"   "End_time"     "s_up"         "e_down"      
# [9] "duration"     "avg_c"        "diff_time"    "newC"         "date"         "delta_s_time" "delta_e_time" "n"           
# [17] "s_down"       "No"           "newAvgC"  
# segment_time<-function(no,ID,s_down,delta_s,delta_e,avg_c,n)
a<-sample[c("No","ID","s_down","delta_s_time","delta_e_time","newAvgC","n")]

out<-segment_time(no=a[1,1],ID=a[1,2],s_down=a[1,3],delta_s=a[1,4],delta_e=a[1,5],avg_c=a[1,6],n=a[1,7])

total_obs<-dim(sample)[1]
for(i in 2:total_obs) {
  
  new<-segment_time(no=a[i,1],ID=a[i,2],s_down=a[i,3],delta_s=a[i,4],delta_e=a[i,5],avg_c=a[i,6],n=a[i,7])
  out<-rbind(out,new)
}

names(out)
sample<-out


sample$Consumption<-as.numeric(sample$avg_con*sample$delta_t)
sample$hour<-as.POSIXlt(sample$date)$hour#???տ?ʼʱ?????ڵ?ʱ??
sample$date<-as.Date(sample$date)
Part_2<-sample[c("No","ID","date","hour","Consumption")]
write.csv(Part_2,"part_2.csv")

rm(list=ls())
Part_1<-read.csv("Part_1.csv")
Part_2<-read.csv("part_2.csv")
a<-Part_1
names(a)
names(Part_2)

class(a$No)
class(a$ID)
class(a$date)#factor
class(a$hour)
class(a$Consumption)#integer

class(Part_2$No)
class(Part_2$ID)
class(Part_2$date)#date
class(Part_2$hour)#
class(Part_2$Consumption)#numeric

a$date<-as.Date(a$date)
a$Consumption<-as.numeric(a$Consumption)
Merged_part_1_2<-rbind(a,Part_2)
# No	ID	date	hour	Consumption

write.csv(Merged_part_1_2,"Merged_part_1_2.csv")
mydata<-read.csv("Merged_part_1_2.csv")

library(sqldf)
names(mydata)
mydata<-mydata[-1]
mydata<-mydata[-1]
mydata<-mydata[-1]
names(mydata)
newdata<-sqldf("select ID,date,hour,sum(Consumption)as Consumption from mydata group by ID,date,hour")
write.csv(newdata,"Merged_part_1_2.csv")
