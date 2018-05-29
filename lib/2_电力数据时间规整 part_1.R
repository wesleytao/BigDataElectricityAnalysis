# 电力数据时间规整 part_1
# date 2017/6/17
# function: sort_electricity data
# input:long_normalize.csv, short_normalize.csv
# output: Part_1.csv (clean)  Part_2_edit.csv(dirty)
# encoding UTF-8


library(lubridate)
library(foreign)
library(sqldf)
rm(list=ls())
setwd("C:/Wesley Tao/database/new")

#long:时长超过1.1个小时，并且diff_time>0 也就是结束时间的floor减去起始时间的ceiling大于0 横跨两个???
long<-read.csv("long_normalize.csv",encoding = "UTF-8")
#short：时长低???1.1小时，或者diff_time<0???  其中要考虑 diff_tiem>0 但是时长低于1.1小时的情???
short<-read.csv("short_normalize.csv",encoding = "UTF-8")


short_sample<-short
short_sample$Smin<-as.POSIXlt(short_sample$Start_time)$min
# summary(short_sample$Smin)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   59.00   59.00   58.33   59.00   59.00 
less_than_10<-subset(short_sample,Smin<=10)#140
more_than_10<-subset(short_sample,10<Smin & Smin<=50& diff_time>=0)#18500

temp<-setdiff(short_sample$No,more_than_10$No)
temp<-setdiff(temp,less_than_10$No)
more_tan_50<-subset(short_sample, No %in% temp)#817732

#第一部分 less_than_10 直接取开始和时间的floor 
names(less_than_10)
short_1<-less_than_10[c("No","ID","newC","Start_time")]
s_d<-floor_date(as.POSIXlt(short_1$Start_time,origin="1970/01/01 0:00"),"hour")
short_1$hour<-s_d$hour
short_1$date<-as.Date(s_d)
short_1$Start_time<-as.character(short_1$Start_time)
short_1<-sqldf("select no,ID,date,hour,newC as Consumption from short_1")


#第二部分最难处理因为横跨两个时???  need to be merge into long
more_than_10<-more_than_10[-15]
more_than_10<-more_than_10[-15]
names(more_than_10)
names(long)
need_further_procees<-rbind(more_than_10,long)



#third part: start_time>50  ceiling
short_3<-more_tan_50[c("No","ID","newC","Start_time")]
s_up<-ceiling_date(as.POSIXlt(short_3$Start_time,origin="1970/01/01 0:00"),"hour")
short_3$hour<-s_up$hour
short_3$date<-as.Date(s_up)
short_3$Start_time<-as.character(short_3$Start_time)
short_3<-sqldf("select no,ID,date,hour,newC as Consumption from short_3")

dont_need_further_process<-rbind(short_1,short_3)
write.csv(dont_need_further_process,"Part_1.csv")

write.csv(need_further_procees,"Part_2_need_edited.csv")

