#电力数据填补空缺
# function：可能有缺失值
# date 2017/6/16
# function:  先将数据变型再找出缺失值
# input:Merged_part_1_2.csv &  "NA_need_to_be_filled.csv"
# output:Final.csv
# encoding UTF-8

library(lubridate)
library(foreign)
library(reshape2)
rm(list=ls())
setwd("C:/Wesley Tao/database/new")

mydata<-read.csv("Merged_part_1_2.csv")
mydata<-mydata[-2]
mydata<-mydata[-1]

# names(mydata) [1] "No"          "ID"          "date"        "hour"        "Consumption"
mydata<-mydata[-1]

str(mydata)#
# $ ID         : int  80000089 80000088 80000088 80000088 80000068 80000066 80000085 80000065 80000065 80000084 ...
# $ date       : Factor w/ 249 levels "2016-10-03","2016-10-04",..: 134 132 133 134 106 122 1 26 117 109 ...
# $ hour       : int  7 8 21 7 2 15 16 16 9 13 ...
# $ Consumption: num  320 10 0 10 0 0 20 10 10 20 ...


sample<-mydata
mydata$date<-as.Date(mydata$date)
summary(mydata$date)

#ID & Date
table(mydata$ID,mydata$hour)
library(sqldf)
id_date<-sqldf("select ID,max(date) as end_date,min(date) as start_date from mydata group by ID") 
id_date$start_date<-as.Date(id_date$start_date,origin = "1970/01/01")
id_date$end_date<-as.Date(id_date$end_date,origin = "1970/01/01")
write.csv(id_date,"id_date.csv")
id_date$days<-difftime(id_date$end_date,id_date$start_date,units = "days")
id_date$days<-as.numeric(id_date$days)
sqldf("select Id,days from id_date where days<=50")
# ID days
# 1 20012893    0
# 2 20018958    9
# 3 20018964    4
# 4 80000132    5





# consumpt 
attach(mydata)
summary(mydata$Consumption)
hist(Consumption[Consumption<100])
plot(density(Consumption[Consumption<500]))
plot(density(Consumption[Consumption<100]))

a<-mydata
a$factor[a$Consumption<0]<-"lower than 0"
a$factor[a$Consumption>=0& a$Consumption<10]<-"0-10"
a$factor[a$Consumption>=10& a$Consumption<50]<-"10-50"
a$factor[a$Consumption>=50 & a$Consumption<100]<-"50-100"
a$factor[a$Consumption>=100& a$Consumption<300]<-"over 100"
a$factor[a$Consumption>=300& a$Consumption<2000]<-"over 300"
a$factor[a$Consumption>=2000]<-"over 2000"
table(a$factor)
prop.table(table(a$factor))*100
#剔除极值数据
# mydata<-subset(mydata,mydata$Consumption<2000)



# time & consumpt
library(sqldf)
summary(mydata$hour)
time_con<-sqldf("select avg(Consumption) as mean_c,hour from mydata group by hour")
barplot(time_con$mean_c,names.arg = time_con$hour)
# ID & consumpt

# ID & date
# 春:03月—05月 夏:06月—08月 秋:09月—11月 冬:12月—02月
names(mydata)
mydata$quarter<-quarters(mydata$date)



list_max<-by(mydata$date,mydata$ID,max)
list_min<-by(mydata$date,mydata$ID,min)

md<-melt(sample,id=c("ID","date","hour"),na.rm = FALSE)
newdata<-dcast(md,ID+date~variable+hour,sum,fill=(-1),drop=FALSE)
newdata[newdata==-1]<-NA
class(newdata)
library(mice)
md.pattern(newdata)

sampleele[1:10,]
sampleweather[1:10,]
write.csv(newdata,"NA_need_to_be_filled.csv")
