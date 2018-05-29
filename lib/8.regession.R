# regession analysis
# import:"Non_peak_users.csv" "merged_peak_users.csv" “detect_similar_id.csv"
# export:
#coding UTF-8
rm(list=ls())
setwd("C:/Wesley Tao/database/new")
library(foreign)
library(reshape2)
Non_peak<-read.csv("Non_peak_users.csv")
merge_peak<-read.csv("merged_peak_users.csv")
summary(Non_peak$consumption)
plot(density(Non_peak$consumption))
summary(merge_peak$consumption)
merged_data<-rbind(Non_peak,merge_peak)
non_peakdata<-as.Date(Non_peak$date_time)
peakdata<-as.Date(merge_peak$date_time)

#regression 
options(digits = 10)
# 居民用户分时峰谷时段划分为：峰时段(6-22时)，谷时段(22时-次日6时);  以上标准自2000年11月15日起执行。 
names(Non_peak)
#　一、单一制：峰时段(6-22时)，谷时段(22时-次日6时);
# summary(merged_data$marked)
#marked 峰谷电价的用户设为1 
#峰时段 设置为0 谷时段设置为1
merged_data$hour<-as.POSIXlt(merged_data$date_time)$hour
summary(merged_data$hour)
merged_data$hour<-as.integer(merged_data$hour)
str(merged_data)
merged_data$marked_t<-NA
merged_data$marked_t[merged_data$hour>=6 & merged_data$hour<22]<-0 #峰
merged_data$marked_t[merged_data$hour<6 | merged_data$hour>=22]<-1 #谷
boxplot(consumption~marked,data=merged_data,main="Consumption of two groups")

library(sm)
summary(merged_data$consumption)
naomitmerged<-na.omit(merged_data)
# attach(naomitmerged)
# sm.density.compare(naomitmerged$consumption,naomitmerged$marked,xlab="hourly electricity consumption")
# markfactor<-factor(naomitmerged$marked,levels=c(0,1),labels=c("0 Non-peak Users","1 Peak Users"))
# colfill<-c(2:(1+length(levels(markfactor))))
# legend(locator(1),levels(markfactor),fill=colfill)
# 
# plot(density(Non_peak$consumption))
# sum(is.na(merge_peak))
# t<-na.omit(merge_peak)
# plot(density(t$consumption))
summary(peakdata)
summary(non_peakdata)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2016-10-03" "2016-12-18" "2017-02-16" "2017-02-13" "2017-04-14" "2017-06-09" 
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2016-01-28" "2016-03-22" "2016-04-04" "2016-03-31" "2016-04-17" "2016-04-30" 
merged_data$t<-as.Date(merged_data$date_time)
t<-subset(merged_data,(merged_data$t>="2016-01-28" & merged_data$t<="2016-04-30") || (merged_data$t>="2017-01-28"&merged_data$t<="2017-04-30"))
merged_data<-t

library(vioplot)
x1<-naomitmerged$consumption[naomitmerged$marked==1]
x0<-naomitmerged$consumption[naomitmerged$marked==0]
vioplot(x0,x1,names=c("Non-peak Users","Peak Users"),col = "black")
title("Violin Plots of Hourly Consumption Distribution of 2 Groups")

summary(mtcars)
merged_data$hour<-as.factor(merged_data$hour)
means<-aggregate(merged_data$consumption,by=list(merged_data$hour),FUN=mean)
library(sqldf)
pics<-sqldf("select avg(consumption) as avg_con,hour,marked from t group by hour,marked")
# names(pics)
library(reshape2)
md<-melt(pics,id=c("hour","marked"))
t<-dcast(md,hour~marked)
write.csv(t,"pics for hour graph.csv")

barplot(merged_data$consumption,by=list(merged_data$marked),FUN=mean)
list(merged_data$marked)
?barplot


summary(merged_data)
write.csv(merged_data,"merged_data.csv")
# merged_data$tem_2<(merged_data$tem*merged_data$tem)
library(plm)
names(merged_data)
merged_data<-plm.data(merged_data,c("ID","date_time"))
# summary(merged_data)
options(digits=2)
summary(merged_data$hour)
as.character(merged_data$date_time)
str(merge_peak)

# price concern
str(merged_data)
merged_data<-merged_data[!names(merged_data) %in% c("X")]
#因为数据仅仅只有3个月不足以支持跨区区分






# [1] 133737
table(merged_data$marked_t,merged_data$hour)

# 21-22 & 22-23 comparison 
t<-subset(merged_data,merged_data$hour<=22 & merged_data$hour>=21 )
t$consumption[t$consumption==0]<-0.000000000001
summary(log(t$consumption))
reg_oneway<-plm(log(consumption)~tem+I(tem^2)+humidity+marked_t+pressure+marked_t:marked,data=t,model="within")
summary(reg_oneway)


reg_1<-plm(consumption~hour+I(hour^2)+tem+I(tem^2)+humidity+pressure,data=subset(t,marked=1),model="within")
summary(reg_1)




reg_twoway<-plm(consumption~tem+I(tem^2)+humidity+pressure+marked_t+marked+marked_t:marked,data=t,model="within",effect = "twoway")

?plm

