
#regression
#input       18foodprice_unit.csv
#
# encoding UTF-8


library(foreign)
rm(list=ls())
setwd("C:/Users/Wesle/Documents/R")
mydata<-read.csv("18foodprice_unit.csv")

all_names<-names(mydata)
drop<-grep("item_id",all_names,ignore.case=F,fixed=T)
all_numbers<-1:length(all_names)
final<-setdiff(all_numbers,drop)
mydata<-mydata[final[-1]]

# 分析5977 产品
# myvar_5977<-c("5977","5992","5969","5976","6034","5984","6040")
# 
# temp<-myvar_5977
# fetch<-as.numeric(apply(as.data.frame(temp),1,function(x){grep(x,names(mydata),ignore.case=F,fixed=T)}))#从中去取元素下标
# 
# regression<-log(mydata[fetch])#需要取对数
# length(names(regression))

#回归 
# log(1)#0 有些天数只卖出去1个 取对数后就是0 啊
# summary(mydata)
# 
# 
# # regression<-log(mydata[-1])
# class(regression)
# is.infinite(regression)
# 
# summary(regression)
# # 5976 应该删除
# drop<-c("X5976.daily_sold","X5976.av_P")
# bool<-names(regression)%in%drop
# regression<-regression[!bool]
# 
# # inf NA omit
# regression
# 
# 
# na.omit(regression)
# 
# result<-lm(X5977.daily_sold~X5977.av_P+X5992.av_P+X5969.av_P+X6034.av_P+X5984.av_P+X6040.daily_sold,data=na.omit(regression))
# summary(result)

# 试着批量处理

regression<-log(mydata[-1])
regression[sapply(regression,is.infinite)]<-NA

#回归
library(car)
result_5977<-lm(X5977.daily_sold~X5977.av_P+X5992.av_P+X11399.av_P+X5984.av_P+X5991.av_P+X6040.av_P,data=(regression))
summary(result_5977)
vif(result_5977)

result_6106<-lm(X6106.daily_sold~X6106.av_P+X5992.av_P+X5969.av_P+X5968.av_P+X5991.av_P,data=regression)
summary(result_6106)
vif(result_6106)



result_5992<-lm(X5992.daily_sold~X5992.av_P+X11399.av_P+X6107.av_P+X5968.av_P+X6186.av_P,data=regression)
summary(result_5992)
vif(result_5992)

result_11399<-lm(X11399.daily_sold~X11399.av_P+X5977.av_P+X5968.av_P+X6186.av_P+X5383.av_P+X11149.av_P,data=regression)
summary(result_11399)
vif(result_11399)

result_5969<-lm(X5969.daily_sold~X5969.av_P+X5969.av_P+X5306.av_P+X12324.av_P+X11537.av_P+X5991.av_P,data=regression)
summary(result_5969)
vif(result_5969)


result_5306<-lm(X5306.daily_sold~X5306.av_P+X5969.av_P+X5991.av_P,data=regression)
vif(result_5306)

result_6107<-lm(X5306.daily_sold~X5306.av_P+X5992.av_P+X5969.av_P+X5991.av_P,data=regression)
vif(result_6107)

result_5968<-lm(X5968.daily_sold~X5968.av_P+X5992.av_P+X5969.av_P+X12324.av_P,data=regression)
vif(result_5968)

result_12324<-lm(X12324.daily_sold~X12324.av_P+X5992.av_P+X5969.av_P+X6107.av_P+X5968.av_P,data=regression)
vif(result_12324)

summary(result_12324)



