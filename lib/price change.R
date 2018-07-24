#price change
# input KFC_for_12.dta,date_
# main var c(5977,6106,5992,11399,5969,5306,5976,6107,5968,12324)
#supplementvars  c(6034,5984,6040,5991,6186,11149,5383,11537,5991)
# output  18foodprice_unit.csv




library(sqldf)
library(foreign)
rm(list=ls())
setwd("C:/Users/Wesle/Documents/R")

date_check<-read.csv("date_check_for_180items.csv")

mainvars<-c(5977,6106,5992,11399,5969,5306,5976,6107,5968,12324)
supplementvars<-c(6034,5984,6040,5991,6186,11149,5383,11537)
allvars<-c(mainvars,supplementvars)

origin<-read.dta("KFC_for_12.dta")
origin$calendar_date<-as.Date(origin$calendar_date,"%m/%d/%y")
#剔除销量为0的数值
sqldf("select count(*)from origin where unit_sold<=0")#571
origin<-sqldf("select * from origin where unit_sold>0")



sqldf("select count(distinct(item_id)) from origin  ")#180
sqldf("select distinct(item_id) from origin  ")
length(allvars)#18
allvars
#从date check 中挑选出目标的
myvars<-as.data.frame(allvars)#18 个
# item_18<-sqldf("select * from date_check where item_id in (select allvars from myvars) ") 发现大部分都是全年销售的，少部分产品终止了
# write.csv(item_18,"item_18.csv")

item_18<-sqldf("select * from origin where item_id in (select allvars from myvars) ")



# 看价格 day_id store_code_item_id group by  calendar_date,item_id,store_code,sell_price,alc_price
check<-sqldf("select calendar_date,item_id,store_code,sum(unit_sold)as unit_sold,sum(amount)as amount, alc_price,sell_price from item_18 group by calendar_date,item_id,store_code,sell_price,alc_price order by item_id")

# 转化城只有 每天的 P 与 unit——sold
class(check)
check_2<-check
mydata<-sqldf("select calendar_date,item_id,sum(unit_sold) as daily_sold, sum(amount) as sum_amount, sum(amount)/sum(unit_sold) as av_P  from check_2 group by calendar_date,item_id")

#按照item code 进行拆分

sqldf("select count(distinct(item_id)) from mydata")#mydata 18


mylist<-split(mydata[-4],mydata$item_id)#删除的是amount

length(names(mylist))#18


x<-as.data.frame(mylist[1])
names(x)[1]<-"calendar_date"

count<-sqldf("select count(*)from x")
for (i in 2:18){
  cool<-mylist[i]
  temp<-as.data.frame(cool)
  count[i]<-sqldf("select count(*)from temp")
  names(temp)[1]<-"calendar_date"
  x<-merge(x,temp,all=T)#全链接
}

length(names(x))


names(count)<-names(mylist)
count

length(count)

write.csv(x,"18foodprice_unit.csv")


