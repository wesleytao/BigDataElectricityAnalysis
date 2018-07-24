##drawing.pics
rm(list=ls())
library(foreign)
library(ggplot2)
library(sqldf)
setwd("C:/Users/Wesle/Documents/R")
origin<-read.dta("KFC_for_12.dta")
origin$calendar_date<-as.Date(origin$calendar_date,"%m/%d/%y")
origin$store_code<-factor(origin$store_code)



date_check_all<-read.csv("date_check_for_180items.csv")


mainvars<-c(5977,6106,5992,11399,5969,5306,5976,6107,5968,12324)
supplementvars<-c(6034,5984,6040,5991,6186,11149,5383,11537)
myvars<-c(mainvars,supplementvars)

just_for_fun<-c(12351,11599,12815,12811,6023,5979)
allvars<-as.data.frame(c(myvars,just_for_fun))

time_check<-sqldf("select * from date_check_all where item_id in (select * from allvars)")
write.csv(time_check,"new_gant_view.csv")

categroy_revenue<-sqldf("select sum(amount)as total_revenue , category_desc,store_code from origin group by category_desc,store_code")
write.csv(categroy_revenue,"category_revenue.csv")

library(ggplot2)
attach(categroy_revenue)
ggplot(data=categroy_revenue,aes(x=total_revenue))+
  geom_histogram()
 



