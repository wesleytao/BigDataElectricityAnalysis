# detect similar_id 
# import:detect_similar_id.csv  &  pics for hour graph

rm(list=ls())
setwd("C:/Wesley Tao/database/new")
library(foreign)
target<-read.csv("pics for hour graph.csv")
pool<-read.csv("detect_similar_id.csv")

pool<-pool[!names(pool) %in% c("X")]
target<-target[!names(target) %in% c("X")]

#6:00-22:00 peak  22:00-6:00 non-peak
str(target)
names(target)<-c("hour","non_peak_users","peak_users")
pool$non_peak_users<-target$non_peak_users
# target[7:22,2:ncol(target)]
cor_pool<-pool[7:22,2:ncol(pool)]
cor_result<-cor(cor_pool)
# cor_pool[cor_pool==0]
cor_result[,13883]
t<-as.data.frame(cor_result)
target<-t[names(t)%in% c("non_peak_users")]
summary(target)
target$user<-dimnames(target)[1]
names(target)<-c("correlation of target","ID")
target<-as.data.frame(target)
write.csv(unlist(target),"correlation.csv")



