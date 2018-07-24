######################################################################
library(foreign)
rm(list=ls())
setwd("C:/Users/Wesle/Documents/R")
library(plm)
library(mice)
library(MSBVAR)
paneldata<-read.csv("consumption.csv",encoding = "GB2312")
names(paneldata)
library(sqldf)

names(paneldata)
model.new_dependent_ratio<-plm(log(consumption)~log(gdp)+gov+new_dependent_ratio,data=paneldata,model="within" ,index=c("regionid","year"))
summary(model.new_dependent_ratio)


model.new_dependent_ratio_young<-plm(log(consumption)~log(gdp)+gov+new_dependent_ratio_young,data=paneldata,model="within" ,index=c("regionid","year"))
summary(model.new_dependent_ratio_young)


model.new_dependent_ratio_old<-plm(log(consumption)~log(gdp)+gov+new_dependent_ratio_old,data=paneldata,model="within" ,index=c("regionid","year"))
summary(model.new_dependent_ratio_old)


model.dependent_ratio<-plm(log(consumption)~log(gdp)+gov+dependent_ratio,data=paneldata,model="within" ,index=c("regionid","year"))
summary(model.dependent_ratio)

model.dependent_ratio_young<-plm(log(consumption)~log(gdp)+gov+dependent_ratio_young,data=paneldata,model="within" ,index=c("regionid","year"))
summary(model.dependent_ratio_young)


model.dependent_ratio_old<-plm(log(consumption)~log(gdp)+gov+dependent_ratio_old,data=paneldata,model="within" ,index=c("regionid","year"))
summary(model.dependent_ratio_old)
