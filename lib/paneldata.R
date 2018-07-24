# 2017 5/7 
# panel data analysis 
# input: panle.csv
#output
# encoding UTF-8

#ln(parent)~DR+ln(GDP)+GOv+urban+ln2+ln3
library(foreign)
rm(list=ls())
setwd("C:/Users/Wesle/Documents/R")
library(plm)
library(mice)
library(MSBVAR)
paneldata<-read.csv("panel.csv",encoding = "GB2312")


names(paneldata)
library(sqldf)
# sqldf("select regionid,year,count(*) from paneldata group by regionid,year having count(*)>1")
# sqldf("select distinct regionid from paneldata")
# temp<-sqldf("select * from paneldata where regionid !=1")

model.new_dependent_ratio<-plm(log(patant)~log(gdp)+gov+new_dependent_ratio+mid+west+log(industry_2)+log(industry_3),data=paneldata,model="within" ,index=c("region","year"))
summary(model.new_dependent_ratio)
fixef(model.new_dependent_ratio)

model.new_dependent_ratio_young<-plm(log(patant)~log(gdp)+gov+new_dependent_ratio_young+mid+west+log(industry_2)+log(industry_3),data=paneldata,model="within" ,index=c("region","year"))
summary(model.new_dependent_ratio_young)
fixef(model.new_dependent_ratio_young)

#okay
model.new_dependent_ratio_old<-plm(log(patant)~log(gdp)+gov+new_dependent_ratio_old+mid+west+log(industry_3),data=paneldata,model="within" ,index=c("region","year"))
summary(model.new_dependent_ratio_old)
fixef(model.new_dependent_ratio_old)


model.dependent_ratio<-plm(log(patant)~log(gdp)+gov+dependent_ratio+mid+west+log(industry_2)+log(industry_3),data=paneldata,model="within" ,index=c("region","year"))
summary(model.dependent_ratio)
fixef(model.dependent_ratio)

model.dependent_ratio_young<-plm(log(patant)~log(gdp)+gov+dependent_ratio_young+mid+west+log(industry_2)+log(industry_3),data=paneldata,model="within" ,index=c("region","year"))
summary(model.dependent_ratio_young)
fixef(model.dependent_ratio_young)

#okay
model.dependent_ratio_old<-plm(log(patant)~log(gdp)+gov+dependent_ratio_old+mid+west+log(industry_2)+log(industry_3),data=paneldata,model="within" ,index=c("region","year"))
summary(model.dependent_ratio_old)
fixef(model.dependent_ratio_old)

#######################################################


model.new_dependent_ratio<-plm(log(patant)~log(gdp)+gov+new_dependent_ratio+mid+west+log(industry_2)+log(industry_3),data=paneldata,model="pooling" ,index=c("region","year"))
summary(model.new_dependent_ratio)

model.new_dependent_ratio_young<-plm(log(patant)~log(gdp)+gov+new_dependent_ratio_young+mid+west+log(industry_2)+log(industry_3),data=paneldata,model="pooling" ,index=c("region","year"))
summary(model.new_dependent_ratio_young)


#okay
model.new_dependent_ratio_old<-plm(log(patant)~log(gdp)+gov+new_dependent_ratio_old+mid+west+log(industry_3),data=paneldata,model="pooling" ,index=c("region","year"))
summary(model.new_dependent_ratio_old)



model.dependent_ratio<-plm(log(patant)~log(gdp)+gov+dependent_ratio+mid+west+log(industry_2)+log(industry_3),data=paneldata,model="pooling" ,index=c("region","year"))
summary(model.dependent_ratio)


model.dependent_ratio_young<-plm(log(patant)~log(gdp)+gov+dependent_ratio_young+mid+west+log(industry_2)+log(industry_3),data=paneldata,model="pooling" ,index=c("region","year"))
summary(model.dependent_ratio_young)


#okay
model.dependent_ratio_old<-plm(log(patant)~log(gdp)+gov+dependent_ratio_old+mid+west+log(industry_2)+log(industry_3),data=paneldata,model="pooling" ,index=c("region","year"))
summary(model.dependent_ratio_old)

