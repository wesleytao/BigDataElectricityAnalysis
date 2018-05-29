#数据整合
#input:data.csv
#output:rawdata.csv

rm()
setwd("C:/Wesley Tao/database")
library(xlsx)
# library(rJava)

data_0<-read.csv("data_0.csv")
data_1<-read.csv("data_1.csv")
data_2<-read.csv("data_2.csv")
data_3<-read.csv("data_3.csv")
data_4<-read.csv("data_4.csv")
data_5<-read.csv("data_5.csv")

data_6<-read.csv("data_6.csv")
data_7<-read.csv("data_7.csv")
data_8<-read.csv("data_8.csv")
data_9<-read.csv("data_9.csv")
data_10<-read.csv("data_10.csv")
data_11<-read.csv("data_11.csv")
data_12<-read.csv("data_12.csv")
data_13<-read.csv("data_13.csv")
data_14<-read.csv("data_14.csv")
data_15<-read.csv("data_15.csv")
data_16<-read.csv("data_16.csv")
data_17<-read.csv("data_17.csv")
names(data_5)<-names(data_1)
names(data_6)<-names(data_1)
names(data_7)<-names(data_1)
names(data_8)<-names(data_1)
names(data_9)<-names(data_1)
names(data_10)<-names(data_1)
names(data_11)<-names(data_1)
names(data_12)<-names(data_1)
names(data_13)<-names(data_1)
names(data_14)<-names(data_1)
names(data_15)<-names(data_1)
names(data_16)<-names(data_1)
names(data_17)<-names(data_1)

rawdata<-rbind(data_0[-7],data_1,data_2,data_3,
      data_4,data_7,data_10,
      data_5[-1,],data_8[-1,],data_11[-1,],
      data_6[-1,],data_9[-1,],data_12[-1,],
      data_13[-1,],data_14[-1,],data_15[-1,],
      data_17[-1,],data_16[-1,]
      )
write.csv(rawdata,"rawdata.csv")




