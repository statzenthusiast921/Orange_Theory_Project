#Orange Theory Data Analysis
#Last Updated: 01/26/2020

#Upload data
#Getting Started
setwd("/Users/jonzimmerman/Desktop/Data Projects/OT Data Analysis")
install.packages("readxl")
library(readxl)

#Load in data
OT_data=as.data.frame(read_excel("OrangeTheoryData.xlsx",sheet=1))
dim(OT_data)
#143    49
colnames(OT_data)

#Similar to PROC CONTENTS IN SAS
str(OT_data)


#Correlations
cor(OT_data$Splat,OT_data$OrangeMin)
cor(OT_data$Splat,OT_data$RedMin)
cor(OT_data$Splat,OT_data$AvgHR)
cor(OT_data$Calories,OT_data$OrangeMin)
cor(OT_data$Calories,OT_data$AvgHR)



#Change several columns to factors
OT_data$Location=as.factor(OT_data$Location)
OT_data$Time=as.factor(OT_data$Time)
OT_data$Trainer=as.factor(OT_data$Trainer)
OT_data$Weekday=as.factor(OT_data$Weekday)

OT_data$Weekday=factor(OT_data$Weekday,levels=c("Thursday","Sunday","Monday","Tuesday","Wednesday","Friday","Saturday"))
OT_data$Time=factor(OT_data$Time,levels=c("7:45AM","8:30AM","8:45AM","9:30AM","9:45AM","10:30AM","11:30AM","1:00PM","2:00PM","4:15PM","4:30PM","5:30PM","5:45PM","6:45PM"))

#Make some plots
install.packages("ggplot2")
library(ggplot2)
ggplot(data=OT_data,aes(y=OT_data$Calories,x=as.factor(OT_data$Location)))+geom_boxplot(aes(col=as.factor(OT_data$Location)))
ggplot(data=OT_data,aes(y=OT_data$Calories,x=as.factor(OT_data$DaysSince)))+geom_boxplot(aes(col=as.factor(OT_data$DaysSince)))
ggplot(data=OT_data,aes(y=OT_data$Calories,x=as.factor(OT_data$Weekday)))+geom_boxplot(aes(col=as.factor(OT_data$Weekday)))
ggplot(data=OT_data,aes(y=OT_data$Calories,x=as.factor(OT_data$Morn_Even)))+geom_boxplot(aes(col=as.factor(OT_data$Morn_Even)))


ggplot(data=OT_data,aes(y=OT_data$Splat,x=as.factor(OT_data$Location)))+geom_boxplot(aes(col=as.factor(OT_data$Location)))
ggplot(data=OT_data,aes(y=OT_data$Splat,x=as.factor(OT_data$DaysSince)))+geom_boxplot(aes(col=as.factor(OT_data$DaysSince)))
ggplot(data=OT_data,aes(y=OT_data$Splat,x=as.factor(OT_data$Weekday)))+geom_boxplot(aes(col=as.factor(OT_data$Weekday)))
ggplot(data=OT_data,aes(y=OT_data$Splat,x=as.factor(OT_data$Morn_Even)))+geom_boxplot(aes(col=as.factor(OT_data$Morn_Even)))


ggplot(data=OT_data,aes(y=OT_data$AvgHR,x=as.factor(OT_data$Location)))+geom_boxplot(aes(col=as.factor(OT_data$Location)))
ggplot(data=OT_data,aes(y=OT_data$AvgHR,x=as.factor(OT_data$DaysSince)))+geom_boxplot(aes(col=as.factor(OT_data$DaysSince)))
ggplot(data=OT_data,aes(y=OT_data$AvgHR,x=as.factor(OT_data$Weekday)))+geom_boxplot(aes(col=as.factor(OT_data$Weekday)))
ggplot(data=OT_data,aes(y=OT_data$AvgHR,x=as.factor(OT_data$Morn_Even)))+geom_boxplot(aes(col=as.factor(OT_data$Morn_Even)))



ggplot(data=OT_data,aes(y=OT_data$MaxHR,x=as.factor(OT_data$Location)))+geom_boxplot(aes(col=as.factor(OT_data$Location)))
ggplot(data=OT_data,aes(y=OT_data$MaxHR,x=as.factor(OT_data$DaysSince)))+geom_boxplot(aes(col=as.factor(OT_data$DaysSince)))
ggplot(data=OT_data,aes(y=OT_data$MaxHR,x=as.factor(OT_data$Weekday)))+geom_boxplot(aes(col=as.factor(OT_data$Weekday)))
ggplot(data=OT_data,aes(y=OT_data$MaxHR,x=as.factor(OT_data$Morn_Even)))+geom_boxplot(aes(col=as.factor(OT_data$Morn_Even)))

#Table for Classes per Studio
StudioVisit=as.data.frame(table(OT_data$Location))
colnames(StudioVisit)=c("Location","LocationVisitCount")
#Change the small visit locations to single group "Other"
OT_data=merge(x=OT_data,y=StudioVisit,by="Location",all.x=TRUE)
dim(OT_data)
#143 49

OT_data$Location2=ifelse(OT_data$Location %in% c("Jersey City, NJ","Rosslyn, VA","Mount Vernon Triangle/Shaw, DC","Thomas Circle, DC"),"Other",OT_data$Location)
table(OT_data$Location2)

OT_data$Location2=factor(OT_data$Location2,levels=c("Other","14th Street, DC","Dunn Lorring, VA","Foggy Bottom, DC","Navy Yard, DC"))



#Poisson Regression
attach(OT_data)
model1=glm(Splat~AvgHR+MaxHR+I(Location2)+DaysSince+ClassLength+Calories, family="poisson",data=OT_data)
summary(model1)

model2=glm(Calories~I(Location2)+AvgHR+DaysSince+ClassLength+Splat, family="poisson",data=OT_data)
summary(model2)

#Gamma Regression
model3=glm(CAL_MIN~AvgHR+DaysSince+Twelve_or_Above+Splat+I(Location2),family="Gamma",data=OT_data)
summary(model3)

#Check for over-disperson
with(model3, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))


#-----TOP 5 / BOTTOM 5 SPLAT ----#
table_calories=OT_data[,c("Calories","Splat","Date","Location","Trainer")]
dim(table_calories)
table_splat=OT_data[,c("Splat","Calories","Date","Location","Trainer")]
dim(table_splat)

bot_spl=head(table_splat[order(table_splat$Splat),],n=5)
top_spl=head(table_splat[order(table_splat$Splat,decreasing = TRUE),],n=5)

bot_cal=head(table_calories[order(table_calories$Calories),],n=5)
top_cal=head(table_calories[order(table_calories$Calories,decreasing = TRUE),],n=5)

