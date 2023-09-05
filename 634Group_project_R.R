library(data.table)
library(dplyr)
library(lmtest)
library(ggplot2)
library(stringr)
library(vars)
library(forecast)
library(ggfortify)
library(gridExtra)
library(changepoint)

#Unemployment Rate Data set 
UnemploymentRatebyAgebySex<-read.csv("UnemploymentRatebyAgebySex.csv", stringsAsFactors=T)
summary(UnemploymentRatebyAgebySex)
UnemploymentRatebyAgebySex<-slice(UnemploymentRatebyAgebySex,1:25)
UnemploymentRatebyAgebySex$Time<-UnemploymentRatebyAgebySex$Labour.Force.Status.by.Sex.by.Age.Group..Qrtly.Mar.Jun.Sep.Dec.
UnemploymentRatebyAgebySex$Total<-UnemploymentRatebyAgebySex$X.20
UnemploymentRate<-data.frame(Time= UnemploymentRatebyAgebySex$Time,UnemploymentRate=UnemploymentRatebyAgebySex$Total)
UnemploymentRate<-slice(UnemploymentRate,5:25)

#Crime Number Data set
Crime_Offenders<-fread ("CrimeQuarter.csv",stringsAsFactors=TRUE) 
Crime_Offenders<-na.omit(Crime_Offenders)
colnames(Crime_Offenders)<-c("District","Time","Type","Number")
Crime_Offenders$Time<-str_replace_all(Crime_Offenders$Time," ", "")

#Cost of living Index Data set
Living_Cost<-fread ("Household-living-costs-price-indexesMain.csv",stringsAsFactors=TRUE) 
Living_Cost<-filter(Living_Cost,Living_Cost$nzhec_name=="All groups" & Living_Cost$hlpi=="allhh")
Living_Cost_Rate<-data.frame(Time=Living_Cost$quarter,CostLivingIndex=Living_Cost$change.a)

#Join data
Crime_Offenders<-dplyr::left_join(Crime_Offenders,UnemploymentRate,by="Time")
Crime_Offenders<-dplyr::left_join(Crime_Offenders,Living_Cost_Rate,by="Time")
summary(Crime_Offenders)
#write.csv(Crime_Offenders,'Crime_Offenders.csv')

Crime_Offenders$UnemploymentRate<-as.numeric(Crime_Offenders$UnemploymentRate)
Crime_Offenders$CostLivingIndex<-as.numeric(Crime_Offenders$CostLivingIndex)

summary(Crime_Offenders$Number)
summary(Crime_Offenders$UnemploymentRate)
summary(Crime_Offenders$CostLivingIndex)
is.na(Crime_Offenders$Number)
is.na(Crime_Offenders$UnemploymentRate)
is.na(Crime_Offenders$CostLivingIndex)

shapiro.test(Crime_Offenders$Number)
shapiro.test(Crime_Offenders$UnemploymentRate)
shapiro.test(Crime_Offenders$CostLivingIndex)

#correlation test
cor.test(Crime_Offenders$Number,Crime_Offenders$UnemploymentRate)
cor.test(Crime_Offenders$Number,Crime_Offenders$CostLivingIndex)

ggplot(data=Crime_Offenders, aes(x=Number,y=CostLivingIndex) )+
  geom_point(size=1, alpha=0.5) 
ggplot(data=Crime_Offenders, aes(x=Number,y=UnemploymentRate) )+
  geom_point(size=1, alpha=0.5) 


##Canterbury three type crime regression##
Crime_Offenders_Canterbury_Burglary<-filter(Crime_Offenders,Crime_Offenders$District=="Canterbury" & Crime_Offenders$Type=="Unlawful Entry With Intent/Burglary, Break and Enter")
regCanterburyBurlary<-lm(formula=Crime_Offenders_Canterbury_Burglary$Number~Crime_Offenders_Canterbury_Burglary$CostLivingIndex +Crime_Offenders_Canterbury_Burglary$UnemploymentRate)
summary(regCanterburyBurlary)
plot(regCanterburyBurlary)

ggplot(data=Crime_Offenders_Canterbury_Burglary, aes(x=Number,y=CostLivingIndex) )+
  geom_point(size=1, alpha=0.5) +
  geom_smooth(method=lm) +
  ggtitle("linear regression for Canterbury_Burglary")

Crime_Offenders_Canterbury_Theft<-filter(Crime_Offenders,Crime_Offenders$District=="Canterbury" & Crime_Offenders$Type=="Theft and Related Offences")
regCanterburyTheft<-lm(formula=Crime_Offenders_Canterbury_Theft$Number~Crime_Offenders_Canterbury_Theft$CostLivingIndex + Crime_Offenders_Canterbury_Theft$UnemploymentRate)
summary(regCanterburyTheft)
Crime_Offenders_Canterbury_Robbery<-filter(Crime_Offenders,Crime_Offenders$District=="Canterbury" & Crime_Offenders$Type=="Robbery, Extortion and Related Offences")
regCanterburyRobbery<-lm(formula=Crime_Offenders_Canterbury_Robbery$Number~Crime_Offenders_Canterbury_Robbery$CostLivingIndex + Crime_Offenders_Canterbury_Robbery$UnemploymentRate)
summary(regCanterburyRobbery)

##Wellington three type crime regression##
Crime_Offenders_Wellington_Burglary<-filter(Crime_Offenders,Crime_Offenders$District=="Wellington" & Crime_Offenders$Type=="Unlawful Entry With Intent/Burglary, Break and Enter")
regWellingtonBurlary<-lm(formula=Crime_Offenders_Wellington_Burglary$Number~Crime_Offenders_Wellington_Burglary$CostLivingIndex + Crime_Offenders_Wellington_Burglary$UnemploymentRate)
summary(regWellingtonBurlary)
Crime_Offenders_Wellington_Theft<-filter(Crime_Offenders,Crime_Offenders$District=="Wellington" & Crime_Offenders$Type=="Theft and Related Offences")
regWellingtonTheft<-lm(formula=Crime_Offenders_Wellington_Theft$Number~Crime_Offenders_Wellington_Theft$CostLivingIndex + Crime_Offenders_Wellington_Theft$UnemploymentRate)
summary(regWellingtonTheft)
Crime_Offenders_Wellington_Robbery<-filter(Crime_Offenders,Crime_Offenders$District=="Wellington" & Crime_Offenders$Type=="Robbery, Extortion and Related Offences")
regWellingtonRobbery<-lm(formula=Crime_Offenders_Wellington_Robbery$Number~Crime_Offenders_Wellington_Robbery$CostLivingIndex + Crime_Offenders_Wellington_Robbery$UnemploymentRate)
summary(regWellingtonRobbery)


#Diagnostic plots with Generalized Linear Models (GLM)
autoplot(regCanterburyBurlary, which = 1:6, ncol = 2, label.size = 3,
         colour = "steelblue") + theme_bw()
autoplot(regCanterburyTheft, which = 1:6, ncol = 2, label.size = 3,
         colour = "steelblue") + theme_bw()
autoplot(regCanterburyRobbery, which = 1:6, ncol = 2, label.size = 3,
         colour = "steelblue") + theme_bw()
autoplot(regWellingtonBurlary, which = 1:6, ncol = 2, label.size = 3,
         colour = "steelblue") + theme_bw()
autoplot(regWellingtonTheft, which = 1:6, ncol = 2, label.size = 3,
         colour = "steelblue") + theme_bw()
autoplot(regWellingtonRobbery, which = 1:6, ncol = 2, label.size = 3,
         colour = "steelblue") + theme_bw()


#Crime number compared to the unemployment rate 
Crime_Offenders_compared_unemployment_rate<-aggregate(Crime_Offenders$Number,by=list(Crime_Offenders$Time,Crime_Offenders$District,Crime_Offenders$Type,Crime_Offenders$UnemploymentRate),sum)
colnames(Crime_Offenders_compared_unemployment_rate)<- c("Time","District","Type","UnemploymentRate","Number")
ggplot(Crime_Offenders_compared_unemployment_rate,aes(x=Time,group=District)) + geom_line(aes(y=Number,color="Crime Number"))+geom_line(aes(y=UnemploymentRate,color="UnemploymentRate"))+facet_grid(District~Type)

#Crime number compared to the cost of living index 
Crime_Offenders_compared_unemployment_rate<-aggregate(Crime_Offenders$Number,by=list(Crime_Offenders$Time,Crime_Offenders$District,Crime_Offenders$Type,Crime_Offenders$CostLivingIndex),sum)
colnames(Crime_Offenders_compared_unemployment_rate)<- c("Time","District","Type","CostLivingIndex","Number")
ggplot(Crime_Offenders_compared_unemployment_rate,aes(x=Time,group=District)) + geom_line(aes(y=Number,color="Crime Number"))+geom_line(aes(y=CostLivingIndex,color="CostLivingIndex"))+facet_grid(District~Type)

#Crime number compared to the unemployment rate and the cost of living index 
Crime_Offenders
ggplot(Crime_Offenders,aes(x=Time,group=District)) + geom_line(aes(y=Number,color="Crime Number"))+geom_line(aes(y=CostLivingIndex,color="CostLivingIndex"))+geom_line(aes(y=UnemploymentRate,color="UnemploymentRate"))+facet_grid(District~Type)


#time series
Crime_Offenders_timeseries<-aggregate(Crime_Offenders$Number,by=list(Crime_Offenders$Time,Crime_Offenders$UnemploymentRate,Crime_Offenders$CostLivingIndex),sum)
colnames(Crime_Offenders_timeseries)<-c("Time","CostLivingIndex","UnemploymentRate","Number")
Crime_Offenders_timeseries<-arrange(Crime_Offenders_timeseries,Crime_Offenders_timeseries$Time)

Crime_Offenders_timeseries<-ts(Crime_Offenders_timeseries,start(2017,7),frequency=4)
class(Crime_Offenders_timeseries)
Crime_Offenders_timeseries
#Crime_Offenders_timeseries_data<-data.frame(Time=c(time(Crime_Offenders_timeseries)),Crime_Offenders_timeseries=c(Crime_Offenders_timeseries))
autoplot(Crime_Offenders_timeseries,facets = TRUE) + ggtitle("The Time Series of crime and cost of living Rate, unemplymentRate")
#autoplot(Crime_Offenders_timeseries,facets = FALSE) + ggtitle("The Time Series of Crime and cost of living, unemplyment")

a <- autoplot(ma(Crime_Offenders_timeseries,3)) 
b <- autoplot(ma(Crime_Offenders_timeseries,7)) 
c <- autoplot(ma(Crime_Offenders_timeseries,10))
d <- autoplot(Crime_Offenders_timeseries)
grid.arrange(d,a,b,c,ncol=2)

forecast_Crime_Offenders_timeseries<-forecast(Crime_Offenders_timeseries)
autoplot(forecast_Crime_Offenders_timeseries)+ ggtitle("Forecast")

#change point
cpt.mean(Crime_Offenders_timeseries)
#DOING 
#autoplot(cpt.mean(Crime_Offenders_timeseries),facts=TRUE) 
#autoplot(breakpoints(Crime_Offenders_timeseries ~ 1),colour = 'green')+ggtitle("The Time Series of Crime and cost of living, unemplyment")
