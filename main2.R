setwd("C:/Users/wujrt/Queensland University of Technology/You-Gan Wang - RyanWu(Jinran)/EPSR2022Covid19/EPSR_R2/epsr_revised")
#setwd("C:/Users/wuj24/OneDrive - Queensland University of Technology/RyanWu(Jinran)/EPSR2022Covid19/EPSR_R2/epsr_revised")
data1<-read.csv("loaddata1.csv")
data2<-read.csv("loaddata2.csv")
data1size<-dim(data1)[1]
data2size<-dim(data2)[1]
DataIndex<-c(1:(data1size+data2size))
Data<-as.data.frame(cbind(DataIndex,rbind(data1,data2)))
Data$Covid<-c(rep(0,data1size+38874),rep(1,(data2size-38874)))
Data$WorkDay<-(rep(c(rep(1,48*1),rep(0,48*2),rep(1,48*4)),floor(length(Data$Demand)/(48*7))))
##CycleFactor 
Data$DailySin<-sin(2*pi*DataIndex/(1*48))
Data$DailyCos<-cos(2*pi*DataIndex/(1*48))
Data$HalfDailySin<-sin(2*pi*DataIndex/(1*24))
Data$HalfDailyCos<-cos(2*pi*DataIndex/(1*24))
Data$WeeklySin<-sin(2*pi*DataIndex/(7*48))
Data$WeeklyCos<-cos(2*pi*DataIndex/(7*48))
Data$MonthlySin<-sin(2*pi*DataIndex/(365.25/12*48))
Data$MonthlyCos<-cos(2*pi*DataIndex/(365.25/12*48))
Data$SeasonalSin<-sin(2*pi*DataIndex/(365.25/4*48))
Data$SeasonalCos<-cos(2*pi*DataIndex/(365.25/4*48))
Data$YearlySin<-sin(2*pi*DataIndex/(365.25*48))
Data$YearlyCos<-cos(2*pi*DataIndex/(365.25*48))
#InterationFactors: LockDown
Data$LockDown_WorkDay<-Data$LockDown*Data$WorkDay
Data$LockDown_DailySin<-Data$LockDown*Data$DailySin
Data$LockDown_DailyCos<-Data$LockDown*Data$DailyCos
Data$LockDown_HalfDailySin<-Data$LockDown*Data$HalfDailySin
Data$LockDown_HalfDailyCos<-Data$LockDown*Data$HalfDailyCos
Data$LockDown_WeeklySin<-Data$LockDown*Data$WeeklySin
Data$LockDown_WeeklyCos<-Data$LockDown*Data$WeeklyCos
Data$LockDown_MonthlySin<-Data$LockDown*Data$MonthlySin
Data$LockDown_MonthlyCos<-Data$LockDown*Data$MonthlyCos
Data$LockDown_SeasonalSin<-Data$LockDown*Data$SeasonalSin
Data$LockDown_SeasonalCos<-Data$LockDown*Data$SeasonalCos 
Data$LockDown_YearlySin<-Data$LockDown*Data$YearlySin
Data$LockDown_YearlyCos<- Data$LockDown*Data$YearlyCos
#Data description
names(Data)
#Training set: before 7/1/2021 0:30 (data1 and 1: 61296)
TrainData<-Data[1:(data1size+61296),]
#Test set: after 7/1/2021 0:30 (61297-end)
TestData<-Data[(data1size+61297):(data1size+data2size),]
# Step 1:regression part
TrainX<-as.matrix(TrainData[,-2])
TrainY<-TrainData[,2] 
TestX<-as.matrix(TestData[,-2])
TestY<-TestData[,2] 
LS<-lm(TrainY~TrainX)
summary(LS)
#Step 2: obtain residuals
TrainFits<-cbind(rep(1,length(TrainY)),TrainX)%*%(LS$coefficients)
TrainResiduals<-TrainY-TrainFits
TestPreds<-cbind(rep(1,length(TestY)),TestX)%*%(LS$coefficients)
TestResiduals<-TestY-TestPreds
#Step 3: temporal pattern diagnostics
write.csv(TrainResiduals,"TrainRes.csv")
acf(TrainResiduals,lag.max = 240)
pacf(TrainResiduals,lag.max = 240)
### model 22222
#Step 4: Temporal pattern modelling
library(forecast)
TemporalModel<-auto.arima(TrainResiduals,ic="aic",trace = TRUE,allowmean = FALSE ,max.order = 20)
summary(TemporalModel)
plot(TemporalModel$residuals)

# ARMA(3,1)
# AR1: 0.6600  AR2: 0.8179  AR3: -0.5524  MA1: 0.9233

#Step 5:Final predictions
WholeRes<-rbind(TrainResiduals,TestResiduals)
lengthres<-dim(WholeRes)[1]
StructureRes<-cbind(WholeRes[1:(lengthres-3)],WholeRes[2:(lengthres-2)],
                    WholeRes[3:(lengthres-1)],WholeRes[4:lengthres])
ARPred<-t(TemporalModel$coef[c(3,2,1)]%*%t(StructureRes[,1:3]))
MARes<-StructureRes[,4]-ARPred
MAX<-MARes[1:(dim(MARes)[1]-1)]
MAY<-MARes[2:dim(MARes)[1]]
MAPred<-MAX*TemporalModel$coef[4]
FinalTemporalPreds<-ARPred[2:(length(MAPred)+1)]+MAPred
TestTemporalPreds<-FinalTemporalPreds[(length(FinalTemporalPreds)-length(TestPreds)+1):
                                        length(FinalTemporalPreds)]
FinalPreds<-TestPreds+TestTemporalPreds
head(FinalPreds)
head(TestY)
plot(TestY,type = "l",col=1)
points(FinalPreds,type = "l",col=2)
# Error indicators
#General test results
LockdownIndex<-which(TestX[,2]==1)
# Predictions without temporal correlation
MRE1<-mean(abs(TestY-TestPreds)/TestY)
MRE1
MAE1<-mean(abs(TestY-TestPreds))
MAE1
RMSE1<-sqrt(mean((TestY-TestPreds)^2))
RMSE1
# Predictions with temporal correlation
MRE2<-mean(abs(TestY-FinalPreds)/TestY)
MRE2
MAE2<-mean(abs(TestY-FinalPreds))
MAE2
RMSE2<-sqrt(mean((TestY-FinalPreds)^2))
RMSE2

## Lockdown
MRE11<-mean(abs(TestY[LockdownIndex]-TestPreds[LockdownIndex])/TestY[LockdownIndex])
MRE11
MAE11<-mean(abs(TestY[LockdownIndex]-TestPreds[LockdownIndex]))
MAE11
RMSE11<-sqrt(mean((TestY[LockdownIndex]-TestPreds[LockdownIndex])^2))
RMSE11
MRE21<-mean(abs(TestY[LockdownIndex]-FinalPreds[LockdownIndex])/TestY[LockdownIndex])
MRE21
MAE21<-mean(abs(TestY[LockdownIndex]-FinalPreds[LockdownIndex]))
MAE21
RMSE21<-sqrt(mean((TestY[LockdownIndex]-FinalPreds[LockdownIndex])^2))
RMSE21

## Result output
## Result output
##Training results
TrainingResults<-LS$fitted.values+TemporalModel$fitted
TestResults<-FinalPreds
write.csv(TrainingResults,"resultscovid19_train.csv")
write.csv(TestResults,"resultscovid19_test.csv")