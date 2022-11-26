setwd("C:/Users/n10141065/Queensland University of Technology/You-Gan Wang - RyanWu(Jinran)/EPSR2022Covid19/EPSR_R2/epsr_revised")
#setwd("C:/Users/wujrt/Queensland University of Technology/You-Gan Wang - RyanWu(Jinran)/EPSR2022Covid19/EPSR_R2/epsr_revised")
data1<-read.csv("loaddata1.csv")
data2<-read.csv("loaddata2.csv")
data1size<-dim(data1)[1]
data2size<-dim(data2)[1]
DataIndex<-c(1:(data1size+data2size))
Data<-as.data.frame(cbind(DataIndex,rbind(data1,data2)))
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

#Data description
names(Data)
#Training set: before 7/1/2021 0:30 (data1 and 1: 61296)
TrainData<-Data[1:(data1size+61296),]
#Test set: after 6/1/2021 0:30 (61297-end)
TestData<-Data[(data1size+61297):(data1size+data2size),]
# Step 1:regression part
TrainX<-as.matrix(TrainData[,c(-2,-3)])
TrainY<-TrainData[,2] 
TestX<-as.matrix(TestData[,c(-2,-3)])
TestY<-TestData[,2] 
LS1<-lm(TrainY~TrainX)
summary(LS1)
##########################
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
#Test set: after 6/1/2021 0:30 (61297-end)
TestData<-Data[(data1size+61297):(data1size+data2size),]
# Step 1:regression part
TrainX<-as.matrix(TrainData[,-2])
TrainY<-TrainData[,2] 
TestX<-as.matrix(TestData[,-2])
TestY<-TestData[,2] 
LS2<-lm(TrainY~TrainX)
summary(LS1)
summary(LS2)
anova(LS1,LS2)

LS1$coefficients
write.csv(LS1$coefficients,"C:/Users/wujrt/Queensland University of Technology/You-Gan Wang - RyanWu(Jinran)/EPSR2022Covid19/EPSR_R2/epsr_revised/ls1.csv")

write.csv(LS2$coefficients,"C:/Users/wujrt/Queensland University of Technology/You-Gan Wang - RyanWu(Jinran)/EPSR2022Covid19/EPSR_R2/epsr_revised/ls2.csv")

