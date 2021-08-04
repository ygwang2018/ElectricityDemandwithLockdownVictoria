## Noa Levi 
## Demand Forecasting and Estimation of COVID19 Lockdown Impacts

setwd("C:/Users/wujrt/Queensland University of Technology/You-Gan Wang - RyanWu(Jinran)/PowerLoadPred/Loadcovid_ryan22072021")

#setwd("C:/Users/n10141065/Queensland University of Technology/You-Gan Wang - RyanWu(Jinran)/PowerLoadPred/Loadcovid_ryan22072021")

library(forecast)

library(readxl)

Data<-read_xlsx("Vic_dataset_ryan.xlsx",sheet=1)

TIME<-c(1:62688)

Data<-as.data.frame(cbind(TIME,Data))

##CycleFactor 

DailySin<-sin(2*pi*TIME/(1*48))

DailyCos<-cos(2*pi*TIME/(1*48))

HalfDailySin<-sin(2*pi*TIME/(1*24))

HalfDailyCos<-cos(2*pi*TIME/(1*24))

WeeklySin<-sin(2*pi*TIME/(7*48))

WeeklyCos<-cos(2*pi*TIME/(7*48))

MonthlySin<-sin(2*pi*TIME/(365/12*48))

MonthlyCos<-cos(2*pi*TIME/(365/12*48))

SeasonalSin<-sin(2*pi*TIME/(365/4*48))

SeasonalCos<-cos(2*pi*TIME/(365/4*48))

YearlySin<-sin(2*pi*TIME/(365*48))

YearlyCos<-cos(2*pi*TIME/(365*48))

CycleFactor<-as.data.frame(cbind(TIME,DailySin,DailyCos,HalfDailySin,HalfDailyCos, WeeklySin,WeeklyCos,MonthlySin,
                                 MonthlyCos,SeasonalSin,SeasonalCos,YearlySin,YearlyCos))

Lockdown<-Data$LockDown

#DayTime:1 (6am-18pm), NightTime:0 (else)

#DayTime<-rep(c(rep(0, 12), rep(1, 24), rep(0, 12)),length(Data$Demand)/48)

#WorkDay: 1 (Monday-Friday), Weekend: 0 (else)

WorkDay<-c(rep(rep(c(rep(1,48*5),rep(0,48*2))),floor(length(Data$Demand)/(48*7))),rep(1,48*4))

#plot(ModelNoInteration$residuals)

# Factor with Interation

Lockdown_HalfDailySin<-Lockdown*HalfDailySin

Lockdown_HalfDailyCos<-Lockdown*HalfDailyCos

Lockdown_WorkDay<-Lockdown*WorkDay

Lockdom_DailySin<-Lockdown*DailySin

Lockdom_DailyCos<-Lockdown*DailyCos

Lockdom_WeeklySin<-Lockdown*WeeklySin

Lockdom_WeeklyCos<-Lockdown*WeeklyCos

#The entry ban takes effect from 9pm AEDT Friday, 20 March 2020, with exemptions only for Australian citizens, 

#permanent residents and their immediate family, including spouses, legal guardians and dependants.


CovidImpact<-c(rep(0,38849),rep(1,23839))

XInteraction<-cbind(TIME,DailySin,DailyCos,HalfDailySin,HalfDailyCos, WeeklySin,WeeklyCos,MonthlySin,
                    MonthlyCos,SeasonalSin,SeasonalCos,YearlySin,YearlyCos,Lockdown,WorkDay,
                    Lockdown_HalfDailySin, Lockdown_HalfDailyCos,Lockdown_WorkDay,Lockdom_DailySin,
                    Lockdom_DailyCos,Lockdom_WeeklySin,Lockdom_WeeklyCos, CovidImpact)
                    
ModelInteration<-auto.arima(Data$Demand,xreg = as.matrix(XInteraction))

summary(ModelInteration)

###Plot for the lockdown change

##Daily change

DailyChange<-ModelInteration$coef[c(9, 10, 11, 12, 21, 23, 24, 26, 27, 30)]

Time<-c(1:48)

DailySin<-sin(2*pi*Time/(1*48))

DailyCos<-cos(2*pi*Time/(1*48))

HalfDailySin<-sin(2*pi*Time/(1*24))

HalfDailyCos<-cos(2*pi*Time/(1*24))

#Lockdown: Yes

LockDown<-rep(1,48)

CovidImpact<-c(rep(1,48))

Lockdown_DailySin<-LockDown*DailySin

Lockdown_DailyCos<-LockDown*DailyCos

Lockdown_HalfDailySin<-LockDown*HalfDailySin

Lockdown_HalfDailyCos<-LockDown*HalfDailyCos

DailyMatrixYes<-as.matrix(rbind(DailySin, DailyCos,HalfDailySin, HalfDailyCos, LockDown, 
                                Lockdown_HalfDailySin,Lockdown_HalfDailyCos, Lockdown_DailySin,
                                Lockdown_DailyCos, CovidImpact))

DailyChangeLockdownYes<-c(as.array(DailyChange)%*%DailyMatrixYes)

#Lockdown: No

LockDown<-rep(0, 48)

CovidImpact<-c(rep(1,48))

Lockdown_DailySin<-LockDown*DailySin

Lockdown_DailyCos<-LockDown*DailyCos

Lockdown_HalfDailySin<-LockDown*HalfDailySin

Lockdown_HalfDailyCos<-LockDown*HalfDailyCos

DailyMatrixNo<-as.matrix(rbind(DailySin, DailyCos,HalfDailySin, HalfDailyCos, LockDown, 
                               Lockdown_HalfDailySin,Lockdown_HalfDailyCos, Lockdown_DailySin,
                               Lockdown_DailyCos, CovidImpact))

DailyChangeLockdownNo<-c(as.array(DailyChange)%*%DailyMatrixNo)

#No covid

LockDown<-rep(0, 48)

CovidImpact<-c(rep(0,48))

Lockdown_DailySin<-LockDown*DailySin

Lockdown_DailyCos<-LockDown*DailyCos

Lockdown_HalfDailySin<-LockDown*HalfDailySin

Lockdown_HalfDailyCos<-LockDown*HalfDailyCos

DailyMatrixNormal<-as.matrix(rbind(DailySin, DailyCos,HalfDailySin, HalfDailyCos, LockDown, 
                                   Lockdown_HalfDailySin,Lockdown_HalfDailyCos, Lockdown_DailySin,
                                   Lockdown_DailyCos, CovidImpact))

DailyChangeNormal<-c(as.array(DailyChange)%*%DailyMatrixNormal)

##Weekly change

WeeklyChange<-ModelInteration$coef[c(13,14,21,22, 25, 28, 29, 30)]

Time<-c(1:(48*7))

WeeklySin<-sin(2*pi*Time/(1*48*7))

WeeklyCos<-cos(2*pi*Time/(1*48*7))

WorkDay<-c(rep(1,(48*5)),rep(0,(48*2)))

#Lockdown: Yes

LockDown<-c(rep(1,(48*7)))

CovidImpact<-c(rep(1,48*7))

LockDown_WorkDay<-LockDown*WorkDay

LockDown_WeeklySin<-LockDown*WeeklySin

LockDown_WeeklyCos<-LockDown*WeeklyCos

WeeklyMatrixYes<-as.matrix(rbind(WeeklySin, WeeklyCos, LockDown, WorkDay, LockDown_WorkDay,
                           LockDown_WeeklySin, LockDown_WeeklyCos, CovidImpact))

WeeklyChangeLockdownYes<-c(as.array(WeeklyChange)%*%WeeklyMatrixYes)

#Lockdown: No

LockDown<-c(rep(0,(48*7)))

CovidImpact<-c(rep(1,48*7))

LockDown_WorkDay<-LockDown*WorkDay

LockDown_WeeklySin<-LockDown*WeeklySin

LockDown_WeeklyCos<-LockDown*WeeklyCos

WeeklyMatrixNo<-as.matrix(rbind(WeeklySin, WeeklyCos, LockDown, WorkDay, LockDown_WorkDay,
                                LockDown_WeeklySin, LockDown_WeeklyCos, CovidImpact))

WeeklyChangeLockdownNo<-c(as.array(WeeklyChange)%*%WeeklyMatrixNo)

#Normal 

LockDown<-c(rep(0,(48*7)))

CovidImpact<-c(rep(0,48*7))

LockDown_WorkDay<-LockDown*WorkDay

LockDown_WeeklySin<-LockDown*WeeklySin

LockDown_WeeklyCos<-LockDown*WeeklyCos

WeeklyMatrixNormal<-as.matrix(rbind(WeeklySin, WeeklyCos, LockDown, WorkDay, LockDown_WorkDay,
                                    LockDown_WeeklySin, LockDown_WeeklyCos, CovidImpact))

WeeklyChangeNormal<-c(as.array(WeeklyChange)%*%WeeklyMatrixNormal)



########################Plot

par(mfrow=c(2,1))

#Daily Average

plot(DailyChangeNormal, type="l",col="black", xlim = c(1,48),ylim=c(-1100,800), 
     xlab="Time (00:00-00:30)", ylab="Change  (MW)", main="Daily Average")

points(DailyChangeLockdownYes, type="l", col="red")

points(DailyChangeLockdownNo,type="l",  col="blue")

legend("topleft", legend=c("Normal", "Lockdown", "No Lockdown"),
       col=c("black","red", "blue"), lty=1:2, cex=0.65)

#Weekly Average

plot(WeeklyChangeNormal, type="l",col="black", xlim = c(1,48*7),ylim=c(-500,500), 
     xlab="Time (Mon.-Sun.)", ylab="Change  (MW)", main="Weekyly Average")

points(WeeklyChangeLockdownYes, type="l", col="red")

points(WeeklyChangeLockdownNo,type="l",  col="blue")

legend("topright", legend=c("Normal", "Lockdown", "No Lockdown"),
       col=c("black","red", "blue"), lty=1:2, cex=0.65)

##Total change

TotalChange<-ModelInteration$coef[c(9, 10, 11, 12, 21, 23, 24, 26, 27, 30, 13,14,22, 25, 28, 29)]

Time<-c(1:(48*7))

WorkDay<-c(rep(1,(48*5)),rep(0,(48*2)))

HalfDailySin<-sin(2*pi*Time/(1*24))

HalfDailyCos<-cos(2*pi*Time/(1*24))

DailySin<-sin(2*pi*Time/(1*48))

DailyCos<-cos(2*pi*Time/(1*48))

WeeklySin<-sin(2*pi*Time/(1*48*7))

WeeklyCos<-cos(2*pi*Time/(1*48*7))

#Lockdown Yes

LockDown<-c(rep(1,(48*7)))

CovidImpact<-c(rep(1,48*7))

Lockdown_DailySin<-LockDown*DailySin

Lockdown_DailyCos<-LockDown*DailyCos

Lockdown_HalfDailySin<-LockDown*HalfDailySin

Lockdown_HalfDailyCos<-LockDown*HalfDailyCos

LockDown_WorkDay<-LockDown*WorkDay

LockDown_WeeklySin<-LockDown*WeeklySin

LockDown_WeeklyCos<-LockDown*WeeklyCos

TotalMatrixYes<-as.matrix(rbind(DailySin, DailyCos,HalfDailySin, HalfDailyCos, LockDown, 
                                Lockdown_HalfDailySin,Lockdown_HalfDailyCos, Lockdown_DailySin,
                                Lockdown_DailyCos, CovidImpact, WeeklySin, WeeklyCos, WorkDay, LockDown_WorkDay,
                                LockDown_WeeklySin, LockDown_WeeklyCos))

TotalChangeLockdownYes<-c(as.array(TotalChange)%*%TotalMatrixYes)




#No

LockDown<-c(rep(0,(48*7)))

CovidImpact<-c(rep(1,48*7))

Lockdown_DailySin<-LockDown*DailySin

Lockdown_DailyCos<-LockDown*DailyCos

Lockdown_HalfDailySin<-LockDown*HalfDailySin

Lockdown_HalfDailyCos<-LockDown*HalfDailyCos

LockDown_WorkDay<-LockDown*WorkDay

LockDown_WeeklySin<-LockDown*WeeklySin

LockDown_WeeklyCos<-LockDown*WeeklyCos

TotalMatrixNo<-as.matrix(rbind(DailySin, DailyCos,HalfDailySin, HalfDailyCos, LockDown, 
                               Lockdown_HalfDailySin,Lockdown_HalfDailyCos, Lockdown_DailySin,
                               Lockdown_DailyCos, CovidImpact, WeeklySin, WeeklyCos, WorkDay, LockDown_WorkDay,
                               LockDown_WeeklySin, LockDown_WeeklyCos))

TotalChangeLockdownNo<-c(as.array(TotalChange)%*%TotalMatrixNo)


#Normal

LockDown<-c(rep(0,(48*7)))

CovidImpact<-c(rep(0,48*7))

Lockdown_DailySin<-LockDown*DailySin

Lockdown_DailyCos<-LockDown*DailyCos

Lockdown_HalfDailySin<-LockDown*HalfDailySin

Lockdown_HalfDailyCos<-LockDown*HalfDailyCos

LockDown_WorkDay<-LockDown*WorkDay

LockDown_WeeklySin<-LockDown*WeeklySin

LockDown_WeeklyCos<-LockDown*WeeklyCos

TotalMatrixNormal<-as.matrix(rbind(DailySin, DailyCos,HalfDailySin, HalfDailyCos, LockDown, 
                                   Lockdown_HalfDailySin,Lockdown_HalfDailyCos, Lockdown_DailySin,
                                   Lockdown_DailyCos, CovidImpact, WeeklySin, WeeklyCos, WorkDay, LockDown_WorkDay,
                                   LockDown_WeeklySin, LockDown_WeeklyCos))

TotalChangeNormal<-c(as.array(TotalChange)%*%TotalMatrixNormal)


par(mfrow=c(2,2))

#Monday

plot(TotalChangeNormal[1:48], main="Monday", type="l",col="black", 
     xlim = c(1,48),ylim=c(-1200,800),  xlab="Time (00:30-00:00)", ylab="Change (MW)")

points(TotalChangeLockdownYes[1:48], type="l", col="red")

points(TotalChangeLockdownNo[1:48],type="l",  col="blue")

legend("bottomright", legend=c("Normal", "Lockdown", "No Lockdown"),
       col=c("black","red", "blue"), lty=1:3, cex=0.6)

#Tuesday

plot(TotalChangeNormal[49:96], main="Tuesday", type="l",col="black", 
     xlim = c(1,48),ylim=c(-1000,1000),  xlab="Time (00:30-00:00)", ylab="Change (MW)")

points(TotalChangeLockdownYes[49:96], type="l", col="red")

points(TotalChangeLockdownNo[49:96],type="l",  col="blue")

legend("bottomright", legend=c("Normal", "Lockdown", "No Lockdown"),
       col=c("black","red", "blue"), lty=1:3, cex=0.6)

#Wednesday

plot(TotalChangeNormal[97:144], main="Wednesday", type="l",col="black", 
     xlim = c(1,48),ylim=c(-800,1100),  xlab="Time (00:30-00:00)", ylab="Change (MW)")

points(TotalChangeLockdownYes[97:144], type="l", col="red")

points(TotalChangeLockdownNo[97:144],type="l",  col="blue")

legend("bottomright", legend=c("Normal", "Lockdown", "No Lockdown"),
       col=c("black","red", "blue"), lty=1:3, cex=0.6)

#Thursday 

plot(TotalChangeNormal[145:192], main="Thursday ", type="l",col="black", 
     xlim = c(1,48),ylim=c(-800,1000),  xlab="Time (00:30-00:00)", ylab="Change (MW)")

points(TotalChangeLockdownYes[145:192], type="l", col="red")

points(TotalChangeLockdownNo[145:192],type="l",  col="blue")

legend("bottomright", legend=c("Normal", "Lockdown", "No Lockdown"),
       col=c("black","red", "blue"), lty=1:3, cex=0.6)

par(mfrow=c(2,2))

#Friday

plot(TotalChangeNormal[193:240], main="Friday", type="l",col="black", 
     xlim = c(1,48),ylim=c(-1000,800),  xlab="Time (00:30-00:00)", ylab="Change (MW)")

points(TotalChangeLockdownYes[193:240], type="l", col="red")

points(TotalChangeLockdownNo[193:240],type="l",  col="blue")

legend("bottomright", legend=c("Normal", "Lockdown", "No Lockdown"),
       col=c("black","red", "blue"), lty=1:3, cex=0.6)

#Saturday

plot(TotalChangeNormal[241:288], main="Saturday", type="l",col="black", 
     xlim = c(1,48),ylim=c(-1300,600),  xlab="Time (00:30-00:00)", ylab="Change (MW)")

points(TotalChangeLockdownYes[241:288], type="l", col="red")

points(TotalChangeLockdownNo[241:288],type="l",  col="blue")

legend("bottomright", legend=c("Normal", "Lockdown", "No Lockdown"),
       col=c("black","red", "blue"), lty=1:3, cex=0.6)

#Sunday

plot(TotalChangeNormal[289:336], main="Sunday", type="l",col="black", 
     xlim = c(1,48),ylim=c(-1300,500),  xlab="Time (00:30-00:00)", ylab="Change (MW)")

points(TotalChangeLockdownYes[289:336], type="l", col="red")

points(TotalChangeLockdownNo[289:336],type="l",  col="blue")

legend("bottomright", legend=c("Normal", "Lockdown", "No Lockdown"),
       col=c("black","red", "blue"), lty=1:3, cex=0.6)