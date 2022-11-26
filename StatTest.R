#R test
# Loading
setwd("C:/Users/wujrt/Queensland University of Technology/You-Gan Wang - RyanWu(Jinran)/EPSR2022Covid19/EPSR_R2/epsr_revised")
Model3_TrainPreds<-read.csv("resultsnocovid19_train.csv")
Model3_TestPreds<-read.csv("resultsnocovid19_test.csv")
Proposed_TrainPreds<-read.csv("resultscovid19_train.csv")
Proposed_TestPreds<-read.csv("resultscovid19_test.csv")
data1<-read.csv("loaddata1.csv")
data2<-read.csv("loaddata2.csv")
RawData<-rbind(data1,data2)
# statistical test
Model3_Preds<-c(Model3_TrainPreds[,2],Model3_TestPreds[,2])
Proposed_Preds<-c(Proposed_TrainPreds[,2],Proposed_TestPreds[,2])
Delta_Preds<-Proposed_Preds-Model3_Preds
label=RawData[,2]
DDATA<-as.data.frame(cbind(Delta_Preds,label))
DDATA_locKdown<-DDATA[DDATA[,2]==1,]
DDATA_nonlockdown<-DDATA[DDATA[,2]==0,]
#test lockdown
t.test(DDATA_locKdown[,1], mu =0) 
# test nonlockdown
t.test(Delta_Preds, mu =0) 
