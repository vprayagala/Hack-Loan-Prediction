#avidya - Loan Prediction Problem
rm(list=ls())
getwd()
setwd("/Users/Saatwik/Documents/avidya/LoanPred")
require(ROCR)
require(DMwR)

#1. get the training data
train.file.name="train.csv"
train.data<-read.csv(train.file.name,header=T)
#check the dimensions
dim(train.data)
#check the structure and summary
str(train.data)
summary(train.data)

# check if any missing values
sum(is.na(train.data))
train.data.nas<-train.data[(!complete.cases(train.data)),]
#drop the loan ID as it is not significant
train.data<-train.data[,-1]
#fill the space values as 'NA'
train.data[(!train.data$Gender %in% c("Male","Female")),]$Gender<-NA
train.data[(!train.data$Married %in% c("Yes","No")),]$Married<-NA
train.data[(!train.data$Dependents %in% c("0","1","2","3","4","5")),]$Dependents<-NA
train.data[(!train.data$Self_Employed %in% c("Yes","No")),]$Self_Employed<-NA
#compute the missing values using central statistics
train.data.imp<-centralImputation(train.data)

#now apply the logistics regression on train data.
#Loan_status as depedent and rest all as independent
Log_Reg1<-glm(Loan_Status~.,family=binomial,data=train.data.imp)
summary(Log_Reg1)

#Droping insignificant attributes
Log_Reg2<-glm(Loan_Status~Married+Credit_History+Property_Area,family=binomial,
              data=train.data.imp)
summary(Log_Reg2)

#ROC and AUC

test_model<-function(model,data){
  p<-predict(model,data,type="response")
  p.class<-ifelse(p > 0.5, "Y","N")
  pr<-prediction(p,data$Loan_Status)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  plot(prf)
  abline(a=0, b= 1)
  
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  
  conf.mat = table(data$Loan_Status,p.class)
  accuracy = sum(diag(conf.mat))/sum(conf.mat)
  precision = conf.mat[2,2]/sum(conf.mat[,2])
  recall = conf.mat[2,2]/sum(conf.mat[2,])
  res<-data.frame(auc,accuracy,precision,recall)
  return (res)
}
#Test training model
train.perf<-test_model(Log_Reg2,train.data.imp)
#From avove error metrics , the train model is okay
#we can test the same on test data
test.data.filename<-"test.csv"
test.data<-read.csv(test.data.filename,header=T)
#remove loan id
str(test.data)
test.data.ret<-test.data
test.data<-test.data[,-1]
#fill the space values as 'NA'
test.data[(!test.data$Gender %in% c("Male","Female")),]$Gender<-NA
test.data[(!test.data$Married %in% c("Yes","No")),]$Married<-NA
test.data[(!test.data$Dependents %in% c("0","1","2","3","4","5")),]$Dependents<-NA
test.data[(!test.data$Self_Employed %in% c("Yes","No")),]$Self_Employed<-NA
#compute the missing values using central statistics
test.data.imp<-centralImputation(test.data)
p<-predict(Log_Reg1,test.data.imp,type="response")
p.class<-ifelse(p > 0.5, "Y","N")
#Build the Results
results<-data.frame(test.data.ret[,1],p.class)
names(results)<-c("Loan_ID","Loan_Status")
write.csv(results,"Loan_Pred_result2.csv")
