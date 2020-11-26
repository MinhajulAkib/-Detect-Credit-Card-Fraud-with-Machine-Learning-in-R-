#credit card fraud detection on sample dataset
#importing library
#install.packages("ranger")
library(ranger)
library(caret)
library(data.table)
setwd("C:/Users/Akib/Desktop/R")
df <-read.csv("C:/Users/Akib/Desktop/R/creditcard.csv/akib.csv")

#exploration
data.table(df)

#Doing random statistical....
summary(df)
#balanced and imbalanced data check
table(df$Class)
names(df)

#summary of amount
summary(df$Amount) #produce result summaries of the results of various model fitting functions
sd(df$Amount)    #Standard deviation
IQR(df$Amount)   # Interquartile Range
var(df$Amount)   #Correlation, Variance and Covariance (Matrices)

#Manipulation
df$Amount <- scale(data$Amount)   #normalization
data <- df[,-c(1)]     #removing time from dataset
head(data)
set.seed(12)
library(caTools)

sample_data <-sample.split(data$Class,SplitRatio = 0.80)  #[sample.split=classification into train and test dataset]
train_data <-subset(data,sample_data==TRUE)
test_data <-subset(data,sample_data==FALSE)
dim(train_data)
dim(test_data)

#fit logistic on data

Logistic_Model <- glm(Class~., test_data, family = binomial())  
summary(Logistic_Model)
plot(Logistic_Model)

Logistic_Model1 <- glm(Class~., train_data, family = binomial())  
summary(Logistic_Model1)
plot(Logistic_Model1)

#we need roc curve visit bigquery tutorial to leaarn about roc

library(pROC)
lr.predict <-predict(Logistic_Model1,test_data,probability=TRUE)
auc.gb <- roc(test_data$Class,lr.predict,plot = TRUE,col="green")


#fit a decision tree
library(rpart)
library(rpart.plot)

decision_model<-rpart(Class~.,data,method = "class")
predicted_val <-predict(decision_model, data,type="class")
probability <-predict(decision_model, data,type="prob")
rpart.plot(decision_model)


#Neural Network
library(neuralnet)
NN_model<-neuralnet::neuralnet(Class~.,train_data,linear.output = FALSE)
plot(NN_model)

predNN<-compute(NN_model,test_data)
resultNN<-predNN$net.result
resultNN=ifelse(resultNN>0.6,1,0)




















