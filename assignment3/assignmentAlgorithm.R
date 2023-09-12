#install.packages("ISLR")
library(ISLR)
library(caTools)
library(caret)
#install.packages("ROCR")
library(ROCR)
library(class)

#QUESTION 2

data = Default
str(data)

set.seed(425)

data[,c(3,4)] = scale(data[,c(3,4)])
data
data$default = as.numeric(data$default)
data$student = as.numeric(data$student)

split = sample.split(data$default,SplitRatio = 0.75)

data_tr = subset(data,split ==TRUE)
data_test = subset(data,split == FALSE)

data_tr_without_default = data_tr[,-1]
data_tr_default = data_tr[,1]
data_test_without_default = data_test[,-1]



error_rate = NULL
specificity = NULL
sensitivity = NULL


for(i in 1:10){
  pred = knn(data_tr_without_default,data_test_without_default,data_tr_default,k = i)
  table = table(data_test[,1],pred)
  error_rate[i] = 1 - ((table[1,1] + table[2,2]) / (table[1,1] + table[1,2] + table[2,1] + table[2,2]))
  specificity[i] = table[2,2] /(table[2,2] + table[2,1])
  sensitivity[i] = table[1,1] / (table[1,1] + table[1,2])
}
error_rate
specificity
sensitivity




# Question 3

setwd('C:/Users/alica/OneDrive/Masaüstü/ali belgeler/BOUN/3.Sýnýf/2.DÖNEM/IE 425/HW3')

hr_data = read.csv('HR.csv')
str(hr_data)
hr_data$sales = as.factor(hr_data$sales)
hr_data$salary = as.factor(hr_data$salary)
hr_data$left = as.factor(hr_data$left)
hr_data$Work_accident = as.factor(hr_data$Work_accident)
hr_data$promotion_last_5years = as.factor(hr_data$promotion_last_5years)
str(hr_data)



#QUESTION 3.A

set.seed(425)
split = sample.split(hr_data$left, SplitRatio = 0.8)

hr_tr = subset(hr_data, split == TRUE)
hr_test = subset(hr_data,split == FALSE)

#QUESTION 3.B

model = glm(left~., hr_tr, family = 'binomial')
summary(model)
# satisfaction_level, last_evaluation, number_project, average_monthly_hours, time_spend_company, work_accident, promotion_last_5years1, salary, salesRandD has importance

#QUESTION 3.C

predictions = predict(model,hr_test, type ='response')

for(i in 1:length(predictions)){
  if(predictions[i] >= 0.5){
    predictions[i] = 1
  }else{
    predictions[i] = 0
  }
}

predictions = as.factor(predictions)

confusionMatrix(hr_test$left, predictions)
#Sensitivity = Recall =  0.8155
#Specificity 0.5725
#Precision 0.9226



#QUESTION 3.D
set.seed(425)
predictions = predict(model,hr_test, type ='response')
tapply(predictions,hr_test$left,mean)
prediction_ROCR = prediction(predictions,hr_test$left)
perf_model= performance(prediction_ROCR,'tpr','fpr')
plot(perf_model)


as.numeric(performance(prediction_ROCR,'auc')@y.values)  #0.8221515







