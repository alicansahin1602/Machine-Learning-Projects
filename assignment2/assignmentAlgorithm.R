#This homework is done by Ali Can Þahin 2018402189 and Selin Coþkun 2018402213
#Some computations are made in different computers. Some have been run by Ali Can's and some have been run by Selin's computer
#to avoid huge run times. However, all the outputs are given in comment just after the code.



setwd('C:/Users/alica/OneDrive/Masaüstü/ali belgeler/BOUN/3.Sýnýf/2.DÖNEM/IE 425/HW2')
hr_data = read.csv('HR.csv')
str(hr_data)
hr_data$Work_accident = as.factor(hr_data$Work_accident)
hr_data$left = as.factor(hr_data$left)
hr_data$promotion_last_5years = as.factor(hr_data$promotion_last_5years)
hr_data$salary = as.factor(hr_data$salary)
hr_data$sales = as.factor(hr_data$sales)
str(hr_data)


#Question 1.a
library(caTools) #for split fnc
set.seed(425)

split = sample.split(hr_data$left,SplitRatio = 0.8 )
hr_train = subset(hr_data,split == TRUE)
hr_test = subset(hr_data,split == FALSE)


#Question 1.b
library(randomForest)
library(caret)
set.seed(425)
control_param = trainControl(method = 'repeatedcv',number = 10,repeats = 5)
rf_hr_5 = train(left~.,data = hr_train,method = 'rf', metric = 'Accuracy',trControl = control_param,
             ntree = 5 ,tuneGrid = expand.grid(.mtry = c(3:9))) 
rf_hr_5
rf_hr_5$finalModel #OOB ERROR RATE 2.22%
set.seed(425)
rf_hr_10 = train(left~.,data = hr_train,method = 'rf', metric = 'Accuracy',trControl = control_param,
                 ntree = 10,tuneGrid = expand.grid(.mtry = c(3:9))) 

rf_hr_10
rf_hr_10$finalModel #OOB ESTÝMATE RATE 1.46%

set.seed(425)
rf_hr_20 = train(left~.,data = hr_train,method = 'rf', metric = 'Accuracy',trControl = control_param,
                 ntree = 20,tuneGrid = expand.grid(.mtry = c(3:9))) 

rf_hr_20
rf_hr_20$finalModel # OOB ESTIMATE RATE 1.09%
set.seed(425)
rf_hr_30 = train(left~.,data = hr_train,method = 'rf', metric = 'Accuracy',trControl = control_param,
                 ntree = 30,tuneGrid = expand.grid(.mtry = c(3:9))) 

rf_hr_30
rf_hr_30$finalModel # OOB ESTIMATE RATE 1.03%
set.seed(425)
rf_hr_50 = train(left~.,data = hr_train,method = 'rf', metric = 'Accuracy',trControl = control_param,
                 ntree = 50,tuneGrid = expand.grid(.mtry = c(3:9))) 

rf_hr_50
rf_hr_50$finalModel #OOB ESTIMATE RATE 0.99%

set.seed(425)
rf_hr_100 = train(left~.,data = hr_train,method = 'rf', metric = 'Accuracy',trControl = control_param,
                 ntree = 100,tuneGrid = expand.grid(.mtry = c(3:9))) 

rf_hr_100
rf_hr_100$finalModel #OOB ESTIMATE RATE 0.86% mtry 8 

set.seed(425)
rf_hr_200 = train(left~.,data = hr_train,method = 'rf', metric = 'Accuracy',trControl = control_param,
                  ntree = 200,tuneGrid = expand.grid(.mtry = c(3:9))) 

rf_hr_200
rf_hr_200$finalModel #OOB ESTIMATE RATE 0.83% mtry 8 



#After several trials, We have decided to continue with mtry = 8 and ntree = 100. We have tried mtry between 3:9 because there are 9 variables and default
# value is sqrt(9) = 3.

#We have tried for ntree parameters 5, 10,20,30,50,100, and 200 because of the huge time needed to run it. At the end, we have decided to move on with ntree = 100 
# because it gives relatively low error rate and it is very close to ntree = 200 (0.83). Considering also run time, it seems logical to select ntree = 100



set.seed(425)
rf_package_hr = randomForest(left~., data = hr_train,ntree = 100,mtry = 9, importance = T)


rf_package_hr
#Our OOB accuracy = 1- 0.0103 = 0.9897

round(importance(rf_package_hr),2)
varImp(rf_package_hr)
varImpPlot(rf_package_hr)

#When we plotted their importance and check MeanDecreaseAccuracy which indicates the degree of decreasing in Accuracy when 
#a specific variable is removed, It shows us that satisfaction_level is the most important variable. Additionally,
#number_of_project, last_evaluation , average_monthly_hours and time_spend_company variables are also important. 


#Question 1.c

set.seed(425)
predict(rf_hr_100,hr_test)
confusionMatrix(predict(rf_hr_100,hr_test),hr_test$left)

#Sensitivity : 0.9978
#Specificity : 0.9748 
#precision = tp/(tp+fp) = pos pred value =0.9922
#recall = sensivitiy = 0.9978





#Question 1.d
#install.packages('gbm')
library(gbm)
set.seed(425)
gbmGrid=expand.grid(interaction.depth = c(2,3,4,5), 
                    n.trees = (1:10)*10,shrinkage = (1:5)*0.1,n.minobsinnode = 20)
gbm_hr = train(left~.,data = hr_train,method='gbm',metric = 'Accuracy',trControl = control_param,tuneGrid = gbmGrid,verbose = FALSE)
gbm_hr #n.trees = 100,interaction.depth = 5,shrinkage = 0.4,n.minobsinnode = 20

#Question 1.e
set.seed(425)


predicted_gbm_hr = predict(gbm_hr,newdata = hr_test)

predicted_gbm_hr
confusionMatrix(predicted_gbm_hr,hr_test$left) 
#Sensitivity = Recall = 0.9908
#Specifity = 0.9454
#Precision = Pos Pred Value = 0.9831 





#Question 2.a

toyota_data = read.csv("ToyotaCorolla.csv")
toyota_data$FuelType = as.factor(toyota_data$FuelType)
toyota_data$MetColor = as.factor(toyota_data$MetColor)
toyota_data$Automatic = as.factor(toyota_data$Automatic)



set.seed(425)
tr = sample(1:nrow(toyota_data),nrow(toyota_data)*0.8)
toyota_train = toyota_data[tr,]
toyota_test = toyota_data[-tr,]

#Question 2.b
set.seed(425)
control_param = trainControl(method = 'repeatedcv',number = 10,repeats = 5)

toyota_5 = train(Price~.,data = toyota_train,method = 'rf', metric = 'RMSE',trControl = control_param,
                ntree = 5 ,tuneGrid = expand.grid(.mtry = c(3:9))) 
toyota_5
toyota_5$finalModel # MSE 1865927 MTRY = 7

set.seed(425)
toyota_10 = train(Price~.,data = toyota_train,method = 'rf', metric = 'RMSE',trControl = control_param,
                 ntree = 10 ,tuneGrid = expand.grid(.mtry = c(3:9))) 
toyota_10
toyota_10$finalModel #MSE 1585056 MTRY = 5

set.seed(425)
toyota_20 = train(Price~.,data = toyota_train,method = 'rf', metric = 'RMSE',trControl = control_param,
                 ntree =20 ,tuneGrid = expand.grid(.mtry = c(3:9))) 
toyota_20
toyota_20$finalModel #MSE 1349773 MTRY 5 

set.seed(425)
toyota_50 = train(Price~.,data = toyota_train,method = 'rf', metric = 'RMSE',trControl = control_param,
                 ntree = 50 ,tuneGrid = expand.grid(.mtry = c(3:9))) 
toyota_50
toyota_50$finalModel #MSE 1280655 MTRY 5

set.seed(425)
toyota_100 = train(Price~.,data = toyota_train,method = 'rf', metric = 'RMSE',trControl = control_param,
                  ntree = 100 ,tuneGrid = expand.grid(.mtry = c(3:9))) 
toyota_100
toyota_100$finalModel #MSE 1227036 MTRY 5

set.seed(425)
toyota_200 = train(Price~.,data = toyota_train,method = 'rf', metric = 'RMSE',trControl = control_param,
                   ntree = 200 ,tuneGrid = expand.grid(.mtry = c(3:9))) 
toyota_200
toyota_200$finalModel #MSE 1.223.556 MTRY 4

set.seed(425)
toyota_300 = train(Price~.,data = toyota_train,method = 'rf', metric = 'RMSE',trControl = control_param,
                   ntree = 300 ,tuneGrid = expand.grid(.mtry = c(3:9))) 
toyota_300
toyota_300$finalModel #MSE 1.219.604 MTRY 4

set.seed(425)
toyota_400 = train(Price~.,data = toyota_train,method = 'rf', metric = 'RMSE',trControl = control_param,
                   ntree = 400 ,tuneGrid = expand.grid(.mtry = c(3:9))) 
toyota_400
toyota_400$finalModel #MSE 1.217.089 MTRY 4

set.seed(425)
toyota_500 = train(Price~.,data = toyota_train,method = 'rf', metric = 'RMSE',trControl = control_param,
                   ntree = 500 ,tuneGrid = expand.grid(.mtry = c(3:9))) 
toyota_500
toyota_500$finalModel #MSE 1.214.412 MTRY 4

#We evaluated from ntree = 5 to ntree = 500 which is default value. We have seen that ntree = 500 gives lowest mse value and also
# mtry is close to 3 which is m/3. We didn't continue to increase ntree because of the time restrictions. Therefore, we selected
# ntree = 500 mtry = 4.


set.seed(425)
rf_toyota = randomForest(Price~.,data = toyota_train,ntree = 500,mtry = 4, importance = T)
rf_toyota # MSE 1205718 ~~ out of bag error

round(importance(rf_toyota),2)
varImp(rf_toyota)
varImpPlot(rf_toyota)

#When we plot the importance of the variables we can clearly see that Age variable plays a huge role in determining the price of the 
# cars.
#In some ntree sizes we calculated in different computers to reduce the run time. Therefore, it can't be seen in environment. For example
# after the size 50 it is calculated in my friend's computer. 


#Question 2.c

predicted_toyota = predict(toyota_500,toyota_test)
library(Metrics)
rmse(actual = toyota_test$Price,predicted = predicted_toyota) # 1042.861
mae(actual = toyota_test$Price,predicted = predicted_toyota) # 784.1177


#Question 2.d

set.seed(425)

gbmGrid_toy=expand.grid(interaction.depth = c(2,3,4,5), 
                    n.trees = (1:5)*100,shrinkage = (1:5)*0.1,n.minobsinnode = 20)
gbm_toyota = train(Price~.,data = toyota_train,method='gbm',metric = 'RMSE',trControl = control_param,tuneGrid = gbmGrid_toy,verbose = FALSE)
gbm_toyota 

#best values :
#n.trees = 300 , interaction.depth = 4, shrinkage = 0.1, n.minobsinnode = 20


#Question 2.e
set.seed(425)
predicted_gbm_toyota = predict(gbm_toyota,newdata = toyota_test)
predicted_gbm_toyota


rmse(actual = toyota_test$Price,predicted = predicted_gbm_toyota) #1061.201
mae(actual = toyota_test$Price,predicted = predicted_gbm_toyota) # 789.4138














