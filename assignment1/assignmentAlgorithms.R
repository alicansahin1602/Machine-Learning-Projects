
setwd("C:/Users/alica/OneDrive/Masaüstü/ali belgeler/BOUN/3.Sınıf/2.DÖNEM/IE 425/HW1")
data = read.csv("FinancialDistress-cat.csv")
data$Financial.Distress = as.factor(data$Financial.Distress)
str(data)
library(caTools)
set.seed(500)
split = sample.split(data$Financial.Distress,SplitRatio = 0.75)
data_tr = subset(data,split == TRUE)
data_test = subset(data,split == FALSE)

#Question 1.a
percantage_overall = length(which(data$Financial.Distress == 1))/length(data$Financial.Distress)
percantage_tr_0 = length(which(data_tr$Financial.Distress == FALSE))/length(data_tr$Financial.Distress)
percantage_tr_1 = length(which(data_tr$Financial.Distress == TRUE))/length(data_tr$Financial.Distress)
percantage_test_0 = length(which(data_test$Financial.Distress == FALSE))/length(data_test$Financial.Distress)
percantage_test_1 = length(which(data_test$Financial.Distress == TRUE))/length(data_test$Financial.Distress)

percantage_overall #0.06862745
percantage_tr_0 #0.9313725
percantage_tr_1 #0.06862745
percantage_test_0 #0.9313725
percantage_test_1 ##0.06862745

#Question 1.b

library(rpart)

tree = rpart(Financial.Distress ~., data= data_tr,cp = 0.00001)

printcp(tree)
tree$cptable[which.min(unname(tree$cptable[,'xerror'])),]

best_size_1_b = tree$cptable[which.min(unname(tree$cptable[,'xerror'])),'CP']  #0.3174603
number_of_leaf_nodes_1_b = tree$cptable[which.min(unname(tree$cptable[,'xerror'])),'nsplit'] + 1 #3
number_of_leaf_nodes_1_b
tree$cptable
# Question 1.c
#install.packages('caret')
library(caret)
best_tree = prune.rpart(tree = tree,cp = best_size_1_b)

predicted = predict(best_tree,newdata = data_test,type='class')
confusionMatrix(predicted,data_test$Financial.Distress)
# error rate = 1-accuracy = 1-0.9368 =  #0.0632
# sensivity = recall = tp/(tp+fn) =  0.9813
# specificity = tn/(tn+fp) =  0.3333
# precision = tp/(tp+fp) = pos pred value = 0.9523






#Question 1.d
library(tree)
tree_2 = tree(Financial.Distress~., data= data_tr)
summary(tree_2)
cv.tree(tree_2)
cv.tree(tree_2)$dev
which.min(cv.tree(tree_2)$dev)
best_size2 = cv.tree(tree_2)$size[which.min(cv.tree(tree_2)$dev)]
best_size2
best_tree2 = prune.tree(tree = tree_2,newdata = data_test,best = best_size2)
summary(best_tree2)
best_tree2
#number of leaf nodes = 6 because size = 6

#Question 1.e

pred_tree_2 = predict(best_tree2,newdata = data_test,type = 'class')
confusionMatrix(pred_tree_2,data_test$Financial.Distress)

#error rate = 1-accuracy = 1-0.9368 = 0.0632
#sensitivity = recall = tp/(tp+fn) = 0.9813
#specificity = tn(tn+fp) = 0.3333
#precision = tp/(tp+fp) = pos pred value = 0.9523

#Comprasion

#1.c
# error rate = 1-accuracy = 1-0.9368 =  #0.0632
# sensivity = recall = tp/(tp+fn) =  0.9813
# specificity = tn/(tn+fp) =  0.3333
# precision = tp/(tp+fp) = pos pred value = 0.9523

#1.e

#error rate = 1-accuracy = 1-0.9368 = 0.0632
#sensitivity = recall = tp/(tp+fn) = 0.9813
#specificity = tn/(tn+fp) = 0.3333
#precision = tp/(tp+fp) = pos pred value = 0.9523

# Nothing has changed when we used tree package. It can show that only difference between these two packages are just their syntax.




#QUESTION 2

#2.a

toyota_data = read.csv('ToyotaCorolla.csv')
toyota_data
set.seed(500)

tr = sample(1:nrow(toyota_data),nrow(toyota_data)*0.8)
toyota_data_tr = toyota_data[tr,]
toyota_data_test = toyota_data[-tr,]
#2.b

toyota_tree = rpart(Price~.,data = toyota_data_tr,minbucket = 1, minsplit = 2, cp = 0.00001)
toyota_tree$cptable
best_xv_table = toyota_tree$cptable[which.min(unname(toyota_tree$cptable[,'xerror'])),]
best_size_toyota = toyota_tree$cptable[which.min(unname(toyota_tree$cptable[,'xerror'])),'CP']  #0.0004206154
best_size_toyota

number_of_leaf_nodes_toyota = best_xv_table['nsplit'] + 1
number_of_leaf_nodes_toyota  #52 

best_toyota_tree = prune.rpart(toyota_tree,cp = best_size_toyota)
best_toyota_tree$cptable
#2.c
library(Metrics)
predicted_toyota = predict(best_toyota_tree,newdata = toyota_data_test)
rmse(actual = toyota_data_test[,'Price'],predicted  = predicted_toyota) #1105.707
mae(actual = toyota_data_test[,'Price'],predicted = predicted_toyota) #856.9598























