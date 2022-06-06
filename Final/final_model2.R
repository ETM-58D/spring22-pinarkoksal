# install.packages('rpart')
# install.packages('rpart.plot')
# install.packages('caret')
# install.packages('rattle')
# install.packages('randomForest')
# install.packages('ranger')

require(ggplot2)
require(tidyverse)
require(rpart)
require(dplyr)
require(rpart.plot)
require(caret)
require(rattle)
require(randomForest)
require(ranger)
require(data.table)

# 2nd dataset absenteeism

d2=fread("C:/Users/pinar/Desktop/Absenteeism_at_work_AAA/Absenteeism_at_work.csv")
head(d2)
str(d2)

set.seed(4321)

# Output target is absenteeism time in hours

# Test and train splitting
split2 <- sample(c(rep(0, 0.7 * nrow(d2)), rep(1, 0.3 * nrow(d2))))

traindata2 <- d2[split2 == 0, ]
testdata2 <- d2[split2 == 1, ]

head(traindata2)
head(testdata2)

# Decision tree algorithm

dt2 <- rpart(Absenteeism ~ ., data= traindata2, method= "class")
rpart.plot(dt2, type = 2, extra=106, box.palette = "Blues")

predictdata_dt2 <- predict(dt2,testdata2, type="class")
predictdata_dt2

table_mat1_2 <- table(testdata2$Absenteeism, predictdata_dt2)
table_mat1_2

#Accuracy
accuracy_Test1_2 <- sum(diag(table_mat1_2)) / sum(table_mat1_2)
print(paste('Accuracy for decision tree algorithm', accuracy_Test1_2))

# Random forest algorithm

rf2 <- randomForest(formula= Absenteeism ~ ., data = traindata2, ntree= 500, nodesize =5)
rf2

# Number of trees with lowest MSE
which.min(rf2$mse)

# Produce variable importance plot
varImpPlot(rf2) 

# Tuning
model_tuned2 <- tuneRF(
  traindata2[,1:20], # define predictor variables
  traindata2$Absenteeism, # define response variable
  ntreeTry=500,
  mtryStart=4, 
  stepFactor=1.5,
  improve=0.01,
  trace=FALSE) #don't show real-time progress

best.m2 <- model_tuned2[model_tuned2[, 2] == min(model_tuned2[, 2]), 1]
print(model_tuned2)
print(best.m2)

# set best.m
set.seed(321)
rf2 <- randomForest(formula= Absenteeism ~ ., data = traindata2, ntree= 500, mtry=best.m2, nodesize= 5, type="class")
rf2
plot(rf2)

#Prediction
predictdata_rf2 <-predict(rf2, testdata2)
table(predictdata_rf2, testdata2$Absenteeism)

#Accuracy
table_mat2_2 <- table(testdata2$Absenteeism, predictdata_rf2)

accuracy_Test2_2 <- sum(diag(table_mat2_2)) / sum(table_mat2_2)
print(paste('Accuracy for random forest algorithm', accuracy_Test2_2))

