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

# 3rd dataset Turkish Music-Emotion Relation

d3=fread("C:/Users/pinar/Desktop/Acoustic Features.csv")
head(d3)
str(d3)

set.seed(43210)

# Output target is emotion class

# Test and train splitting
split3 <- sample(c(rep(0, 0.7 * nrow(d3)), rep(1, 0.3 * nrow(d3))))

traindata3 <- d3[split3 == 0, ]
testdata3 <- d3[split3 == 1, ]

head(traindata3)
head(testdata3)

# Decision tree algorithm

dt3 <- rpart(Class ~ ., data= traindata3, method= "class")
rpart.plot(dt3, type = 2, extra=106, box.palette = "Oranges")

predictdata_dt3 <- predict(dt3,testdata3, type="class")
predictdata_dt3

table_mat1_3 <- table(testdata3$Class, predictdata_dt3)
table_mat1_3

#Accuracy
accuracy_Test1_3 <- sum(diag(table_mat1_3)) / sum(table_mat1_3)
print(paste('Accuracy for decision tree algorithm', accuracy_Test1_3))

# Random forest algorithm

rf3 <- randomForest(as.factor(Class) ~ ., data = traindata3, ntree= 500, nodesize =5, type= "class")
rf3

# Number of trees with lowest MSE
which.min(rf3$mse)

# Produce variable importance plot
varImpPlot(rf3) 

# Tuning
model_tuned3 <- tuneRF(
  traindata3[,2:50], # define predictor variables
  as.factor(traindata3$Class), # define response variable
  ntreeTry=500,
  mtryStart=4, 
  stepFactor=1.5,
  improve=0.01,
  trace=FALSE) #don't show real-time progress

best.m3 <- model_tuned3[model_tuned3[, 2] == min(model_tuned3[, 2]), 1]
print(model_tuned3)
print(best.m3)

# set best.m
set.seed(3210)
rf3 <- randomForest(as.factor(Class) ~ ., data = traindata3, ntree= 500, mtry=best.m3, nodesize= 5, type="class")
rf3
plot(rf3)

#Prediction
predictdata_rf3 <-predict(rf3, testdata3)
table(predictdata_rf3, testdata3$Class)

#Accuracy
table_mat2_3 <- table(testdata3$Class, predictdata_rf3)

accuracy_Test2_3 <- sum(diag(table_mat2_3)) / sum(table_mat2_3)
print(paste('Accuracy for random forest algorithm', accuracy_Test2_3))

