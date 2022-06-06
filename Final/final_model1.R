
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


# 1st dataset student performance

d1=read.table("C:/Users/pinar/Desktop/student/student-mat.csv",sep=";",header=TRUE)
head(d1)
str(d1)

set.seed(1234)

# Output target is G3 (final grade)

# Test and train splitting
split1 <- sample(c(rep(0, 0.7 * nrow(d1)), rep(1, 0.3 * nrow(d1))))

traindata1 <- d1[split1 == 0, ]
testdata1 <- d1[split1 == 1, ]

head(traindata1)
head(testdata1)

# Decision tree algorithm

  # dt <- rpart(G3 ~ .,data= traindata) #results as anova, so the method is defined.
  # rpart.plot(dt, type = 2, box.palette = "Greens")
  
  dt1 <- rpart(G3 ~ .,data= traindata1, method= "class")
  rpart.plot(dt1, type = 2, extra=106, box.palette = "Greens")
  
  predictdata_dt1 <- predict(dt1,testdata1, type="class")
  predictdata_dt1
  
  table_mat1_1 <- table(testdata1$G3, predictdata_dt1)
  table_mat1_1
  
  #Accuracy
  accuracy_Test1_1 <- sum(diag(table_mat1_1)) / sum(table_mat1_1)
  print(paste('Accuracy for decision tree algorithm', accuracy_Test1_1))

# Random forest algorithm

  rf1 <- randomForest(formula= G3 ~ ., data = traindata1, ntree= 500, nodesize =5)
  rf1
  
  # Number of trees with lowest MSE
  which.min(rf1$mse)
  
  # Produce variable importance plot
  varImpPlot(rf1) 

  # Tuning
  model_tuned1 <- tuneRF(
    traindata1[,1:32], # define predictor variables
    traindata1$G3, # define response variable
    ntreeTry=500,
    mtryStart=4, 
    stepFactor=1.5,
    improve=0.01,
    trace=FALSE) #don't show real-time progress
  
  best.m1 <- model_tuned1[model_tuned1[, 2] == min(model_tuned1[, 2]), 1]
  print(model_tuned1)
  print(best.m1)
  
  # set best.m
  set.seed(123)
  rf1 <- randomForest(formula= G3 ~ ., data = traindata1, ntree= 500, mtry=best.m1, nodesize= 5, type="class")
  rf1
  plot(rf1)
  
  #Prediction
  predictdata_rf1 <-predict(rf1, testdata1)
  table(predictdata_rf1, testdata1$G3)
  
  #Accuracy
  table_mat2_1 <- table(testdata1$G3, predictdata_rf1)
  
  accuracy_Test2_1 <- sum(diag(table_mat2_1)) / sum(table_mat2_1)
  print(paste('Accuracy for random forest algorithm', accuracy_Test2_1))
  
