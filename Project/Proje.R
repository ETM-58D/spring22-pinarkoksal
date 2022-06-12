# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("lubridate")


require(data.table)
require(ggplot2)
require(lubridate)
require(ggcorrplot)
require(corrplot)
require(GGally)
require(skimr)
require(dplyr)
library("rattle")
library("caret")
library(tidyverse)
library(readxl)
library(magrittr)
library(tidyquant)
library(tsibble)
library(feasts)
library(glmnet)
library(ranger)


# production=fread("C:/ETM58D/project/project_data/production.csv")
# weatherdata=fread("C:/ETM58D/project/project_data/long_weather.csv")

production=fread("C:/Users/pinar/Desktop/project_data/production.csv")
weatherdata=fread("C:/Users/pinar/Desktop/project_data/long_weather.csv")


head(production)
head(weatherdata)

datamerge <- merge(weatherdata,production)
head(datamerge)

production[,timestamp:=as.Date(date)+dhours(hour)]
head(production)

ggplot(production,aes(x=timestamp,y=production)) + geom_line()

ggplot(production[date>'2022-04-25'],aes(x=timestamp,y=production)) + geom_line()

plot(datamerge$hour,datamerge$production, xlab = "Hour", ylab = "Production")

# Removing non-productive hours
datamerge = filter(datamerge, hour>4,hour<21)
tail(datamerge)
hist(datamerge$production)

dswrf=weatherdata[variable=='DSWRF' & lat==36.5 & lon==33.25]
dswrf[,timestamp:=as.Date(date)+dhours(hour)]
head(dswrf)

ggplot(dswrf[date>'2022-04-25'],aes(x=timestamp,y=value)) + geom_line() + geom_line()

wide_weather=dcast(datamerge,date+hour+production~variable+lat+lon,value.var='value')
tail(wide_weather)

plot_table = dcast(datamerge,date+hour+production+lat+lon~variable,value.var='value')
tail(plot_table)
str(plot_table)

# Plots to see the relationship between variables and production
plot(plot_table$production, plot_table$CLOUD_LOW_LAYER)
plot(plot_table$production, plot_table$DSWRF)
plot(plot_table$production, plot_table$REL_HUMIDITY)
plot(plot_table$production, plot_table$TEMP)

# Seasonality check
plot(month(plot_table$date), plot_table$production)

correl=cor(as.matrix(wide_weather[,2:ncol(wide_weather)]),as.matrix(wide_weather[,2:ncol(wide_weather)]))
correl

# ggpairs(wide_weather[,2:ncol(wide_weather)])

ggcorrplot(correl,hc.order = TRUE, type = "lower",lab = TRUE)
  
skim(wide_weather)

lin_reg = lm(production~., wide_weather)
lin_reg
summary(lin_reg)

# Since same variables are highly correlated in different coordinates, taking average to unify those variables would increase the R squared value.
wide_weather2 = mutate(wide_weather, avg_cloud = ((CLOUD_LOW_LAYER_36.25_33 + CLOUD_LOW_LAYER_36.25_33.25 + CLOUD_LOW_LAYER_36.25_33.5 +CLOUD_LOW_LAYER_36.5_33 
                                          + CLOUD_LOW_LAYER_36.5_33.25 +CLOUD_LOW_LAYER_36.5_33.5 +CLOUD_LOW_LAYER_36.75_33 +CLOUD_LOW_LAYER_36.75_33.25 
                                          + CLOUD_LOW_LAYER_36.75_33.5)/9),
                                        avg_temp = ((TEMP_36.25_33 + TEMP_36.25_33.25 + TEMP_36.25_33.5 + TEMP_36.5_33 + TEMP_36.5_33.25 + TEMP_36.5_33.5 + TEMP_36.75_33
                                          + TEMP_36.75_33.25)/9),
                                        avg_dswrf = ((DSWRF_36.25_33 + DSWRF_36.25_33.25 + DSWRF_36.25_33.5 + DSWRF_36.5_33 + DSWRF_36.5_33.25 + DSWRF_36.5_33.5
                                          + DSWRF_36.75_33 + DSWRF_36.75_33.25 + DSWRF_36.75_33.5)/9),
                                        avg_hum = ((REL_HUMIDITY_36.25_33 + REL_HUMIDITY_36.25_33.25 + REL_HUMIDITY_36.25_33.5 + REL_HUMIDITY_36.5_33 + REL_HUMIDITY_36.5_33.25
                                          + REL_HUMIDITY_36.5_33.5 + REL_HUMIDITY_36.75_33 + REL_HUMIDITY_36.75_33.25 + REL_HUMIDITY_36.75_33.5)/9)) %>% select(date, hour, production,avg_cloud, avg_temp, avg_dswrf, avg_hum)
head(wide_weather2)
skim(wide_weather2)

# Creating lagged variables for time forecasting. 1 day lag and 1 week lag is obtained.
wide_weather2 <- mutate(wide_weather2,
          production_lag1 = lag(production, 16, default = mean(wide_weather2$production)),
         avg_cloud_lag1 = lag(avg_cloud, 16, default = mean(wide_weather2$avg_cloud)),
         avg_temp_lag1 = lag(avg_temp, 16, default = mean(wide_weather2$avg_temp)),
         avg_dswrf_lag1 = lag(avg_dswrf, 16, default = mean(wide_weather2$avg_dswrf)),
         avg_hum_lag1 = lag(avg_hum, 16, default = mean(wide_weather2$avg_hum)),
         production_lag7 = lag(production, 112, default = mean(wide_weather2$production)),
         avg_cloud_lag7 = lag(avg_cloud, 112,default = mean(wide_weather2$avg_cloud)),
         avg_temp_lag7 = lag(avg_temp, 112, default = mean(wide_weather2$avg_temp)),
         avg_dswrf_lag7 = lag(avg_dswrf, 112, default = mean(wide_weather2$avg_dswrf))) %>%filter(date >= as_date('2021-02-02'))

head(wide_weather2, 18)

# Split train and test data

train_weather <- wide_weather2[ which(wide_weather2$date<as.Date('2022-03-01'))]
test_weather <- wide_weather2[ which(wide_weather2$date>=as.Date('2022-03-01'))]

head(train_weather)

# Splitting the data hourly to have a different model for every hour
train_hour05 <- train_weather[ which(train_weather$hour==5),]
train_hour06 <- train_weather[ which(train_weather$hour==6),]
train_hour07 <- train_weather[ which(train_weather$hour==7),]
train_hour08 <- train_weather[ which(train_weather$hour==8),]
train_hour09 <- train_weather[ which(train_weather$hour==9),]
train_hour10 <- train_weather[ which(train_weather$hour==10),]
train_hour11 <- train_weather[ which(train_weather$hour==11),]
train_hour12 <- train_weather[ which(train_weather$hour==12),]
train_hour13 <- train_weather[ which(train_weather$hour==13),]
train_hour14 <- train_weather[ which(train_weather$hour==14),]
train_hour15 <- train_weather[ which(train_weather$hour==15),]
train_hour16 <- train_weather[ which(train_weather$hour==16),]
train_hour17 <- train_weather[ which(train_weather$hour==17),]
train_hour18 <- train_weather[ which(train_weather$hour==18),]
train_hour19 <- train_weather[ which(train_weather$hour==19),]
train_hour20 <- train_weather[ which(train_weather$hour==20),]

test_hour05 <- test_weather[ which(test_weather$hour==5),]
test_hour06 <- test_weather[ which(test_weather$hour==6),]
test_hour07 <- test_weather[ which(test_weather$hour==7),]
test_hour08 <- test_weather[ which(test_weather$hour==8),]
test_hour09 <- test_weather[ which(test_weather$hour==9),]
test_hour10 <- test_weather[ which(test_weather$hour==10),]
test_hour11 <- test_weather[ which(test_weather$hour==11),]
test_hour12 <- test_weather[ which(test_weather$hour==12),]
test_hour13 <- test_weather[ which(test_weather$hour==13),]
test_hour14 <- test_weather[ which(test_weather$hour==14),]
test_hour15 <- test_weather[ which(test_weather$hour==15),]
test_hour16 <- test_weather[ which(test_weather$hour==16),]
test_hour17 <- test_weather[ which(test_weather$hour==17),]
test_hour18 <- test_weather[ which(test_weather$hour==18),]
test_hour19 <- test_weather[ which(test_weather$hour==19),]
test_hour20 <- test_weather[ which(test_weather$hour==20),]

# Get largest mean production level based on hour
meanProductions <- data.frame(c(mean(train_hour05$production),
                                mean(train_hour06$production),
                                mean(train_hour07$production),
                                mean(train_hour08$production),
                                mean(train_hour09$production),
                                mean(train_hour10$production),
                                mean(train_hour11$production),
                                mean(train_hour12$production),
                                mean(train_hour13$production),
                                mean(train_hour14$production),
                                mean(train_hour15$production),
                                mean(train_hour16$production),
                                mean(train_hour17$production),
                                mean(train_hour18$production),
                                mean(train_hour19$production),
                                mean(train_hour20$production)))

maxMeanProd <- max(meanProductions)
maxMeanProd

meanProductions

# Maximum mean production occurs at 12AM with 27.59931

# Creating 12AM model to obtain tuning parameters for all hours

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5)
# Decision tree
hour12_dectree <- train(production~., 
                        train_hour12[,3:ncol(train_hour12)],
                        method = "rpart",
                        trControl = fitControl,
                        tuneLength = 5)

hour12_dectree
plot(hour12_dectree)

# cp is tuned to  0.03190291 (lowest value)

fancyRpartPlot(hour12_dectree$finalModel)
hour12_dectree$finalModel

# Linear regression
hour12_linreg <- train(production ~ .,
                   train_hour12[,3:ncol(train_hour12)],
                   method = "lm",
                   trControl = fitControl,
                   tuneLength = 5)

summary(hour12_linreg)
hour12_linreg$finalModel

# Tuning parameter 'intercept' was held constant at a value of TRUE

# Random Forest
hour12_ranfor <- train(production~.,
                       train_hour12[,3:ncol(train_hour12)],
                       method = "ranger",
                       trControl = fitControl,
                       tuneLength = 5)

hour12_ranfor
plot(hour12_ranfor)

#mtry tuned to 4 according to RMSE.

# GLM
hour12_glm <- train(production~.,
                  method = "glm",
                  train_hour12[,3:ncol(train_hour12)],
                  trControl = fitControl,
                  tuneLength = 5)

hour12_glm
summary(hour12_glm)
plot(hour12_glm)

# GLM does not have tuning https://stackoverflow.com/questions/47822694/logistic-regression-tuning-parameter-grid-in-r-caret-package 

#Prepare models for each hour with tuned parameters

# Hour 05

# Decision tree
hour05_dectree <- train(production~., 
                        train_hour05[,3:ncol(train_hour05)],
                        method = "rpart",
                        trControl = fitControl,
                        tuneLength = 5,
                        cp=0.03190291)
hour05_dectree

# Linear regression
hour05_linreg <- train(production~.,
                       train_hour05[,3:ncol(train_hour05)],
                       method = "lm",
                       trControl = fitControl,
                       tuneLength = 5,
                       tuneGrid = expand.grid(intercept = TRUE))

hour05_linreg

# Random Forest
hour05_ranfor <- train(production~.,
                       train_hour05[,3:ncol(train_hour05)],
                       method = "ranger",
                       trControl = fitControl,
                       tuneLength = 5,
                        tuneGrid = expand.grid(mtry = 4, splitrule = "variance", min.node.size = 5))

hour05_ranfor


#GLM

hour05_glm <- train(production~.,
                    method = "glm",
                    train_hour05[,3:ncol(train_hour05)],
                    trControl = fitControl,
                    tuneLength = 5)

hour05_glm
summary(hour05_glm)

# Model Comparison
Comp05 <- resamples(list(Ranfor = hour05_ranfor, LinReg=hour05_linreg, DecTree=hour05_dectree, GLM = hour05_glm))
Comp05
summary(Comp05)
bwplot(Comp05)

# Hour 06

#Decision tree
hour06_dectree <- train(production~., 
                        train_hour06[,3:ncol(train_hour06)],
                        method = "rpart",
                        trControl = fitControl,
                        tuneLength = 5,
                        cp=0.03190291)
hour06_dectree


#Linear regression
hour06_linreg <- train(production~.,
                       train_hour06[,3:ncol(train_hour06)],
                       method = "lm",
                       trControl = fitControl,
                       tuneLength = 5,
                       tuneGrid = expand.grid(intercept = TRUE))

hour06_linreg

# Random Forest
hour06_ranfor <- train(production~.,
                       train_hour06[,3:ncol(train_hour06)],
                       method = "ranger",
                       trControl = fitControl,
                       tuneLength = 5,
                        tuneGrid = expand.grid(mtry = 4, splitrule = "variance", min.node.size = 5))

hour06_ranfor

# GLM
hour06_glm <- train(production~.,
                    method = "glm",
                    train_hour06[,3:ncol(train_hour06)],
                    trControl = fitControl,
                    tuneLength = 5)

hour06_glm
summary(hour06_glm)

# Model Comparison
Comp06 <- resamples(list(Ranfor = hour06_ranfor, LinReg=hour06_linreg, DecTree=hour06_dectree, GLM = hour06_glm))
Comp06
summary(Comp06)
bwplot(Comp06)

# Hour 07

# Decision tree
hour07_dectree <- train(production~., 
                        train_hour07[,3:ncol(train_hour07)],
                        method = "rpart",
                        trControl = fitControl,
                        tuneLength = 5,
                        cp=0.03190291)
hour07_dectree


# Linear regression
hour07_linreg <- train(production~.,
                       train_hour07[,3:ncol(train_hour07)],
                       method = "lm",
                       trControl = fitControl,
                       tuneLength = 5,
                       tuneGrid = expand.grid(intercept = TRUE))

hour07_linreg

# Random Forest
hour07_ranfor <- train(production~.,
                       train_hour07[,3:ncol(train_hour07)],
                       method = "ranger",
                       trControl = fitControl,
                       tuneLength = 5,
                        tuneGrid = expand.grid(mtry = 4, splitrule = "variance", min.node.size = 5))

hour07_ranfor

# GLM
hour07_glm <- train(production~.,
                    method = "glm",
                    train_hour07[,3:ncol(train_hour07)],
                    trControl = fitControl,
                    tuneLength = 5)

hour07_glm
summary(hour07_glm)

# Model Comparison
Comp07 <- resamples(list(Ranfor = hour07_ranfor, LinReg=hour07_linreg, DecTree=hour07_dectree, GLM = hour07_glm))
Comp07
summary(Comp07)
bwplot(Comp07)

# Hour 08

# Decision tree
hour08_dectree <- train(production~., 
                        train_hour08[,3:ncol(train_hour08)],
                        method = "rpart",
                        trControl = fitControl,
                        tuneLength = 5,
                        cp=0.03190291)
hour08_dectree


# Linear regression
hour08_linreg <- train(production~.,
                       train_hour08[,3:ncol(train_hour08)],
                       method = "lm",
                       trControl = fitControl,
                       tuneLength = 5,
                       tuneGrid = expand.grid(intercept = TRUE))

hour08_linreg

# Random Forest
hour08_ranfor <- train(production~.,
                       train_hour08[,3:ncol(train_hour08)],
                       method = "ranger",
                       trControl = fitControl,
                       tuneLength = 5,
                        tuneGrid = expand.grid(mtry = 4, splitrule = "variance", min.node.size = 5))

hour08_ranfor

# GLM
hour08_glm <- train(production~.,
                    method = "glm",
                    train_hour08[,3:ncol(train_hour08)],
                    trControl = fitControl,
                    tuneLength = 5)

hour08_glm
summary(hour08_glm)

# Model Comparison
Comp08 <- resamples(list(Ranfor = hour08_ranfor, LinReg=hour08_linreg, DecTree=hour08_dectree, GLM = hour08_glm))
Comp08
summary(Comp08)
bwplot(Comp08)

# Hour 09

# Decision tree
hour09_dectree <- train(production~., 
                        train_hour09[,3:ncol(train_hour09)],
                        method = "rpart",
                        trControl = fitControl,
                        tuneLength = 5,
                        cp=0.03190291)
hour09_dectree


# Linear regression
hour09_linreg <- train(production~.,
                       train_hour09[,3:ncol(train_hour09)],
                       method = "lm",
                       trControl = fitControl,
                       tuneLength = 5,
                       tuneGrid = expand.grid(intercept = TRUE))

hour09_linreg

# Random Forest
hour09_ranfor <- train(production~.,
                       train_hour09[,3:ncol(train_hour09)],
                       method = "ranger",
                       trControl = fitControl,
                       tuneLength = 5,
                        tuneGrid = expand.grid(mtry = 4, splitrule = "variance", min.node.size = 5))

hour09_ranfor

#GLM
hour09_glm <- train(production~.,
                    method = "glm",
                    train_hour09[,3:ncol(train_hour09)],
                    trControl = fitControl,
                    tuneLength = 5)

hour09_glm
summary(hour09_glm)

# Model Comparison
Comp09 <- resamples(list(Ranfor = hour09_ranfor, LinReg=hour09_linreg, DecTree=hour09_dectree, GLM = hour09_glm))
Comp09
summary(Comp09)
bwplot(Comp09)

# Hour 10

# Decision tree
hour10_dectree <- train(production~., 
                        train_hour10[,3:ncol(train_hour10)],
                        method = "rpart",
                        trControl = fitControl,
                        tuneLength = 5,
                        cp=0.03190291)
hour10_dectree


# Linear regression
hour10_linreg <- train(production~.,
                       train_hour10[,3:ncol(train_hour10)],
                       method = "lm",
                       trControl = fitControl,
                       tuneLength = 5,
                       tuneGrid = expand.grid(intercept = TRUE))

hour10_linreg

# Random Forest
hour10_ranfor <- train(production~.,
                       train_hour10[,3:ncol(train_hour10)],
                       method = "ranger",
                       trControl = fitControl,
                       tuneLength = 5,
                        tuneGrid = expand.grid(mtry = 4, splitrule = "variance", min.node.size = 5))

hour10_ranfor

# GLM
hour10_glm <- train(production~.,
                    method = "glm",
                    train_hour10[,3:ncol(train_hour10)],
                    trControl = fitControl,
                    tuneLength = 5)

hour10_glm
summary(hour10_glm)

# Model Comparison
Comp10 <- resamples( list( Ranfor = hour10_ranfor, LinReg=hour10_linreg, DecTree=hour10_dectree, GLM = hour10_glm))
Comp10
summary(Comp10)
bwplot(Comp10)

# Hour 11

# Decision tree
hour11_dectree <- train(production~., 
                        train_hour11[,3:ncol(train_hour11)],
                        method = "rpart",
                        trControl = fitControl,
                        tuneLength = 5,
                        cp=0.03190291)
hour11_dectree

# Linear regressions
hour11_linreg <- train(production~.,
                       train_hour11[,3:ncol(train_hour11)],
                       method = "lm",
                       trControl = fitControl,
                       tuneLength = 5,
                       tuneGrid = expand.grid(intercept = TRUE))

hour11_linreg

# Random Forest

hour11_ranfor <- train(production~.,
                       train_hour11[,3:ncol(train_hour11)],
                       method = "ranger",
                       trControl = fitControl,
                       tuneLength = 5,
                        tuneGrid = expand.grid(mtry = 4, splitrule = "variance", min.node.size = 5))

hour11_ranfor

# GLM

hour11_glm <- train(production~.,
                    method = "glm",
                    train_hour11[,3:ncol(train_hour11)],
                    trControl = fitControl,
                    tuneLength = 5)

hour11_glm
summary(hour11_glm)

# Model Comparison
Comp11 <- resamples(list(Ranfor = hour11_ranfor, LinReg=hour11_linreg, DecTree=hour11_dectree, GLM = hour11_glm))
Comp11
summary(Comp11)
bwplot(Comp11)

# Hour 12

# Decision tree
hour12_dectree <- train(production~., 
                        train_hour12[,3:ncol(train_hour12)],
                        method = "rpart",
                        trControl = fitControl,
                        tuneLength = 5,
                        cp=0.03190291)
hour12_dectree


# Linear regression
hour12_linreg <- train(production~.,
                       train_hour12[,3:ncol(train_hour12)],
                       method = "lm",
                       trControl = fitControl,
                       tuneLength = 5,
                       tuneGrid = expand.grid(intercept = TRUE))

hour12_linreg

# Random Forest
hour12_ranfor <- train(production~.,
                       train_hour12[,3:ncol(train_hour12)],
                       method = "ranger",
                       trControl = fitControl,
                       tuneLength = 5,
                        tuneGrid = expand.grid(mtry = 4, splitrule = "variance", min.node.size = 5))

hour12_ranfor

# GLM
hour12_glm <- train(production~.,
                    method = "glm",
                    train_hour12[,3:ncol(train_hour12)],
                    trControl = fitControl,
                    tuneLength = 5)

hour12_glm
summary(hour12_glm)

# Model Comparison
Comp12 <- resamples(list(Ranfor = hour12_ranfor, LinReg=hour12_linreg, DecTree=hour12_dectree, GLM = hour12_glm))
Comp12
summary(Comp12)
bwplot(Comp12)

# Hour 13

# Decision tree
hour13_dectree <- train(production~., 
                        train_hour13[,3:ncol(train_hour13)],
                        method = "rpart",
                        trControl = fitControl,
                        tuneLength = 5,
                        cp=0.03190291)
hour13_dectree

# Linear regression
hour13_linreg <- train(production~.,
                       train_hour13[,3:ncol(train_hour13)],
                       method = "lm",
                       trControl = fitControl,
                       tuneLength = 5,
                       tuneGrid = expand.grid(intercept = TRUE))

hour13_linreg

# Random Forest
hour13_ranfor <- train(production~.,
                       train_hour13[,3:ncol(train_hour13)],
                       method = "ranger",
                       trControl = fitControl,
                       tuneLength = 5,
                        tuneGrid = expand.grid(mtry = 4, splitrule = "variance", min.node.size = 5))

hour13_ranfor

# GLM
hour13_glm <- train(production~.,
                    method = "glm",
                    train_hour13[,3:ncol(train_hour13)],
                    trControl = fitControl,
                    tuneLength = 5)

hour13_glm
summary(hour13_glm)

# Model Comparison
Comp13 <- resamples(list(Ranfor = hour13_ranfor, LinReg=hour13_linreg, DecTree=hour13_dectree, GLM = hour13_glm))
Comp13
summary(Comp13)
bwplot(Comp13)

# Hour 14

# Decision tree
hour14_dectree <- train(production~., 
                        train_hour14[,3:ncol(train_hour14)],
                        method = "rpart",
                        trControl = fitControl,
                        tuneLength = 5,
                        cp=0.03190291)
hour14_dectree

# Linear regression
hour14_linreg <- train(production~.,
                       train_hour14[,3:ncol(train_hour14)],
                       method = "lm",
                       trControl = fitControl,
                       tuneLength = 5,
                       tuneGrid = expand.grid(intercept = TRUE))

hour14_linreg

# Random Forest
hour14_ranfor <- train(production~.,
                       train_hour14[,3:ncol(train_hour14)],
                       method = "ranger",
                       trControl = fitControl,
                       tuneLength = 5,
                        tuneGrid = expand.grid(mtry = 4, splitrule = "variance", min.node.size = 5))

hour14_ranfor

# GLM
hour14_glm <- train(production~.,
                    method = "glm",
                    train_hour14[,3:ncol(train_hour14)],
                    trControl = fitControl,
                    tuneLength = 5)

hour14_glm
summary(hour14_glm)

# Model Comparison
Comp14 <- resamples(list(Ranfor = hour14_ranfor, LinReg=hour14_linreg, DecTree=hour14_dectree, GLM = hour14_glm))
Comp14
summary(Comp14)
bwplot(Comp14)

# Hour 15

# Decision tree
hour15_dectree <- train(production~., 
                        train_hour15[,3:ncol(train_hour15)],
                        method = "rpart",
                        trControl = fitControl,
                        tuneLength = 5,
                        cp=0.03190291)
hour15_dectree

# Linear regression
hour15_linreg <- train(production~.,
                       train_hour15[,3:ncol(train_hour15)],
                       method = "lm",
                       trControl = fitControl,
                       tuneLength = 5,
                       tuneGrid = expand.grid(intercept = TRUE))

hour15_linreg

# Random Forest
hour15_ranfor <- train(production~.,
                       train_hour15[,3:ncol(train_hour15)],
                       method = "ranger",
                       trControl = fitControl,
                       tuneLength = 5,
                        tuneGrid = expand.grid(mtry = 4, splitrule = "variance", min.node.size = 5))

hour15_ranfor

# GLM
hour15_glm <- train(production~.,
                    method = "glm",
                    train_hour15[,3:ncol(train_hour15)],
                    trControl = fitControl,
                    tuneLength = 5)

hour15_glm
summary(hour15_glm)

# Model Comparison
Comp15 <- resamples(list(Ranfor = hour15_ranfor, LinReg=hour15_linreg, DecTree=hour15_dectree, GLM = hour15_glm))
Comp15
summary(Comp15)
bwplot(Comp15)

# Hour 16

# Decision tree
hour16_dectree <- train(production~., 
                        train_hour16[,3:ncol(train_hour16)],
                        method = "rpart",
                        trControl = fitControl,
                        tuneLength = 5,
                        cp=0.03190291)
hour16_dectree

# Linear regression
hour16_linreg <- train(production~.,
                       train_hour16[,3:ncol(train_hour16)],
                       method = "lm",
                       trControl = fitControl,
                       tuneLength = 5,
                       tuneGrid = expand.grid(intercept = TRUE))

hour16_linreg

# Random Forest
hour16_ranfor <- train(production~.,
                       train_hour16[,3:ncol(train_hour16)],
                       method = "ranger",
                       trControl = fitControl,
                       tuneLength = 5,
                        tuneGrid = expand.grid(mtry = 4, splitrule = "variance", min.node.size = 5))

hour16_ranfor

# GLM
hour16_glm <- train(production~.,
                    method = "glm",
                    train_hour16[,3:ncol(train_hour16)],
                    trControl = fitControl,
                    tuneLength = 5)

hour16_glm
summary(hour16_glm)

# Model Comparison
Comp16 <- resamples(list(Ranfor = hour16_ranfor, LinReg=hour16_linreg, DecTree=hour16_dectree, GLM = hour16_glm))
Comp16
summary(Comp16)
bwplot(Comp16)

# Hour 17

# Decision tree
hour17_dectree <- train(production~., 
                        train_hour17[,3:ncol(train_hour17)],
                        method = "rpart",
                        trControl = fitControl,
                        tuneLength = 5,
                        cp=0.03190291)
hour17_dectree

# Linear regression
hour17_linreg <- train(production~.,
                       train_hour17[,3:ncol(train_hour17)],
                       method = "lm",
                       trControl = fitControl,
                       tuneLength = 5,
                       tuneGrid = expand.grid(intercept = TRUE))

hour17_linreg

# Random Forest

hour17_ranfor <- train(production~.,
                       train_hour17[,3:ncol(train_hour17)],
                       method = "ranger",
                       trControl = fitControl,
                       tuneLength = 5,
                        tuneGrid = expand.grid(mtry = 4, splitrule = "variance", min.node.size = 5))

hour17_ranfor

# GLM
hour17_glm <- train(production~.,
                    method = "glm",
                    train_hour17[,3:ncol(train_hour17)],
                    trControl = fitControl,
                    tuneLength = 5)

hour17_glm
summary(hour17_glm)

# Model Comparison
Comp17 <- resamples(list(Ranfor = hour17_ranfor, LinReg=hour17_linreg, DecTree=hour17_dectree, GLM = hour17_glm))
Comp17
summary(Comp17)
bwplot(Comp17)

# Hour 18

# Decision tree
hour18_dectree <- train(production~., 
                        train_hour18[,3:ncol(train_hour18)],
                        method = "rpart",
                        trControl = fitControl,
                        tuneLength = 5,
                        cp=0.03190291)
hour18_dectree

# Linear regression
hour18_linreg <- train(production~.,
                       train_hour18[,3:ncol(train_hour18)],
                       method = "lm",
                       trControl = fitControl,
                       tuneLength = 5,
                       tuneGrid = expand.grid(intercept = TRUE))

hour18_linreg

# Random Forest
hour18_ranfor <- train(production~.,
                       train_hour18[,3:ncol(train_hour18)],
                       method = "ranger",
                       trControl = fitControl,
                       tuneLength = 5,
                        tuneGrid = expand.grid(mtry = 4, splitrule = "variance", min.node.size = 5))

hour18_ranfor

# GLM
hour18_glm <- train(production~.,
                    method = "glm",
                    train_hour18[,3:ncol(train_hour18)],
                    trControl = fitControl,
                    tuneLength = 5)

hour18_glm
summary(hour18_glm)

# Model Comparison
Comp18 <- resamples(list(Ranfor = hour18_ranfor, LinReg=hour18_linreg, DecTree=hour18_dectree, GLM = hour18_glm))
Comp18
summary(Comp18)
bwplot(Comp18)

# Hour 19

# Decision tree
hour19_dectree <- train(production~., 
                        train_hour19[,3:ncol(train_hour19)],
                        method = "rpart",
                        trControl = fitControl,
                        tuneLength = 5,
                        cp=0.03190291)
hour19_dectree

# Linear regression
hour19_linreg <- train(production~.,
                       train_hour19[,3:ncol(train_hour19)],
                       method = "lm",
                       trControl = fitControl,
                       tuneLength = 5,
                       tuneGrid = expand.grid(intercept = TRUE))

hour19_linreg

# Random Forest
hour19_ranfor <- train(production~.,
                       train_hour19[,3:ncol(train_hour19)],
                       method = "ranger",
                       trControl = fitControl,
                       tuneLength = 5,
                        tuneGrid = expand.grid(mtry = 4, splitrule = "variance", min.node.size = 5))

hour19_ranfor

# GLM
hour19_glm <- train(production~.,
                    method = "glm",
                    train_hour19[,3:ncol(train_hour19)],
                    trControl = fitControl,
                    tuneLength = 5)

hour19_glm
summary(hour19_glm)

# Model Comparison
Comp19 <- resamples(list(Ranfor = hour19_ranfor, LinReg=hour19_linreg, DecTree=hour19_dectree, GLM = hour19_glm))
Comp19
summary(Comp19)
bwplot(Comp19)

# Hour 20

# Decision tree
hour20_dectree <- train(production~., 
                        train_hour20[,3:ncol(train_hour20)],
                        method = "rpart",
                        trControl = fitControl,
                        tuneLength = 5,
                        cp=0.03190291)
hour20_dectree

# Linear regression
hour20_linreg <- train(production~.,
                       train_hour20[,3:ncol(train_hour20)],
                       method = "lm",
                       trControl = fitControl,
                       tuneLength = 5,
                       tuneGrid = expand.grid(intercept = TRUE))

hour20_linreg

# Random Forest
hour20_ranfor <- train(production~.,
                       train_hour20[,3:ncol(train_hour20)],
                       method = "ranger",
                       trControl = fitControl,
                       tuneLength = 5,
                       tuneGrid = expand.grid(mtry = 4, splitrule = "variance", min.node.size = 5))

hour20_ranfor

# GLM
hour20_glm <- train(production~.,
                    method = "glm",
                    train_hour20[,3:ncol(train_hour20)],
                    trControl = fitControl,
                    tuneLength = 5)

hour20_glm
summary(hour20_glm)

# Model Comparison
Comp20 <- resamples(list(Ranfor = hour20_ranfor, LinReg=hour20_linreg, DecTree=hour20_dectree, GLM = hour20_glm))
Comp20
summary(Comp20)
bwplot(Comp20)


# Random forest performed better for the majority. Test predictions:
hour05predtest <- predict(hour05_ranfor$finalModel, data = test_hour05)
hour06predtest <- predict(hour06_ranfor$finalModel, data = test_hour06)
hour07predtest <- predict(hour07_ranfor$finalModel, data = test_hour07)
hour08predtest <- predict(hour08_ranfor$finalModel, data = test_hour08)
hour09predtest <- predict(hour09_ranfor$finalModel, data = test_hour09)
hour10predtest <- predict(hour10_ranfor$finalModel, data = test_hour10)
hour11predtest <- predict(hour11_ranfor$finalModel, data = test_hour11)
hour12predtest <- predict(hour12_ranfor$finalModel, data = test_hour12)
hour13predtest <- predict(hour13_ranfor$finalModel, data = test_hour13)
hour14predtest <- predict(hour14_ranfor$finalModel, data = test_hour14)
hour15predtest <- predict(hour15_ranfor$finalModel, data = test_hour15)
hour16predtest <- predict(hour16_ranfor$finalModel, data = test_hour16)
hour17predtest <- predict(hour17_ranfor$finalModel, data = test_hour17)
hour18predtest <- predict(hour18_ranfor$finalModel, data = test_hour18)
hour19predtest <- predict(hour19_ranfor$finalModel, data = test_hour19)
hour20predtest <- predict(hour20_ranfor$finalModel, data = test_hour20)


# Test predictions are merged with test data set for each hour.
test_hour05merged<-test_hour05%>%select(date, hour, production)%>%mutate(Prediction=hour05predtest$predictions)
test_hour06merged<-test_hour06%>%select(date, hour, production)%>%mutate(Prediction=hour06predtest$predictions)
test_hour07merged<-test_hour07%>%select(date, hour, production)%>%mutate(Prediction=hour07predtest$predictions)
test_hour08merged<-test_hour08%>%select(date, hour, production)%>%mutate(Prediction=hour08predtest$predictions)
test_hour09merged<-test_hour09%>%select(date, hour, production)%>%mutate(Prediction=hour09predtest$predictions)
test_hour10merged<-test_hour10%>%select(date, hour, production)%>%mutate(Prediction=hour10predtest$predictions)
test_hour11merged<-test_hour11%>%select(date, hour, production)%>%mutate(Prediction=hour11predtest$predictions)
test_hour12merged<-test_hour12%>%select(date, hour, production)%>%mutate(Prediction=hour12predtest$predictions)
test_hour13merged<-test_hour13%>%select(date, hour, production)%>%mutate(Prediction=hour13predtest$predictions)
test_hour14merged<-test_hour14%>%select(date, hour, production)%>%mutate(Prediction=hour14predtest$predictions)
test_hour15merged<-test_hour15%>%select(date, hour, production)%>%mutate(Prediction=hour15predtest$predictions)
test_hour16merged<-test_hour16%>%select(date, hour, production)%>%mutate(Prediction=hour16predtest$predictions)
test_hour17merged<-test_hour17%>%select(date, hour, production)%>%mutate(Prediction=hour17predtest$predictions)
test_hour18merged<-test_hour18%>%select(date, hour, production)%>%mutate(Prediction=hour18predtest$predictions)
test_hour19merged<-test_hour19%>%select(date, hour, production)%>%mutate(Prediction=hour19predtest$predictions)
test_hour20merged<-test_hour20%>%select(date, hour, production)%>%mutate(Prediction=hour20predtest$predictions)

# Tests in each hour with predictions are binded.
test_merged <- rbind(test_hour05merged,
                     test_hour06merged,
                     test_hour07merged,
                     test_hour08merged,
                     test_hour09merged,
                     test_hour10merged,
                     test_hour11merged,
                     test_hour12merged,
                     test_hour13merged,
                     test_hour14merged,
                     test_hour15merged,
                     test_hour16merged,
                     test_hour17merged,
                     test_hour18merged,
                     test_hour19merged,
                     test_hour20merged)%>% arrange(date, hour)
test_merged

# Scatter Plot of Actual vs Predicted values for test set
plot(test_merged$Prediction, test_merged$production, xlab = "Predicted", ylab = "Actual", main = "Predicted-Actual Production for Test Data")
abline(a=0,b=1,col='blue', lty = 2)

# Histogram of residuals for test set
rf_residual_test <- test_merged$production - test_merged$Prediction
hist(rf_residual_test, xlab = "Residuals", main = "Residuals Histogram")

# Scatter Plot of Predicted vs Residuals values for test set
plot(test_merged$Prediction, rf_residual_test, xlab = "Predicted", ylab = "Residuals", main = "Predicted vs Residuals Plot for Random Forest Model with Test Set")
abline(h = 0, col = "red", lty = 2)

# RMSE of test set predictions
RMSE(test_merged$production, test_merged$Prediction, na.rm = TRUE)

