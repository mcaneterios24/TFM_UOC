
### Manuel Cañete Ríos
### 21/11/2021
### This script will be used to perform SVM classification

library(dplyr)
library(stringr)
library(ggplot2)
library(caret)
library(data.table)
library(pROC)
library(doParallel)

### We turn on parallel computing
cl <- makePSOCKcluster(3)
registerDoParallel(cl)

### Input directory
input_dir <- "C:/Users/manue/OneDrive/Escritorio/Master/TFM/Analysis/Final outputs"


### We keep the pixels of interest
arranged_data <- fread(file.path(input_dir, "pca_data_1024.txt")) %>%
  mutate(Pattern = factor(Pattern))

### We generate the training and test datasets
set.seed(123456)
indxTrain <- createDataPartition(y = arranged_data$Pattern, p = 0.75, list = FALSE)
training <- arranged_data[indxTrain,]
testing <- arranged_data[-indxTrain,]

    ### Linear classifier
set.seed(654321)
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 5, 
                     classProbs = T, 
                     summaryFunction = twoClassSummary, 
                     returnData = F, 
                     trim = T) 

SVMFit <- train(Pattern ~ ., 
               data = training, 
               method = "svmLinear",
               trControl = ctrl,
               tuneGrid = expand.grid(C = seq(0, 2, length = 20)),
               metric = "ROC")

### We print RF model info
SVMFit

### We plot the cross-validation results
plot(SVMFit)

### We use this model to predict classes on the test data
pred <- predict(SVMFit, testing)
confusionMatrix(pred, testing$Pattern)

    ### Non-linear classifier --> Radial
set.seed(654321)
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 5, 
                     classProbs = T, 
                     summaryFunction = twoClassSummary, 
                     returnData = F, 
                     trim = T) 
SVMFit_radial <- train(Pattern ~ ., 
                data = training, 
                method = "svmRadial",
                trControl = ctrl,
                tuneLength = 15,
                metric = "ROC")

### We print RF model info
SVMFit_radial

### We plot the cross-validation results
plot(SVMFit_radial)

### We use this model to predict classes on the test data
pred <- predict(SVMFit_radial, testing)
confusionMatrix(pred, testing$Pattern)

### Non-linear classifier --> Radial2
set.seed(654321)
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 5, 
                     classProbs = T, 
                     summaryFunction = twoClassSummary, 
                     returnData = F, 
                     trim = T) 
svmGrid <- expand.grid(sigma= 2^c(-10.05, -10.075, -10.1, -10.15, -10.2), C= 2^c(0:10))

SVMFit_radial <- train(Pattern ~ ., 
                       data = training, 
                       method = "svmRadial",
                       trControl = ctrl,
                       tuneGrid = svmGrid,
                       metric = "ROC")

### We print RF model info
SVMFit_radial

### We plot the cross-validation results
plot(SVMFit_radial)

### We use this model to predict classes on the test data
pred <- predict(SVMFit_radial, testing)
confusionMatrix(pred, testing$Pattern)

    ### Non-linear classifier --> polynomial
set.seed(654321)
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3, 
                     classProbs = T, 
                     summaryFunction = twoClassSummary, 
                     returnData = F, 
                     trim = T) 
SVMFit_poly <- train(Pattern ~ ., 
                       data = training, 
                       method = "svmPoly",
                       trControl = ctrl,
                       tuneLength = 15,
                       metric = "ROC")

### We print RF model info
SVMFit_poly

### We plot the cross-validation results
plot(SVMFit_poly)

### We use this model to predict classes on the test data
pred <- predict(SVMFit_poly, testing)
confusionMatrix(pred, testing$Pattern)

### We stop parallel computing
stopCluster(cl)
