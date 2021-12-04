

### Manuel Cañete Ríos
### 08/11/2021
### This script will be used to perform KNN classification

### Dependencies
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(caret)
library(data.table)
library(doParallel)
library(pROC)

### We turn on parallel computing
cl <- makePSOCKcluster(12)
registerDoParallel(cl)

### Input directory
input_dir <- "C:/Users/mcr3927/Desktop/TFM/Inputs"


### We keep the pixels of interest
arranged_data <- fread(file.path(input_dir, "pca_data_1024.txt")) %>%
  mutate(Pattern = factor(Pattern))


### We generate the training and test datasets
set.seed(123456)
indxTrain <- createDataPartition(y = arranged_data$Pattern, p = 0.75, list = FALSE)
training <- arranged_data[indxTrain,]
testing <- arranged_data[-indxTrain,]

### Generate KNN model
set.seed(654321)
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 5, 
                     classProbs = T, 
                     summaryFunction = twoClassSummary, 
                     returnData = FALSE, 
                     trim = T) 
                                                                                                          
knnFit <- train(Pattern ~ ., 
                data = training, 
                method = "knn", 
                trControl = ctrl, 
                tuneLength = 20,
                metric = "ROC") # ROC is not performing well

### We print KNN model info
knnFit

### We plot the cross-validation results
plot(knnFit)

### We use this model to predict classes on the test data
pred <- predict(knnFit, testing)
confusionMatrix(pred, testing$Pattern)
pred <- predict(knnFit, testing)
resROC <- roc(testing$Pattern, as.numeric(pred))
plot(resROC)

### We stop parallel computing
stopCluster(cl)
