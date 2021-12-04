
### Manuel Cañete Ríos
### 08/11/2021
### This script will be used to perform DT classification

### Dependencies
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(caret)
library(data.table)
library(doParallel)
library(C50)

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


### Generate DT model
set.seed(654321)
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 5, 
                     classProbs = T, 
                     summaryFunction = twoClassSummary, 
                     returnData = F, 
                     trim = T) 

treeFit <- train(Pattern ~ ., 
                data = training, 
                method = "C5.0", 
                trControl = ctrl,
                tuneLength = 20,
                metric = "ROC")

treeFit2 <- train(Pattern ~ ., 
                 data = training, 
                 method = "rpart", 
                 trControl = ctrl,
                 tuneLength = 20,
                 metric = "ROC")

### We print DT model info
treeFit
treeFit2

### We plot the cross-validation results
plot(treeFit)
plot(treeFit2)


### We use this model to predict classes on the test data
pred <- predict(treeFit, testing)
confusionMatrix(pred, testing$Pattern)
resROC <- roc(testing$Pattern, as.numeric(pred))
plot(resROC)

pred2 <- predict(treeFit2, testing)
confusionMatrix(pred2, testing$Pattern)
resROC2 <- roc(testing$Pattern, as.numeric(pred2))
plot(resROC2)

### We stop parallel computing
stopCluster(cl)
