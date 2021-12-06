
### Manuel Cañete Ríos
### 21/11/2021
### This script will be used to perform CNN classification

library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(keras)
library(caret)
library(tensorflow)
library(reticulate)

reticulate::use_python("C:/Users/manue/AppData/Local/R-MINI~1/envs/tf_image", required = T)


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

train.x <- as.matrix(sapply(training[,2:785], scale), nrow = 589)
train.y <- to_categorical(matrix(as.numeric(training$Pattern)-1))

test.x <- as.matrix(sapply(testing[,2:785], scale), nrow = 195)
test.y <- to_categorical(matrix(as.numeric(testing$Pattern)-1))



### Instantiate model

model <- keras_model_sequential()

model %>%
  layer_dense(units = 64, activation = "sigmoid", input_shape = ncol(train.x)) %>%
  layer_dense(units = 64, activation = "sigmoid") %>%
  layer_dense(units = ncol(train.y), activation = "softmax")

model %>% compile(optimizer = "rmsprop", 
                  loss = "categorical_crossentropy",  
                  metric=c("AUC"))

print(model)

history <- fit(model, train.x, train.y, 
               epochs = 15, 
               batch_size = 50, 
               validation_data = list(test.x, test.y))

plot(history)

pred <- model %>% predict(test.x) %>% k_argmax()
confusionMatrix(pred, as.numeric(testing$Pattern)-1)
confusionMatrix(table(as.vector(pred), as.numeric(testing$Pattern)-1)) 
