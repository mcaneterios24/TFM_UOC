
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
library(OpenImageR)

reticulate::use_python("C:/Users/manue/AppData/Local/R-MINI~1/envs/tf_image", required = T)

### Input directory
input_dir <- "C:/Users/manue/OneDrive/Escritorio/Master/TFM"

### We need to import the original pixel data (all of them, without PCA)
available_images <- read.delim(file.path(input_dir, "Analysis", "Final outputs", "available_images.txt")) %>%
  mutate(Image3 = str_replace(Image, "&", "_")) %>%
  mutate(Pattern = factor(Pattern))

### Get pixel values and crop images, convert data to array
cropped_data <- lapply(available_images$Image, function(x){
  filename <- str_replace(x, ".tiff", ".txt")
  vect <- scan(file.path(input_dir, "Imatges", "Input data", "Pixels", filename), quiet = T)
  mat <- matrix(vect, nrow = 1024)
  cropImage(mat, new_width = 362:662, new_height = 362:662, type = "user_defined") # We had to scale due to memory problems with the PCA
}) %>%
  simplify2array(.) %>%
  aperm(., c(3, 1, 2))


### We generate the sets
set.seed(123456)
indxTrain <- createDataPartition(y = available_images$Pattern, p = 0.75, list = FALSE)
training <- cropped_data[indxTrain,,]/255 ## WORKS BETTER NORMALIZING INTENSITY!
testing <- cropped_data[-indxTrain,,]/255

train.y <- to_categorical(matrix(as.numeric(available_images$Pattern[indxTrain])-1))
test.y <- to_categorical(matrix(as.numeric(available_images$Pattern[-indxTrain])-1))

dim(training) <- c(dim(training), 1)
dim(testing) <- c(dim(testing), 1)

### Instantiate model

model <- keras_model_sequential()

model %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu", 
                input_shape = c(301, 301, 1)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = ncol(train.y), activation = "softmax")

model %>% compile(optimizer = "rmsprop", 
                  loss = "binary_crossentropy",  
                  metric=c("AUC"))

print(model)

history <- model %>%
  fit(x = training, y = train.y,
      epochs = 30,
      validation_data = list(testing, test.y))


plot(history)

pred2 <- predict_classes(model, testing)
confusionMatrix(table(as.vector(pred2), as.numeric(available_images$Pattern[-indxTrain])-1)) 
