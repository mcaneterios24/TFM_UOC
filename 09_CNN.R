
### Manuel Cañete Ríos
### 06/12/2021
### This script will be used to perform CNN classification


### Dependencies
library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(keras)
library(caret)
library(tensorflow)
library(reticulate)
library(OpenImageR)

### Connect to pyhton tensorflow and keras
reticulate::use_python("path/to/miniconda/R-MINI~1/envs/tf_image", required = T)

### Input directory
input_dir <- "path/to/files"

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

# Training set
training <- cropped_data[indxTrain,,]/255
train.y <- to_categorical(matrix(as.numeric(available_images$Pattern[indxTrain])-1))
dim(training) <- c(dim(training), 1)

# Testing set
testing <- cropped_data[-indxTrain,,]/255
test.y <- to_categorical(matrix(as.numeric(available_images$Pattern[-indxTrain])-1))
dim(testing) <- c(dim(testing), 1)

### We instantiate the CNN that we will use for image classification
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

### We train the instantiated model
history <- model %>%
  fit(x = training, y = train.y,
      epochs = 30,
      validation_data = list(testing, test.y))

### We use the model to predict classes on the test data
pred <- predict_classes(model, testing)
confusionMatrix(table(as.vector(pred), as.numeric(available_images$Pattern[-indxTrain])-1))

# --------------------------------------------------------------------------------------------------

### We prepare data augmentation generator
datagen <- image_data_generator(
  rotation_range = 180,
  horizontal_flip = TRUE,
  vertical_flip = TRUE,
  fill_mode = "nearest"
)

train_generator <- flow_images_from_data(
  x = training,
  y = train.y,
  generator = datagen,
  batch_size = 45
)

validation_generator <- flow_images_from_data(
  x = testing,
  y = test.y,
  generator = datagen,
  batch_size = 15
)

### We instantiate the CNN model
model %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu", 
                input_shape = c(301, 301, 1)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = ncol(train.y), activation = "softmax")
  
### We will use both accuracy and AUC as metrics for the training
model %>% compile(optimizer = "rmsprop", 
                  loss = "binary_crossentropy",  
                  metric=c("accuracy"))
                  
model %>% compile(optimizer = "rmsprop", 
                  loss = "binary_crossentropy",  
                  metric=c("AUC"))

### For each model, do the training and validation
history <- model %>%
  fit_generator(
    generator = train_generator,
    steps_per_epoch = 10,
    epochs = 40,
    validation_data = validation_generator,
    validation_steps = 10
  )

### We use the models to predict classes on the test data
pred <- predict_classes(model, testing)
confusionMatrix(table(as.vector(pred), as.numeric(available_images$Pattern[-indxTrain])-1))

### We print the system information
Sys.info()
