
### Manuel Cañete Ríos
### 06/12/2021
### This script will be used to perform ANN classification

library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(keras)
library(caret)
library(tensorflow)
library(reticulate)

# Connect to pyhton tensorflow and keras
reticulate::use_python("path/to/miniconda/R-MINI~1/envs/tf_image", required = T)

### Input directory
input_dir <- "path/to/files"

### We keep the pixels of interest
arranged_data <- fread(file.path(input_dir, "pca_data_1024.txt")) %>%
  mutate(Pattern = factor(Pattern))

### We generate the training and test datasets
set.seed(123456)
indxTrain <- createDataPartition(y = arranged_data$Pattern, p = 0.75, list = FALSE)
training <- arranged_data[indxTrain,]
testing <- arranged_data[-indxTrain,]

### We arrange the data for using it on keras
# Training data
train.x <- as.matrix(sapply(training[,2:785], scale), nrow = 589)
train.y <- to_categorical(matrix(as.numeric(training$Pattern)-1))

# Testing data
test.x <- as.matrix(sapply(testing[,2:785], scale), nrow = 195)
test.y <- to_categorical(matrix(as.numeric(testing$Pattern)-1))

### We instantiate the ANN networks we will use (either using activation = "sigmoid" / "linear" / "relu")
model <- keras_model_sequential()

model %>%
  layer_dense(units = 64, activation = "sigmoid", input_shape = ncol(train.x)) %>%
  layer_dense(units = 64, activation = "sigmoid") %>%
  layer_dense(units = ncol(train.y), activation = "softmax")

model %>% compile(optimizer = "rmsprop", 
                  loss = "categorical_crossentropy",  
                  metric=c("AUC"))

print(model)

### We train the model
history <- fit(model, train.x, train.y, 
               epochs = 15, 
               batch_size = 50, 
               validation_data = list(test.x, test.y))

plot(history)

### We plot the history of all three models
ann %>% # This rbf2 object has been created in an Excel file using the output of printing history$metrics into R console
  pivot_longer(-c("epoch", "activation"), names_to = "Set", values_to = "AUC") %>%
  na.omit() %>%
  ggplot(mapping = aes(x = epoch, y = AUC, group = Set, col = Set)) +
  geom_point(show.legend = F) +
  geom_line(show.legend = F) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  facet_wrap(~activation, scales = "free") +
  scale_color_manual(values = c("#003049", "#D62828", "#FCBF49"))

ggsave("epochs_ann.pdf", plot = last_plot(), width = 300, height = 100, units = "mm")

### We use the model to predict classes on the test data
pred <- predict_classes(model, test.x)
confusionMatrix(table(as.vector(pred2), as.numeric(testing$Pattern)-1))

### We print the system information
Sys.info()
