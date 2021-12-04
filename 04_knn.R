### Manuel Cañete Ríos
### 04/12/2021
### This script will be used to perform kNN image classification

### Dependencies
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(caret)
library(data.table)
library(doParallel)

### We turn on parallel computing
cl <- makePSOCKcluster(12)
registerDoParallel(cl)

### Input directory
input_dir <- "path/to/files"

### We import the input data (only informative pixels and dimensions reduced using PCA)
arranged_data <- fread(file.path(input_dir, "pca_data_1024.txt")) %>%
  mutate(Pattern = factor(Pattern))

### We generate the training and test datasets (3:1, respectively)
set.seed(123456)
indxTrain <- createDataPartition(y = arranged_data$Pattern, p = 0.75, list = FALSE)
training <- arranged_data[indxTrain,]
testing <- arranged_data[-indxTrain,]

### Generate kNN model
set.seed(654321)
ctrl <- trainControl(method = "repeatedcv", # use repeated k-fold cross-validation
                     repeats = 5, # 5-fold cross-validation
                     classProbs = T, 
                     summaryFunction = twoClassSummary, 
                     returnData = FALSE, 
                     trim = T) 
                                                                                                          
knnFit <- train(Pattern ~ ., 
                data = training, 
                method = "knn", 
                trControl = ctrl, 
                tuneLength = 20, # test 20 different values of k
                metric = "ROC") # base model selection on AUROC instead of accuracy

### We print KNN model info
knnFit

### We plot the cross-validation results
plot(knnFit)

knn %>% # This knn object has been created in an Excel file using the output of printing KnnFit into R console
  pivot_longer(-k, names_to = "Metric", values_to = "Value") %>%
  ggplot(mapping = aes(x = k, y = Value, group = Metric, col = Metric)) +
  geom_point(show.legend = F) +
  geom_line(show.legend = F) +
  geom_vline(xintercept = 11, lty = "dashed", col = "grey60") +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  facet_wrap(~Metric, scales = "free") +
  scale_color_manual(values = c("#003049", "#D62828", "#FCBF49"))

ggsave("CV_knn.pdf", plot = last_plot(), width = 300, height = 100, units = "mm")

### We use this model to predict classes on the test data
pred <- predict(knnFit, testing)
confusionMatrix(pred, testing$Pattern)

### We stop parallel computing
stopCluster(cl)

### We print system info
Sys.info()
