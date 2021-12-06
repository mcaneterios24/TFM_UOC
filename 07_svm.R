
### Manuel Cañete Ríos
### 06/12/2021
### This script will be used to perform SVM classification

library(dplyr)
library(stringr)
library(ggplot2)
library(caret)
library(data.table)
library(doParallel)

### We turn on parallel computing
cl <- makePSOCKcluster(3)
registerDoParallel(cl)

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

### Declare cross-validation settings
set.seed(654321)
ctrl <- trainControl(method = "repeatedcv", # use repeated k-fold cross-validation
                     repeats = 5,  # 5-fold cross-validation
                     classProbs = T, 
                     summaryFunction = twoClassSummary, 
                     returnData = F, 
                     trim = T) 

### Generate SVM model using linear kernerl
SVMFit_linear <- train(Pattern ~ ., 
               data = training, 
               method = "svmLinear",
               trControl = ctrl,
               tuneGrid = expand.grid(C = seq(0, 2, length = 20)),
               metric = "ROC")

### Generate SVM model using radial kernel (default settings)
SVMFit_radial <- train(Pattern ~ ., 
                data = training, 
                method = "svmRadial",
                trControl = ctrl,
                tuneLength = 15,
                metric = "ROC")

### Generate SVM model using radial kernel (user-defined grid)
SVMFit_radial2 <- train(Pattern ~ ., 
                       data = training, 
                       method = "svmRadial",
                       trControl = ctrl,
                       tuneGrid = expand.grid(sigma= 2^c(-10.05, -10.075, -10.1, -10.15, -10.2),
                                              C= 2^c(0:10)),
                       metric = "ROC")

### We print the models info
SVMFit_linear
SVMFit_radial
SVMFit_radial2

### We plot the cross-validation results
# SVM linear
plot(SVMFit_linear)

svmlinear %>% # This svmlinear object has been created in an Excel file using the output of printing RFFit into R console
  pivot_longer(-C, names_to = "Metric", values_to = "Value") %>%
  na.omit() %>%
  ggplot(mapping = aes(x = -log(C), y = Value, group = Metric, col = Metric)) +
  geom_point(show.legend = F) +
  geom_line(show.legend = F) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  facet_wrap(~Metric, scales = "free") +
  scale_color_manual(values = c("#003049", "#D62828", "#FCBF49"))

ggsave("CV_svmlinear.pdf", plot = last_plot(), width = 300, height = 100, units = "mm")

# SVM radial
plot(SVMFit_radial)

rbf1 %>% # This rbf1 object has been created in an Excel file using the output of printing RFFit into R console
  pivot_longer(-C, names_to = "Metric", values_to = "Value") %>%
  ggplot(mapping = aes(x = log2(C), y = Value, group = Metric, col = Metric)) +
  geom_point(show.legend = F) +
  geom_line(show.legend = F) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  facet_wrap(~Metric, scales = "free") +
  scale_color_manual(values = c("#003049", "#D62828", "#FCBF49"))

ggsave("CV_svmrbf1.pdf", plot = last_plot(), width = 300, height = 100, units = "mm")

# SVM radial 2
plot(SVMFit_radial2)

rbf2 %>% # This rbf2 object has been created in an Excel file using the output of printing RFFit into R console
  pivot_longer(-c("sigma", "Sigma", "C"), names_to = "Metric", values_to = "Value") %>%
  na.omit() %>%
  ggplot(mapping = aes(x = log2(C), y = Value, group = Sigma, col = Sigma)) +
  geom_point(show.legend = T) +
  geom_line(show.legend = F) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  facet_wrap(~Metric, scales = "free")

ggsave("CV_svmrbf2_legend.pdf", plot = last_plot(), width = 300, height = 100, units = "mm")

### We use these models to predict classes on the test data
# SVM linear
pred <- predict(SVMFit_linear, testing)
confusionMatrix(pred, testing$Pattern)

# SVM radial
pred2 <- predict(SVMFit_radial, testing)
confusionMatrix(pred2, testing$Pattern)  

# SVM radial 2
pred3 <- predict(SVMFit_radial2, testing)
confusionMatrix(pred3, testing$Pattern) 

### We stop parallel computing
stopCluster(cl)

### We print the system information
Sys.info()
