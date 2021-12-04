
### Manuel Cañete Ríos
### 04/12/2021
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
input_dir <- "path/to/files"

### We import the input data (only informative pixels and dimensions reduced using PCA)
arranged_data <- fread(file.path(input_dir, "pca_data_1024.txt")) %>%
  mutate(Pattern = factor(Pattern))

### We generate the training and test datasets (3:1, respectively)
set.seed(123456)
indxTrain <- createDataPartition(y = arranged_data$Pattern, p = 0.75, list = FALSE)
training <- arranged_data[indxTrain,]
testing <- arranged_data[-indxTrain,]

### Declare cross-validation settings
set.seed(654321)
ctrl <- trainControl(method = "repeatedcv", # use repeated k-fold cross-validation
                     repeats = 5, # 5-fold cross-validation
                     classProbs = T, 
                     summaryFunction = twoClassSummary, 
                     returnData = F, 
                     trim = T) 

### Generate CART model
treeFit1 <- train(Pattern ~ ., 
                 data = training, 
                 method = "rpart", 
                 trControl = ctrl,
                 tuneLength = 20, # test 20 different values of Cp
                 metric = "ROC")

### Generate C5.0 model
treeFit2 <- train(Pattern ~ ., 
                data = training, 
                method = "C5.0", 
                trControl = ctrl,
                tuneLength = 20,
                metric = "ROC")

### We print DT models info
treeFit
treeFit2

### We plot the cross-validation results
# CART
plot(treeFit)

cart %>% # This cart object has been created in an Excel file using the output of printing treeFit into R console
  pivot_longer(-cp, names_to = "Metric", values_to = "Value") %>%
  ggplot(mapping = aes(x = cp, y = Value, group = Metric, col = Metric)) +
  geom_point(show.legend = F) +
  geom_line(show.legend = F) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  facet_wrap(~Metric, scales = "free") +
  scale_color_manual(values = c("#003049", "#D62828", "#FCBF49")) +
  scale_x_continuous(breaks = round(seq(from = 0, to = 0.3, length.out = 8),2))

ggsave("CV_cart.pdf", plot = last_plot(), width = 300, height = 100, units = "mm")


#C5.0
plot(treeFit2)

c5 %>% # This c5 object has been created in an Excel file using the output of printing treeFit2 into R console
  pivot_longer(-c("trials", "model", "winnow"), names_to = "Metric", values_to = "Value") %>%
  ggplot(mapping = aes(x = trials, y = Value, group = Metric, col = Metric)) +
  geom_point(show.legend = F) +
  geom_line(show.legend = F) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  facet_wrap(~Metric+model+winnow, scales = "free") +
  scale_color_manual(values = c("#003049", "#D62828", "#FCBF49"))

ggsave("CV_c5.pdf", plot = last_plot(), width = 300, height = 250, units = "mm")


### We use these models to predict classes on the test data
# CART
pred <- predict(treeFit, testing)
confusionMatrix(pred, testing$Pattern)

# C5.0
pred2 <- predict(treeFit2, testing)
confusionMatrix(pred2, testing$Pattern)

### We stop parallel computing
stopCluster(cl)

### We print the system information
Sys.info()
