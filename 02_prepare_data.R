
### Manuel Cañete Ríos
### 07/11/2021
### This script will be used to reduce image resolution and also
### to remove non-informative pixels

### Dependencies
library(dplyr)
library(stringr)
library(ggplot2)
library(OpenImageR)
library(data.table)

### Input directory
input_dir <- "C:/Users/manue/OneDrive/Escritorio/Master/TFM/Imatges/Input data"

### Source script to import the available_images object
source('C:/Users/manue/OneDrive/Escritorio/Master/TFM/Analysis/01_curate_database.R')

### Rescale images and append pattern information
rescaled_data <- sapply(available_images$Image, function(x){
  filename <- str_replace(x, ".tiff", ".txt")
  vect <- scan(file.path(input_dir, "Pixels", filename), quiet = T)
  mat <- matrix(vect, nrow = 1024)
  resized <- resizeImage(mat, width = 1024, height = 1024, method = "nearest") # We had to scale due to memory problems with the PCA
  as.vector(resized)
}) %>%
  t() %>%
  as.data.frame() %>%
  mutate(Pattern = available_images$Pattern) %>%
  select(Pattern, everything())

### Save rescaled data
fwrite(rescaled_data, "rescaled_images_400.txt", sep = "\t", row.names = F)

### Generate two average images
input_diffuse <- rescaled_data %>%
  filter(Pattern == "Diffuse") %>%
  select(-Pattern)

input_aggregate <- rescaled_data %>%
  filter(Pattern == "Aggregate") %>%
  select(-Pattern)

rm(rescaled_data)

diffuse_average <- matrix(apply(input_diffuse, 2, median), nrow = 400)
aggregate_average <- matrix(apply(input_aggregate, 2, median), nrow = 350)

imageShow(diffuse_average)
imageShow(aggregate_average)

### Select which pixels to keep
pixels_to_keep <- which(aggregate_average > 0.02)
write.csv(pixels_to_keep, "pixels_to_keep.csv", row.names = F)