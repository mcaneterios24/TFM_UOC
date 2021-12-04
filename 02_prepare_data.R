
### Manuel Cañete Ríos
### 04/12/2021
### This script will be used to remove non-informative pixels 

### Dependencies
library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(OpenImageR)

### Input directory
input_dir <- "path/to/files"

### Import the selected images from 01_curate_database.R
available_images <- read.delim(file.path(input_dir, "Inputs", "available_images.txt")) %>%
  mutate(Image3 = str_replace(Image, "&", "_"))

### Get pixel values for each image in the available_images object and append to this dataframe the pattern information
pixel_data <- sapply(available_images$Image3, function(x){
  filename <- str_replace(x, ".tiff", ".txt")
  pixels <- as.matrix(fread(file.path(input_dir, "Pixels", filename), header = FALSE))
  c(pixels)
}) %>%
  t() %>%
  as.data.frame() %>%
  mutate(Pattern = available_images$Pattern) %>%
  select(Pattern, everything())

### Save this dataframe into a .txt as it takes long to produce this object (1024x1024 pixels)
fwrite(pixel_data, "pixel_data_1024.txt", sep = "\t", row.names = F)

### Now we want to select those pixels that will be important for classification
### To do so, we will generate median images and, from them, select the important pixels

# First we generate the input objects, selecting the cells of each kind
input_diffuse <- rescaled_data %>%
  filter(Pattern == "Diffuse") %>%
  select(-Pattern)

input_aggregate <- rescaled_data %>%
  filter(Pattern == "Aggregate") %>%
  select(-Pattern)

# Now we calculate the median of each pixel for all images
diffuse_average <- matrix(apply(input_diffuse, 2, median), nrow = 1024)
aggregate_average <- matrix(apply(input_aggregate, 2, median), nrow = 1024)

# Now we save both median images to take them into Fiji ImageJ:
fwrite(diffuse_average, file.path(input_dir, "diffuse_median_image.txt"), sep = "\t", row.names = F, col.names = F)
fwrite(aggregate_average, file.path(input_dir, "aggregate_median_image.txt"), sep = "\t", row.names = F, col.names = F)

# If we want to display them in R:
imageShow(diffuse_average)
imageShow(aggregate_average)

# We select which pixels to keep
pixels_to_keep <- which(aggregate_average > 0.02)

# We write the index of each selected pixel into a file
write.csv(pixels_to_keep, "pixels_to_keep.csv", row.names = F)

### Finally we print the system information
Sys.info()
