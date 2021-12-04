
### Manuel Cañete Ríos
### 07/11/2021
### This script will be used to filter out images with more than one cell
### And also to select less images to reduce computational load

### Dependencies
library(dplyr)
library(stringr)
library(data.table)

### Input directory
input_dir <- "C:/Users/manue/OneDrive/Escritorio/Master/TFM/Imatges/Input data"


# We get info of which images have more than one cell
input_data <- read.delim(file.path(input_dir, "Patterns", "all_patterns.txt")) %>%
  mutate(Image = str_extract(Cell, ".*(?=_S001)"))

single_images <- input_data %>%
  group_by(Image) %>%
  summarise(Counts = n()) %>%
  filter(Counts == 1)


# All available images
available_images <- data.frame(Image = list.files(file.path(input_dir, "MIPs"))) %>%
  mutate(Image2 = str_extract(Image, ".*(?=_S001)")) %>%
  mutate(Image2 = str_replace(Image2, "-", "_")) %>%
  filter(Image2 %in% single_images$Image) %>%
  left_join(input_data, by = c("Image2" = "Image")) %>%
  mutate(Pattern = ifelse(str_detect(Pattern, "iffuse"), "Diffuse", "Aggregate")) %>%
  select(-Cell)

intensities_and_area <- read.delim(file.path(input_dir, "Intensity and area", "area_intensities.txt")) %>%
  mutate(Image2 = str_extract(Cell, ".*(?=_S001)")) %>%
  inner_join(., available_images, by = "Image2")

fwrite(available_images, "available_images.txt", row.names = F, sep = "\t")
fwrite(intensities_and_area, "intensities_and_area.txt", row.names = F, sep = "\t")

Sys.info()
