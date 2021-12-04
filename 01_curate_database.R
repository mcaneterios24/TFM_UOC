
### Manuel Cañete Ríos
### 04/12/2021
### This script will be used to filter out images with more than one cell

### Dependencies
library(dplyr)
library(stringr)
library(data.table)

### Input directory
input_dir <- "path/to/file"

# We get info of which images have more than one cell
# Images with more than one cell appear more than once in the Cell column
input_data <- read.delim(file.path(input_dir, "Patterns", "all_patterns.txt")) %>%
  mutate(Image = str_extract(Cell, ".*(?=_S001)"))

single_images <- input_data %>%
  group_by(Image) %>%
  summarise(Counts = n()) %>%
  filter(Counts == 1)

# From all the images of the database we just keep those with single cells
# We will collpase the different patterns into either Diffuse or Aggregate
available_images <- data.frame(Image = list.files(file.path(input_dir, "MIPs"))) %>%
  mutate(Image2 = str_extract(Image, ".*(?=_S001)")) %>%
  mutate(Image2 = str_replace(Image2, "-", "_")) %>%
  filter(Image2 %in% single_images$Image) %>%
  left_join(input_data, by = c("Image2" = "Image")) %>%
  mutate(Pattern = ifelse(str_detect(Pattern, "iffuse"), "Diffuse", "Aggregate")) %>%
  select(-Cell)

# We will also keep the intensity and area information of the filtered cells
intensities_and_area <- read.delim(file.path(input_dir, "Intensity and area", "area_intensities.txt")) %>%
  mutate(Image2 = str_extract(Cell, ".*(?=_S001)")) %>%
  inner_join(., available_images, by = "Image2")

# We write the dataframes into the corresponding .txt files
fwrite(available_images, "available_images.txt", row.names = F, sep = "\t")
fwrite(intensities_and_area, "intensities_and_area.txt", row.names = F, sep = "\t")

# We print the system information
Sys.info()
