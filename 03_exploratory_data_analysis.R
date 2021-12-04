
### Manuel Cañete Ríos
### 04/12/2021
### This script will be used to perform EDA on the curated database

### Dependencies
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(data.table)

### Input directory
input_dir <- "path/to/files"

### Import objects from 01_curated_database.R and 02_prepare_data.R
available_images <- read.delim(file.path(input_dir, "Inputs", "available_images.txt"))
intensities_and_area <- read.delim(file.path(input_dir, "Inputs", "intensities_and_area.txt"))
pixels_to_keep <- read.csv(file.path(input_dir, "Inputs", "pixels_to_keep.csv"))
pixel_data <- fread(file.path(input_dir, "Inputs", "pixel_data_1024.txt"))

### Histograms of cell area and mean intensity

# Log10 values
intensities_and_area %>%
  pivot_longer(-c("Cell", "Image", "Image2", "Image3", "Pattern"), names_to = "Variable", values_to = "Value") %>%
  filter(Variable %in% c("Area", "Mean_intensity")) %>%
  group_by(Variable, Pattern) %>%
  ggplot(mapping = aes(x = log10(Value), fill = Pattern)) +
  geom_histogram(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), bins = 100, show.legend = F) +
  scale_fill_manual(values = c("#5B84B1FF", "#FC766AFF")) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  ylab("Frequency") +
  facet_wrap(~Variable+Pattern, ncol = 4, scales = "free")

ggsave("histograms_log.pdf", plot = last_plot(), width = 300, height = 80, units = "mm")

# Untransformed values
intensities_and_area %>%
  pivot_longer(-c("Cell", "Image2", "Image", "Pattern", "Image3"), names_to = "Variable", values_to = "Value") %>%
  filter(Variable %in% c("Area", "Mean_intensity")) %>%
  group_by(Variable, Pattern) %>%
  ggplot(mapping = aes(x = Value, fill = Pattern)) +
  geom_histogram(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), bins = 15, show.legend = F) +
  scale_fill_manual(values = c("#5B84B1FF", "#FC766AFF")) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  ylab("Frequency") +
  facet_wrap(~Variable+Pattern, ncol = 4, scales = "free")

ggsave("histograms_raw.pdf", plot = last_plot(), width = 300, height = 80, units = "mm")


### Mean estimates of cell area and mean intensity (log10-transformed)
intensities_and_area %>%
  pivot_longer(-c("Cell", "Image", "Image2", "Image3", "Pattern"), names_to = "Variable", values_to = "Value") %>%
  filter(Variable %in% c("Area", "Mean_intensity")) %>%
  group_by(Pattern, Variable) %>%
  summarise(Mean = mean(log10(Value)))

### Statistical comparison of log10-transformed values
# Mean intensity
shapiro.test(log10(intensities_and_area$Mean_intensity[intensities_and_area$Pattern == "Diffuse"]))
shapiro.test(log10(intensities_and_area$Mean_intensity[intensities_and_area$Pattern == "Aggregate"]))
var.test(log10(Mean_intensity) ~ Pattern, intensities_and_area)
t.test(log10(Mean_intensity) ~ Pattern, intensities_and_area, paired = F, var.equal = T)

# Cell area
wilcox.test(log10(Area) ~ Pattern, intensities_and_area, paired = F)
t.test(log10(Area) ~ Pattern, intensities_and_area, paired = F, var.equal = T)

### Perform PCAs of the final dataset

# We keep the pixels we're interested in
selected_cols <- colnames(pixel_data)[pixels_to_keep$x]
arranged_data <- pixel_data %>%
  select(all_of(selected_cols))

# We perform a PCA, without centering the data
pca.res <- fast.prcomp(arranged_data, center = F)

# To get the % explained variance of each component
100*pca.res$sdev^2/sum(pca.res$sdev^2)

# We generate a dataframe with the first three components and the corresponding Pattern of each cell
pca.df <- data.frame(Pattern = pixel_data$Pattern, 
                     PC1 = pca.res$x[,1],
                     PC2 = pca.res$x[,2],
                     PC3 = pca.res$x[,3]) %>%
  pivot_longer(-c("Pattern", "PC1"), names_to = "Var", values_to = "PCX")

# We generate the PCA plots using PC1 and PC2/PC3
pca.df %>%
  ggplot(mapping = aes(x = PC1, y = PCX, col = Pattern)) +
  geom_point() +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("#5B84B1FF", "#FC766AFF")) +
  facet_wrap(~Var, ncol = 2)

ggsave("PCA_EDA.pdf", plot = last_plot(), width = 300, height = 200, units = "mm")

### We print the system information
Sys.info()
