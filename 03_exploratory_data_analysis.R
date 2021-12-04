
### Manuel Cañete Ríos
### 07/11/2021
### This script will be used to perform EDA on the database

### Dependencies
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

### Input directory
input_dir <- "C:/Users/manue/OneDrive/Escritorio/Master/TFM/Imatges/Input data"

### Source script to import objects
source('C:/Users/manue/OneDrive/Escritorio/Master/TFM/Analysis/01_curate_database.R')
source('C:/Users/manue/OneDrive/Escritorio/Master/TFM/Analysis/02_prepare_data.R')

### Boxplots of cell area and integrated density
intensities_and_area %>%
  pivot_longer(-c("Cell", "Image", "Image2", "Pattern", "Image3"), names_to = "Variable", values_to = "Value") %>%
  filter(Variable %in% c("Area", "Integrated_density")) %>%
  ggplot(mapping = aes(x = Pattern, y = log10(Value), fill = Pattern)) +
  geom_boxplot(show.legend = F) +
  scale_fill_manual(values = c("#5B84B1FF", "#FC766AFF")) +
  theme_bw() +
  xlab(NULL) +
  theme(text = element_text(size = 12)) +
  facet_wrap(~Variable, ncol = 2)

### Mean estimates of cell area and integrated density
intensities_and_area %>%
  group_by(Pattern, Variable) %>%
  summarise(Mean = mean(log10(Value)))

### Statistical comparison
var.test(log10(Area) ~ Pattern, intensities_and_area)
var.test(log10(Integrated_density) ~ Pattern, intensities_and_area)
t.test(log10(Area) ~ Pattern, intensities_and_area, var.equal = F)
t.test(log10(Integrated_density) ~ Pattern, intensities_and_area, var.equal = T)

### Perform PCAs of the final dataset
pca.res <- prcomp(arranged_data, center = F)

head(100*pca.res$sdev/sum(pca.res$sdev))

pca.df <- data.frame(Pattern = rescaled_data$Pattern, 
                     PC1 = pca.res$x[,1],
                     PC2 = pca.res$x[,2],
                     PC3 = pca.res$x[,3]) %>%
  pivot_longer(-c("Pattern", "PC1"), names_to = "Var", values_to = "PCX")

pca.df %>%
  ggplot(mapping = aes(x = PC1, y = PCX, col = Pattern)) +
  geom_point() +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values = c("#5B84B1FF", "#FC766AFF")) +
  facet_wrap(~Var, ncol = 2)

