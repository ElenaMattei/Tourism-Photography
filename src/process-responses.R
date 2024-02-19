library(stringr)
library(tidyverse) 
library(dplyr)
library(reshape2)
library(FactoMineR)
library(factoextra)
library(devtools)
library(readxl)

# Here you should provide the path to your data
setwd("~/Desktop/StatsBremen/Elena-Stats/Data") 
responses <- read_excel("Tag_choices_EM.xlsx")

responses <- responses %>%
  mutate(across(`Tourism Photography`:`Moral Inhibition`, ~ ifelse(.x == "yes", 1, 0)))

used_features <- colnames(responses)[2:length(colnames(responses))]

library(readr)
metadata <- read_delim("Metadata_ch_ag_im_counts/Metadata.csv",
 delim = ";", escape_double = FALSE, trim_ws = TRUE)

metadata <- metadata %>% select(-...4, -...5)

ggplot(metadata, aes(x=factor(1), fill=Channel)) +
  geom_bar(width = 1)+
  coord_polar("y")

xtabs(~ Channel + Agency, data=metadata)

responses$`Image id` <- metadata$`Image id`
full.data <- merge(responses, metadata, by="Image id")

full.data <- full.data %>% select(-image)
rownames(full.data) <- full.data$`Image id`
full.data <- full.data %>% select(-`Image id` )

full.data.rest <- full.data %>% select(-Agency, -Channel)

data.pca <- PCA(full.data.rest, ncp=10, scale.unit = FALSE)
data.pca <- prcomp(full.data.rest, scale = FALSE)

fviz_pca_ind(data.pca, geom.ind = "point", 
             col.ind = full.data$Channel,
             palette = rainbow(10),
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Channel (95% CI)"
)

fviz_pca_ind(data.pca, geom.ind = "point", col.ind = full.data$Channel,
             axes = c(5,6),
             palette = rainbow(10),
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Channel (95% CI)"
)

show_contributions(data.pca, cutoff=0.2)
show_contributions(data.pca, dim=2, cutoff=0.08)

show_contributions(data.pca, dim=3)
show_contributions(data.pca, dim=4)

screeplot(data.pca)
fviz_contrib(data.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(data.pca, choice = "var", axes = 2, top = 100)
fviz_eig(data.pca)

fviz_eig(data.pca, addlabels = TRUE, ylim = c(0, 50))
coords.df <- as.data.frame( get_pca_var(data.pca)$coord )

coords_for_dim2 <-
  coords.df[order(-coords.df$Dim.2),] %>% select(Dim.2)
main_coords_for_dim2 <-
  coords.df[order(-coords.df$Dim.2),] %>% select(Dim.2) %>%
  filter(Dim.2 > 0.1 | Dim.2 < -0.1)

coords_for_dim1 <-
  coords.df[order(-coords.df$Dim.1),] %>% select(Dim.1)
main_coords_for_dim1 <-
  coords.df[order(-coords.df$Dim.1),] %>% select(Dim.1) %>%
  filter(Dim.1 > 0.1 | Dim.1 < -0.1)

get_eigenvalue(data.pca)

fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

