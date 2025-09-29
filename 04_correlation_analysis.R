# 04_correlation_analysis.R

# Load libraries
library(tidyverse)
library(sf)
library(tmap)
library(ggplot2)

# Load config and processed data
source("00_config.R")
imd_lsoa <- readRDS(file.path(data_processed, "imd_lsoa.rds"))

#  1. Statistical Correlation 
# Check the correlation between the overall GMAL accessibility score
# and the Income Deprivation score.

# Invert the gmal_score so that higher values mean *poorer* access.
# This makes the expected correlation positive (high deprivation ~ high poor access).
imd_lsoa <- imd_lsoa %>%
  mutate(inverse_gmal = max(gmal_score, na.rm = TRUE) - gmal_score)

# Perform Pearson correlation test
correlation_test <- cor.test(~ imd_score + inverse_gmal, data = imd_lsoa)
print(correlation_test) 

#  2. Visual Correlation (Scatter Plot) 
scatter_plot <- ggplot(imd_lsoa, aes(x = inverse_gmal, y = imd_score)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", col = "firebrick") +
  labs(
    title = "Correlation between Poor Accessibility and Income Deprivation",
    x = "Poor Public Transport Accessibility (Inverted GMAL Score)",
    y = "IMD Score (IMD)"
  ) +
  theme_minimal()

print(scatter_plot)
ggsave(file.path(maps_out, "correlation_scatter.png"))

#  3. Bivariate Choropleth Map 

# Classify both variables into quantiles 
imd_lsoa <- imd_lsoa %>%
  mutate(
    dep_class = ntile(imd_score, 3),
    acc_class = ntile(inverse_gmal, 3),
    # 1-1 is Low Dep, Low Poor Access (Good-Good)
    # 3-3 is High Dep, High Poor Access (Bad-Bad)
    bivar_class = paste0(dep_class, "-", acc_class)
  )

# Define colours for the map legend
bivar_colors <- c(
  "1-1" = "#e8e8e8", # Low Dep, Low Poor Access
  "1-2" = "#b0d5df",
  "1-3" = "#64acbe", # Low Dep, High Poor Access
  "2-1" = "#e4acac",
  "2-2" = "#ad9ea5", # Both Medium
  "2-3" = "#627f8c",
  "3-1" = "#c85a5a", # High Dep, Low Poor Access
  "3-2" = "#985356",
  "3-3" = "#574249"  # High Dep, High Poor Access (CRITICAL AREAS)
)

bivariate_map <- tm_shape(imd_lsoa) +
  tm_polygons("bivar_class", palette = bivar_colors, title = "Deprivation & Accessibility") +
  tm_layout(
    main.title = "Bivariate Map of Deprivation and Poor Accessibility",
    legend.show = FALSE 
  ) +
  tm_borders(alpha = 0.1)

tmap_save(bivariate_map, file.path(maps_out, "bivariate_map.png"))
message("✅ Correlation analysis and maps complete.")

#  4. Create a Custom Legend for the Bivariate Map 
# A standard tmap legend isn't suitable, so we must create a custom one with ggplot2.

# Create a data frame for the 3x3 legend grid
legend_data <- expand.grid(dep = 1:3, acc = 1:3) %>%
  mutate(bivar_class = paste0(dep, "-", acc))

# Create the legend plot object
bivariate_legend <- ggplot(legend_data, aes(x = acc, y = dep, fill = bivar_class)) +
  geom_tile(linewidth = 0.5, color = "white") +
  scale_fill_manual(values = bivar_colors) +
  labs(
    x = "Poor Accessibility →",
    y = "Deprivation →"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none" 
  ) +
  coord_fixed()

# Save the legend as a separate image file
ggsave(
  file.path(maps_out, "bivariate_legend.png"),
  plot = bivariate_legend,
  width = 2.5,
  height = 2.5,
  units = "in"
)

message("✅ Bivariate map and its custom legend have been saved as separate files.")
