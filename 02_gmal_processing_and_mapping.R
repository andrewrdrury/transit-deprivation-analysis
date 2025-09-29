# 02_gmal_processing_and_mapping.R
# Part 1: Inspect raw GMAL scores before aggregation

# Load libraries
library(sf)
library(tidyverse)
library(tmap)
library(dplyr)
library(units)

# Set tmap to plot mode for clean, static images
tmap_mode("plot")

# Load config (paths and CRS)
source("00_config.R")

# Load spatial data
gmal_sf <- readRDS(file.path(data_processed, "gmal_sf.rds"))
imd_lsoa <- readRDS(file.path(data_processed, "imd_lsoa.rds"))

# Preview structure
glimpse(gmal_sf)
summary(gmal_sf)
names(gmal_sf)
glimpse(imd_lsoa)
summary(imd_lsoa)
names(imd_lsoa)

# Quick check: CRS match
if (!st_crs(gmal_sf) == st_crs(imd_lsoa)) {
  gmal_sf <- st_transform(gmal_sf, st_crs(imd_lsoa))
}

# All Map Blocks
# Each map is a separate block and will render independently.

# Map 1: GMAL Level
tm_shape(gmal_sf) +
  tm_fill(col = "gmal_level", palette = "viridis", style = "cat", title = "GMAL Level") +
  tm_layout(main.title = "Greater Manchester Public Transport Access Levels", legend.outside = TRUE) +
  tm_grid(lines = FALSE)

# Map 2: Bus Scores
tm_shape(gmal_sf) +
  tm_fill(col = "bus_score", palette = "viridis", style = "cont", title = "Bus Accessibility Score") +
  tm_layout(main.title = "Raw Bus Accessibility Scores", legend.outside = TRUE) +
  tm_grid(lines = FALSE)

# Map 3: Rail Scores
tm_shape(gmal_sf) +
  tm_fill(col = "rail_score", palette = "viridis", style = "cont", title = "Rail Accessibility Score") +
  tm_layout(main.title = "Raw Rail Accessibility Scores", legend.outside = TRUE) +
  tm_grid(lines = FALSE)

# Map 4: Metro Scores
tm_shape(gmal_sf) +
  tm_fill(col = "metro_score", palette = "viridis", style = "cont", title = "Metrolink Accessibility Score") +
  tm_layout(main.title = "Raw MetroLink Accessibility Scores", legend.outside = TRUE) +
  tm_grid(lines = FALSE)

# Map 5: Local Link Scores
tm_shape(gmal_sf) +
  tm_fill(col = "l_link_score", palette = "viridis", style = "cont", title = "Local Link Accessibility Score") +
  tm_layout(main.title = "Local Link Accessibility Scores", legend.outside = TRUE) +
  tm_grid(lines = FALSE)

# Calculation of Transit Demand and Scores
imd_lsoa <- imd_lsoa %>%
  mutate(
    # Demographics
    area_km2     = shape_area / 1e6,
    pop_density  = tot_pop / area_km2,
    pct_children = dep_chi / tot_pop,
    pct_elderly  = pop60 / tot_pop,
    dep_ratio    = (dep_chi + pop60) / tot_pop,
    
    # Standardise demographic demand
    z_density    = as.numeric(scale(pop_density)),
    z_children   = as.numeric(scale(pct_children)),
    z_elderly    = as.numeric(scale(pct_elderly)),
    z_dep_ratio  = as.numeric(scale(dep_ratio)),
    
    # Standardise IMD scores (higher = more deprived = more demand)
    z_income     = as.numeric(scale(inc_score)),
    z_employment = as.numeric(scale(emp_score)),
    z_bhs        = as.numeric(scale(bhs_score)),
    z_hdd        = as.numeric(scale(hdd_score)),
    
    # Final demand index (equal weights, 7 components)
    demand_index = (
      z_density + z_children + z_elderly + z_dep_ratio +
        z_income + z_employment + z_hdd
    ) / 7
  )

# Intersect the two layers to get weighted GMAL scores
gmal_lsoa_intersect <- st_intersection(gmal_sf, imd_lsoa)
gmal_lsoa_intersect$int_area <- st_area(gmal_lsoa_intersect)
grid_area <- 250 * 250
gmal_lsoa_intersect$weight <- as.numeric(gmal_lsoa_intersect$int_area) / grid_area
gmal_lsoa_intersect$gmal_score <- as.numeric(gmal_lsoa_intersect$gmal_score)

gmal_scores_lsoa <- gmal_lsoa_intersect %>%
  group_by(lsoa11cd) %>%
  summarise(gmal_score = weighted.mean(gmal_score, weight, na.rm = TRUE)) %>%
  ungroup()

gmal_scores_lsoa_df <- st_drop_geometry(gmal_scores_lsoa)
imd_lsoa <- left_join(imd_lsoa, gmal_scores_lsoa_df, by = "lsoa11cd")

# Calculation of Transit Desert Scores
imd_lsoa$z_demand <- as.numeric(scale(imd_lsoa$demand_index))
imd_lsoa$z_access <- as.numeric(scale(imd_lsoa$gmal_score))

# Overall Transit Desert Score
imd_lsoa$transit_desert <- as.numeric(imd_lsoa$z_demand - imd_lsoa$z_access)
imd_lsoa$transit_desert_clipped <- ifelse(imd_lsoa$transit_desert > 0, imd_lsoa$transit_desert, NA)

# BHS-specific Transit Desert Score
imd_lsoa$transit_desert_bhs <- as.numeric(imd_lsoa$z_bhs - imd_lsoa$z_access)
imd_lsoa$transit_desert_bhs_clipped <- ifelse(imd_lsoa$transit_desert_bhs > 0, imd_lsoa$transit_desert_bhs, NA)


# Final Maps

# LSOA-Level GMAL Score (Area-Weighted)
tm_shape(imd_lsoa) +
  tm_fill(col = "gmal_score", palette = "RdYlGn", style = "quantile", title = "Weighted GMAL Score") +
  tm_borders(col = "white", lwd = 0.3) +
  tm_layout(main.title = "LSOA-Level GMAL Score (Area-Weighted)") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))

# Overall Transit Demand Index
tm_shape(imd_lsoa) +
  tm_borders(col = "white", lwd = 0.2) +
  tm_fill(
    col = "demand_index",
    palette = "Reds",
    style = "quantile",
    title = "Transit Demand Index"
  ) +
  tm_layout(
    main.title = "Spatial Distribution of Transit Demand (LSOA)",
    legend.outside = TRUE
  ) +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))

# Overall Transit Desert Score (Clipped)
tm_shape(imd_lsoa) +
  tm_fill(col = "transit_desert_clipped", palette = "Reds", style = "quantile", title = "Transit Desert Score (Clipped)") +
  tm_borders(col = "white", lwd = 0.2) +
  tm_layout(main.title = "Spatial Distribution of Transit Deserts (LSOA)", legend.outside = TRUE) +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))

# BHS-specific Transit Desert Score (Clipped)
tm_shape(imd_lsoa) +
  tm_fill(col = "transit_desert_bhs_clipped", palette = "Reds", style = "quantile", title = "BHS Transit Desert Score (Clipped)") +
  tm_borders(col = "white", lwd = 0.2) +
  tm_layout(main.title = "Spatial Distribution of BHS-Specific Transit Deserts", legend.outside = TRUE) +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))


# Save the processed data
saveRDS(imd_lsoa, file.path(data_processed, "imd_lsoa.rds"))
