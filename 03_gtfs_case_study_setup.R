# 03_gtfs_case_study_setup.R

# Load packages
library(tidyverse)
library(sf)
library(tidytransit)
library(tmap)
source("00_config.R")

# Load spatial and GTFS data
imd_lsoa <- readRDS(file.path(data_processed, "imd_lsoa.rds"))
gtfs_stops <- readRDS(file.path(data_processed, "gtfs_stops_sf.rds"))
gtfs_obj <- read_gtfs(gtfs_path)

#  Clean up old case study folders 
# This ensures that buffers from previous runs are removed.
if (dir.exists(file.path(data_processed, "gtfs_case_study"))) {
  unlink(file.path(data_processed, "gtfs_case_study"), recursive = TRUE)
  message("✅ Old case study folders deleted.")
}

#  STEP 1: Classify deprivation and access into 3 categories 
imd_classified <- imd_lsoa %>%
  mutate(
    deprivation_class = case_when(
      imd_score >= quantile(imd_score, 0.66, na.rm = TRUE) ~ "high",
      imd_score <= quantile(imd_score, 0.33, na.rm = TRUE) ~ "low",
      TRUE ~ "medium"
    ),
    bhs_deprivation_class = case_when(
      bhs_score >= quantile(bhs_score, 0.66, na.rm = TRUE) ~ "high",
      bhs_score <= quantile(bhs_score, 0.33, na.rm = TRUE) ~ "low",
      TRUE ~ "medium"
    ),
    access_class = case_when(
      gmal_score >= quantile(gmal_score, 0.66, na.rm = TRUE) ~ "high",
      gmal_score <= quantile(gmal_score, 0.33, na.rm = TRUE) ~ "low",
      TRUE ~ "medium"
    ),
    quadrant = paste0(deprivation_class, "_deprivation_", access_class, "_access"),
    bhs_quadrant = paste0(bhs_deprivation_class, "_bhs_deprivation_", access_class, "_access")
  )

#  STEP 2: Select a total of 10 case studies 
set.seed(2025)  # for reproducibility

# Select 1 from each of the 4 deprivation quadrants
case_matrix_imd <- imd_classified %>%
  filter(quadrant %in% c(
    "high_deprivation_low_access",
    "high_deprivation_high_access",
    "low_deprivation_low_access",
    "low_deprivation_high_access"
  )) %>%
  group_by(quadrant) %>%
  slice_sample(n = 1) %>%
  ungroup()

# Select 1 from each of the 4 BHS deprivation quadrants
case_matrix_bhs <- imd_classified %>%
  filter(bhs_quadrant %in% c(
    "high_bhs_deprivation_low_access",
    "high_bhs_deprivation_high_access",
    "low_bhs_deprivation_low_access",
    "low_bhs_deprivation_high_access"
  )) %>%
  group_by(bhs_quadrant) %>%
  slice_sample(n = 1) %>%
  ungroup()

# Add the LSOA with the highest overall transit desert score
top_desert <- imd_lsoa %>%
  arrange(desc(transit_desert)) %>%
  slice(1)

# Add the LSOA with the highest BHS transit desert score
top_desert_bhs_extreme <- imd_lsoa %>%
  arrange(desc(transit_desert_bhs)) %>%
  slice(1)

# Combine all selections and remove duplicates
case_lsoas <- bind_rows(case_matrix_imd, case_matrix_bhs, top_desert, top_desert_bhs_extreme) %>%
  distinct(lsoa11cd, .keep_all = TRUE)

# Add a selection reason for each LSOA
case_lsoas <- case_lsoas %>%
  mutate(
    selection_reason = case_when(
      quadrant == "high_deprivation_low_access" ~ "Mismatch: deprived + underserved",
      quadrant == "high_deprivation_high_access" ~ "Match: deprived + well served",
      quadrant == "low_deprivation_low_access" ~ "Mismatch: not deprived but isolated",
      quadrant == "low_deprivation_high_access" ~ "Baseline: not deprived + well served",
      bhs_quadrant == "high_bhs_deprivation_low_access" ~ "BHS Mismatch: deprived + underserved",
      bhs_quadrant == "high_bhs_deprivation_high_access" ~ "BHS Match: deprived + well served",
      bhs_quadrant == "low_bhs_deprivation_low_access" ~ "BHS Mismatch: not deprived but isolated",
      bhs_quadrant == "low_bhs_deprivation_high_access" ~ "BHS Baseline: not deprived + well served",
      lsoa11cd %in% top_desert$lsoa11cd ~ "Extreme: highest overall transit desert score",
      lsoa11cd %in% top_desert_bhs_extreme$lsoa11cd ~ "BHS Extreme: highest BHS transit desert score",
      TRUE ~ "Unknown"
    )
  )

# Save for reference
saveRDS(case_lsoas, file.path(data_processed, "case_study_lsoas.rds"))

# Create a selection category for mapping purposes
case_lsoas <- case_lsoas %>%
  mutate(
    selection_category = case_when(
      grepl("Mismatch", selection_reason) ~ "Mismatch",
      grepl("Match", selection_reason) ~ "Match",
      grepl("Extreme", selection_reason) ~ "Extreme",
      grepl("Baseline", selection_reason) ~ "Baseline",
      TRUE ~ "Other"
    )
  )

# Define a palette for the categories
category_colors <- c(
  "Mismatch" = "#d73027",  # Red for high need, low access
  "Extreme" = "#a50026",   # Dark red for most extreme
  "Match" = "#4575b4",     # Blue for high need, high access
  "Baseline" = "#abd9e9",  # Light blue for low need, high access
  "Other" = "#fee090"      # Yellow for other cases
)

case_lsoas %>%
  st_drop_geometry() %>%
  select(lsoa11cd, quadrant, bhs_quadrant, transit_desert, transit_desert_bhs, selection_reason) %>%
  arrange(selection_reason) %>%
  print(n = Inf)

# Ensure correct CRS (same as basemap/stops if needed)
case_lsoas <- st_transform(case_lsoas, 27700)
gtfs_stops <- st_transform(gtfs_stops, 27700)

# Base map: all LSOAs shaded by overall transit desert score
base_map <- tm_shape(imd_lsoa) +
  tm_polygons("transit_desert", style = "quantile", palette = "YlOrRd", alpha = 0.5) +
  tm_shape(case_lsoas) +
  tm_borders(col = "black", lwd = 2) +
  tm_text("lsoa11cd", size = 1.6) +
  tm_shape(gtfs_stops) +
  tm_dots(col = "grey70", size = 0.01, alpha = 0.3) +
  tm_layout(title = "Selected Case Study LSOAs", legend.outside = TRUE) +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))
  
# Plot it interactively or save to file
tmap_mode("plot")
base_map

#  STEP 3: Prepare spatial data for each case study area 
for (i in seq_len(nrow(case_lsoas))) {
  lsoa <- case_lsoas[i, ]
  lsoa_code <- lsoa$lsoa11cd
  message(paste("Processing:", lsoa_code))
  
  # Buffer LSOA by 500m
  buffer <- st_buffer(lsoa, dist = 500)
  
  # Get GTFS stops within buffer
  stops_in_buffer <- gtfs_stops[buffer, ]
  
  # Output folder
  case_dir <- file.path(data_processed, "gtfs_case_study", lsoa_code)
  dir.create(case_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save spatial data
  saveRDS(lsoa, file.path(case_dir, "lsoa_geometry.rds"))
  saveRDS(buffer, file.path(case_dir, "buffer.rds"))
  saveRDS(stops_in_buffer, file.path(case_dir, "stops_sf.rds"))
}

message("✅ Case study spatial data setup complete.")

# Reload saved buffers for all case studies
buffer_paths <- list.files(file.path(data_processed, "gtfs_case_study"), pattern = "buffer.rds", recursive = TRUE, full.names = TRUE)
buffers <- lapply(buffer_paths, readRDS) %>% bind_rows()

# Ensure all layers are in same CRS
buffers <- st_transform(buffers, 27700)
case_lsoas <- st_transform(case_lsoas, 27700)
gtfs_stops <- st_transform(gtfs_stops, 27700)

# Create map
tmap_mode("view")
tm_shape(buffers) +
  tm_borders(col = "blue", lwd = 2) +
  tm_shape(case_lsoas) +
  tm_borders(col = "black", lwd = 2) +
  tm_text("lsoa11cd", size = 0.8, col = "black") +
  tm_shape(gtfs_stops) +
  tm_dots(col = "red", size = 0.3, alpha = 1) +
  tm_layout(title = "Case Study Buffers and Nearby GTFS Stops", legend.outside = TRUE)

#  INDIVIDUAL CASE STUDY MAPS FOR APPENDIX 

# Create individual detailed maps for each case study
create_individual_case_map <- function(lsoa_code) {
  # Get case study info
  case_info <- case_lsoas[case_lsoas$lsoa11cd == lsoa_code, ]
  
  # Load spatial data
  case_dir <- file.path(data_processed, "gtfs_case_study", lsoa_code)
  lsoa_geom <- readRDS(file.path(case_dir, "lsoa_geometry.rds"))
  buffer_500m <- readRDS(file.path(case_dir, "buffer.rds"))
  stops_in_buffer <- readRDS(file.path(case_dir, "stops_sf.rds"))
  
  # Create 5km context buffer for more geographic context
  context_buffer <- st_buffer(lsoa_geom, dist = 5000)
  
  # Get the specific color for this case
  case_color <- category_colors[[case_info$selection_category[1]]]
  
  # Create the map with OpenStreetMap basemap
  individual_map <- tm_shape(context_buffer) +
    tm_basemap(server = "CartoDB.Positron") +
    
    # Add 500m service buffer
    tm_shape(buffer_500m) +
    tm_borders(col = "blue", lwd = 2, lty = "dashed") +
    tm_fill(col = "lightblue", alpha = 0.3) +
    
    # Highlight the case study LSOA
    tm_shape(lsoa_geom) +
    tm_borders(col = "black", lwd = 3) +
    tm_fill(col = case_color, alpha = 0.8) +
    
    # Add GTFS stops (smaller size)
    tm_shape(stops_in_buffer) +
    tm_dots(col = "black", size = 0.2, alpha = 0.9) +
    
    # Layout and annotations
    tm_layout(
      title = paste("Case Study:", lsoa_code),
      subtitle = paste("Selection:", case_info$selection_reason[1]),
      main.title.size = 1.4,
      main.title.position = "center",
      frame = TRUE,
      inner.margins = c(0.1, 0.1, 0.1, 0.1)
    ) +
    
    # Add legend
    tm_add_legend(
      type = "fill", 
      col = case_color,
      labels = case_info$selection_category[1],
      title = "Selection Type"
    ) +
    tm_add_legend(
      type = "symbol",
      col = "black", 
      size = 1.2,
      labels = paste("GTFS Stops (n=", nrow(stops_in_buffer), ")"),
      title = "Transport"
    ) +
    tm_add_legend(
      type = "line",
      col = "blue",
      lwd = 2,
      lty = "dashed",
      labels = "500m Service Buffer",
      title = ""
    ) +
    
    # Add compass and scale (simplified to avoid tmap bug)
    tm_compass(position = c("right", "top"), size = 1)
  
  return(individual_map)
}

# Generate all individual maps
tmap_mode("plot")
individual_maps <- map(case_lsoas$lsoa11cd, create_individual_case_map)
names(individual_maps) <- case_lsoas$lsoa11cd

# Save individual maps to files
map2(individual_maps, names(individual_maps), function(map_obj, lsoa_code) {
  filename <- paste0("case_study_", lsoa_code, ".png")
  tmap_save(map_obj, 
            filename = file.path(maps_out, filename),
            width = 8, height = 6, dpi = 300)
  message("Saved: ", filename)
})

# Create a summary table for the appendix
appendix_case_summary <- case_lsoas %>%
  st_drop_geometry() %>%
  select(lsoa11cd, selection_reason, selection_category, 
         transit_desert, transit_desert_bhs, imd_score, gmal_score) %>%
  mutate(
    transit_desert = round(transit_desert, 2),
    transit_desert_bhs = round(transit_desert_bhs, 2),
    imd_score = round(imd_score, 1),
    gmal_score = round(gmal_score, 1)
  ) %>%
  arrange(selection_category, lsoa11cd) %>%
  rename(
    "LSOA Code" = lsoa11cd,
    "Selection Rationale" = selection_reason,
    "Selection Category" = selection_category,
    "Transit Desert Score" = transit_desert,
    "BHS Transit Desert Score" = transit_desert_bhs,
    "IMD Score" = imd_score,
    "GMAL Score" = gmal_score
  )

# Save summary table
write_csv(appendix_case_summary, file.path(tables_out, "appendix_case_study_summary.csv"))
print("Case Study Summary for Appendix:")
print(appendix_case_summary)

# Create a simple overview map for the main text (just locations)
simple_overview_map <- tm_shape(imd_lsoa) +
  tm_borders(col = "lightgray", lwd = 0.2) +
  tm_fill(col = "white") +
  tm_shape(case_lsoas) +
  tm_dots(col = "selection_category", 
          palette = category_colors,
          size = 2,
          title = "Selection Type") +
  tm_text("lsoa11cd", size = 0.6, col = "black", 
          auto.placement = TRUE, 
          ymod = 1.2,  
          fontface = "bold") +
  tm_layout(
    title = "Case Study Locations",
    legend.outside = TRUE,
    main.title.size = 1.4,
    frame = TRUE
  ) +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))

# Save simple overview
tmap_save(simple_overview_map, 
          filename = file.path(maps_out, "case_study_overview_simple.png"),
          width = 10, height = 8, dpi = 300)

message("✅ Individual case study maps created for appendix")
message("  • 10 individual detailed maps saved as PNG files")
message("  • Simple overview map for main text")
message("  • Summary table for appendix")
message("  • Files saved in:", maps_out)