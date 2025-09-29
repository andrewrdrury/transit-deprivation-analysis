# 00_config.R
# Centralised project settings and file paths

# install.packages(c("here", "lubridate"), lib = .libPaths()[1], type = "binary")

#  Load project libraries (minimal for config) 
library(here)       # For reproducible file paths
library(lubridate)  # For working with time windows

#  Coordinate Reference System 
crs_project <- 27700  # British National Grid (EPSG:27700)

#  Raw data file paths 
raw_data <- here::here("raw_data")
gtfs_path <- file.path(raw_data, "gtfsmanchester2019oct16.zip")
gmal_path <- file.path(raw_data, "GMAL_grid_open.shp")
imd_shp_path <- file.path(raw_data, "IMDMANCHESTER.shp")  # if not using `imd` package

#  Processed data outputs 
data_processed <- here::here("data_processed")
gtfs_base <- here::here("data_processed", "gtfs_case_study")

#  Output folders 
outputs <- here::here("outputs")
maps_out <- file.path(outputs, "maps")
tables_out <- file.path(outputs, "tables")
case_out <- file.path(outputs, "case_studies")

#  GTFS Settings (THIS IS REDUNDANT!)
# gtfs_time_windows <- list(
# peak = list(start = "07:00:00", end = "10:00:00"),
# midday = list(start = "12:00:00", end = "14:00:00")
# )

# Case Study LSOAs (THIS IS ALSO REDUNDANT!!!)
# case_lsoas <- c("E01005325", "E01004946", "E01004897", "E01005681", "E01006117")

# Create directories if they don’t exist
dir.create(data_processed, recursive = TRUE, showWarnings = FALSE)
dir.create(maps_out, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_out, recursive = TRUE, showWarnings = FALSE)
dir.create(case_out, recursive = TRUE, showWarnings = FALSE)

gtfs_r5_folder <- file.path(data_processed, "gtfs_case_study", "full_network")
gtfs_base <- file.path(data_processed, "gtfs_case_study")
