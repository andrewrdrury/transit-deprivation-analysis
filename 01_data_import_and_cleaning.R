# 01_data_import_and_cleaning.R
# Load and harmonise spatial base layers

# Install packages if you don't have them yet
install.packages(c("sf", "tidyverse", "tidytransit", "janitor"), lib = .libPaths()[1], type = "binary")

# Load libraries
library(sf)
library(tidyverse)
library(tidytransit)
library(janitor)

# Load config
source("00_config.R")

# Load IMD shapefile (already spatial + attribute joined)
imd_lsoa <- st_read(imd_shp_path) %>%
  st_transform(crs = crs_project) %>%
  clean_names()

saveRDS(imd_lsoa, file.path(data_processed, "imd_lsoa.rds"))

# Load GMAL shapefile
gmal_sf <- st_read(gmal_path) %>%
  st_transform(crs = crs_project) %>%
  clean_names()

saveRDS(gmal_sf, file.path(data_processed, "gmal_sf.rds"))

# Load GTFS and extract stop locations
gtfs_obj <- read_gtfs(gtfs_path)

gtfs_stops <- gtfs_obj$stops %>%
  filter(!is.na(stop_lat), !is.na(stop_lon)) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
  st_transform(crs = crs_project)

saveRDS(gtfs_stops, file.path(data_processed, "gtfs_stops_sf.rds"))

# Basic completion check
message("Data import complete")
message(paste("• IMD features:", nrow(imd_lsoa)))
message(paste("• GMAL features:", nrow(gmal_sf)))
message(paste("• GTFS stops:", nrow(gtfs_stops)))