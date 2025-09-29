# 05_gtfs_network_analysis.R
# Detailed GTFS-based accessibility analysis for case study areas

# Load libraries
library(tidyverse)
library(sf)
library(tidytransit)
library(lubridate)
library(gtfsrouter)  
library(lwgeom)     

# Load config and data
source("00_config.R")
imd_lsoa <- readRDS(file.path(data_processed, "imd_lsoa.rds"))
gtfs_obj <- read_gtfs(gtfs_path)
case_lsoas <- readRDS(file.path(data_processed, "case_study_lsoas.rds"))

#  FUNCTION: Calculate accessibility metrics for one case study 
calculate_case_accessibility <- function(lsoa_code) {
  message(paste("Processing GTFS accessibility for:", lsoa_code))
  
  # Load case study spatial data
  case_dir <- file.path(data_processed, "gtfs_case_study", lsoa_code)
  lsoa <- readRDS(file.path(case_dir, "lsoa_geometry.rds"))
  buffer <- readRDS(file.path(case_dir, "buffer.rds"))
  stops_in_buffer <- readRDS(file.path(case_dir, "stops_sf.rds"))
  
  if(nrow(stops_in_buffer) == 0) {
    message(paste("Warning: No GTFS stops found for", lsoa_code))
    return(data.frame(
      lsoa11cd = lsoa_code,
      stop_density = 0,
      peak_frequency = 0,
      offpeak_frequency = 0,
      route_diversity = 0,
      service_span = 0
    ))
  }
  
  #  1. Basic Stop and Service Metrics 
  
  # Stop density (stops per km²)
  buffer_area_km2 <- as.numeric(st_area(buffer)) / 1e6
  stop_density <- nrow(stops_in_buffer) / buffer_area_km2
  
  # Get stop IDs for filtering GTFS data
  stop_ids <- stops_in_buffer$stop_id
  
  # Filter GTFS stop_times for relevant stops
  stop_times_filtered <- gtfs_obj$stop_times %>%
    filter(stop_id %in% stop_ids)
  
  if(nrow(stop_times_filtered) == 0) {
    message(paste("Warning: No stop times found for", lsoa_code))
    return(data.frame(
      lsoa11cd = lsoa_code,
      stop_density = stop_density,
      peak_frequency = 0,
      offpeak_frequency = 0,
      route_diversity = 0,
      service_span = 0
    ))
  }
  
  #  2. Service Frequency Analysis 
  
  # Convert departure_time to proper time format and calculate frequencies
  frequencies_analysis <- stop_times_filtered %>%
    left_join(gtfs_obj$trips, by = "trip_id") %>%
    left_join(gtfs_obj$routes, by = "route_id") %>%
    mutate(
      # Handle times that go past midnight (e.g., "25:30:00")
      departure_hour = as.numeric(str_extract(departure_time, "^\\d{1,2}")),
      departure_hour = ifelse(departure_hour >= 24, departure_hour - 24, departure_hour)
    ) %>%
    filter(!is.na(departure_hour)) %>%
    # Define peak and off-peak periods
    mutate(
      time_period = case_when(
        departure_hour >= 7 & departure_hour < 10 ~ "peak",
        departure_hour >= 12 & departure_hour < 14 ~ "offpeak",
        TRUE ~ "other"
      )
    ) %>%
    filter(time_period %in% c("peak", "offpeak"))
  
  # Calculate average services per hour by time period
  frequency_summary <- frequencies_analysis %>%
    group_by(time_period) %>%
    summarise(
      total_services = n(),
      unique_routes = n_distinct(route_id),
      .groups = "drop"
    ) %>%
    mutate(
      # Convert to services per hour (3-hour peak, 2-hour offpeak)
      services_per_hour = case_when(
        time_period == "peak" ~ total_services / 3,
        time_period == "offpeak" ~ total_services / 2,
        TRUE ~ total_services
      )
    )
  
  # Extract metrics
  peak_frequency <- frequency_summary %>% 
    filter(time_period == "peak") %>% 
    pull(services_per_hour) %>% 
    ifelse(length(.) == 0, 0, .)
  
  offpeak_frequency <- frequency_summary %>% 
    filter(time_period == "offpeak") %>% 
    pull(services_per_hour) %>% 
    ifelse(length(.) == 0, 0, .)
  
  #  3. Route Diversity 
  route_diversity <- frequencies_analysis %>% 
    distinct(route_id) %>% 
    nrow()
  
  #  4. Service Span (hours of operation) 
  service_span <- stop_times_filtered %>%
    mutate(departure_hour = as.numeric(str_extract(departure_time, "^\\d{1,2}"))) %>%
    filter(departure_hour <= 23) %>% # Remove overnight services for simplicity
    summarise(
      first_service = min(departure_hour, na.rm = TRUE),
      last_service = max(departure_hour, na.rm = TRUE),
      service_span = last_service - first_service
    ) %>%
    pull(service_span) %>%
    ifelse(length(.) == 0 | is.infinite(.), 0, .)
  
  #  5. Destination Accessibility (Simplified) 
  
  # Get LSOA centroid for journey origin
  lsoa_centroid <- st_centroid(lsoa)
  
  # Find nearest stops to LSOA centroid
  if(nrow(stops_in_buffer) > 0) {
    distances_to_stops <- st_distance(lsoa_centroid, stops_in_buffer)
    min_walk_distance <- as.numeric(min(distances_to_stops))
    avg_walk_distance <- as.numeric(mean(distances_to_stops))
  } else {
    min_walk_distance <- NA
    avg_walk_distance <- NA
  }
  
  #  6. Return Results 
  return(data.frame(
    lsoa11cd = lsoa_code,
    stop_density = stop_density,
    peak_frequency = peak_frequency,
    offpeak_frequency = offpeak_frequency,
    route_diversity = route_diversity,
    service_span = service_span,
    min_walk_to_stop = min_walk_distance,
    avg_walk_to_stop = avg_walk_distance,
    total_stops_in_buffer = nrow(stops_in_buffer)
  ))
}

#  MAIN ANALYSIS: Process all case studies 

# Get list of case study LSOAs
case_study_codes <- case_lsoas$lsoa11cd

# Calculate accessibility metrics for each case study
accessibility_results <- map_dfr(case_study_codes, calculate_case_accessibility)

#  CREATE GTFS-BASED ACCESSIBILITY COMPOSITE INDEX 
gtfs_accessibility <- accessibility_results %>%
  mutate(
    # Handle NAs in walking distances first by replacing with median values
    min_walk_to_stop = ifelse(is.na(min_walk_to_stop), 
                              median(min_walk_to_stop, na.rm = TRUE), 
                              min_walk_to_stop),
    avg_walk_to_stop = ifelse(is.na(avg_walk_to_stop), 
                              median(avg_walk_to_stop, na.rm = TRUE), 
                              avg_walk_to_stop)
  ) %>%
  mutate(
    # Standardise metrics (higher values = better accessibility)
    z_stop_density = as.numeric(scale(stop_density)),
    z_peak_freq = as.numeric(scale(peak_frequency)),
    z_offpeak_freq = as.numeric(scale(offpeak_frequency)),
    z_route_diversity = as.numeric(scale(route_diversity)),
    z_service_span = as.numeric(scale(service_span)),
    
    # Invert walk distances (lower distance = better accessibility)
    z_min_walk = as.numeric(scale(-min_walk_to_stop)),
    z_avg_walk = as.numeric(scale(-avg_walk_to_stop)),
    
    # Create composite accessibility index (equal weights)
    gtfs_accessibility_index = (
      z_stop_density + z_peak_freq + z_offpeak_freq + 
        z_route_diversity + z_service_span + z_min_walk + z_avg_walk
    ) / 7
  ) %>%
  # Handle any remaining NAs in the final index
  mutate(
    gtfs_accessibility_index = ifelse(is.na(gtfs_accessibility_index), 
                                      mean(gtfs_accessibility_index, na.rm = TRUE), 
                                      gtfs_accessibility_index)
  )

#  CALCULATE GTFS-BASED TRANSIT DESERT SCORES 
# Join with case study LSOA data to get demand indicators
gtfs_results <- case_lsoas %>%
  st_drop_geometry() %>%
  left_join(gtfs_accessibility, by = "lsoa11cd") %>%
  mutate(
    # GTFS-based transit desert score (demand - accessibility)
    z_demand = as.numeric(scale(demand_index)),
    z_gtfs_access = as.numeric(scale(gtfs_accessibility_index)),
    transit_desert_gtfs = z_demand - z_gtfs_access,
    
    # BHS-specific GTFS transit desert score
    z_bhs = as.numeric(scale(bhs_score)),
    transit_desert_bhs_gtfs = z_bhs - z_gtfs_access
  )

# Save results
saveRDS(gtfs_results, file.path(data_processed, "gtfs_accessibility_results.rds"))

#  COMPARISON WITH GMAL SCORES 
comparison_results <- gtfs_results %>%
  select(lsoa11cd, gmal_score, gtfs_accessibility_index, 
         transit_desert, transit_desert_gtfs,
         transit_desert_bhs, transit_desert_bhs_gtfs) %>%
  mutate(
    # Calculate differences between GMAL and GTFS approaches
    access_diff = gtfs_accessibility_index - scale(gmal_score)[,1],
    desert_diff = transit_desert_gtfs - transit_desert,
    desert_bhs_diff = transit_desert_bhs_gtfs - transit_desert_bhs
  )

# Print summary
message("=== GTFS Accessibility Analysis Complete ===")
print(summary(gtfs_accessibility))
print(comparison_results)

# Save comparison for next script
saveRDS(comparison_results, file.path(data_processed, "accessibility_comparison.rds"))

message("✅ GTFS network analysis complete. Results saved to data_processed folder.")