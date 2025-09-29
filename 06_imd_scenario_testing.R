# 06_imd_integration.R
# Integrate GTFS-based accessibility into IMD framework

# Load libraries
library(tidyverse)
library(sf)

# Load config and results
source("00_config.R")
gtfs_results <- readRDS(file.path(data_processed, "gtfs_accessibility_results.rds"))
imd_lsoa <- readRDS(file.path(data_processed, "imd_lsoa.rds"))

#  EXPONENTIAL TRANSFORMATION FUNCTION 
# Based on IMD 2019 Technical Report methodology
apply_exponential_transformation <- function(domain_scores) {
  # Step 1: Rank the domain scores (1 = least deprived, n = most deprived)
  domain_ranks <- rank(domain_scores, ties.method = "average")
  
  # Step 2: Transform ranks to exponential distribution
  # The IMD uses: exp(-23 + 23 * (rank/max_rank))
  n <- length(domain_ranks)
  
  # Normalize ranks to 0-1 scale
  normalized_ranks <- (domain_ranks - 1) / (n - 1)
  
  # Apply exponential transformation (following IMD methodology)
  # This emphasizes the most deprived end of the distribution
  exponential_scores <- exp(-23 + 23 * normalized_ranks)
  
  return(exponential_scores)
}

#  CONSTRUCT TRANSPORT ACCESSIBILITY DOMAIN 

create_transport_domain <- function(accessibility_data) {
  transport_domain <- accessibility_data %>%
    mutate(
      # Convert accessibility metrics to deprivation indicators
      # (higher scores = worse access = more deprived)
      
      # Service availability indicators (inverted - lower is worse)
      svc_stop_density = -stop_density,
      svc_peak_freq = -peak_frequency,
      svc_offpeak_freq = -offpeak_frequency,
      
      # Connectivity indicators (inverted)
      conn_route_diversity = -route_diversity,
      conn_service_span = -service_span,
      
      # Physical access (walking distance - higher is worse)
      phys_min_walk = min_walk_to_stop,
      phys_avg_walk = avg_walk_to_stop
    ) %>%
    # Standardise all indicators to have mean=0, sd=1
    mutate(
      z_svc_density = as.numeric(scale(svc_stop_density)),
      z_svc_peak = as.numeric(scale(svc_peak_freq)),
      z_svc_offpeak = as.numeric(scale(svc_offpeak_freq)),
      z_conn_routes = as.numeric(scale(conn_route_diversity)),
      z_conn_span = as.numeric(scale(conn_service_span)),
      z_phys_min = as.numeric(scale(phys_min_walk)),
      z_phys_avg = as.numeric(scale(phys_avg_walk))
    ) %>%
    # Handle NAs (replace with mean = 0)
    mutate(across(starts_with("z_"), ~ ifelse(is.na(.), 0, .))) %>%
    # Calculate weighted transport accessibility domain score
    mutate(
      transport_domain_score = (
        z_svc_density * 0.25 +    # Service availability: 50%
          z_svc_peak * 0.15 +
          z_svc_offpeak * 0.10 +
          z_conn_routes * 0.20 +    # Connectivity: 30%
          z_conn_span * 0.10 +
          z_phys_min * 0.15 +       # Physical access: 20%
          z_phys_avg * 0.05
      )
    ) %>%
    # Convert to rank-based score (like other IMD domains)
    mutate(
      transport_rank = rank(transport_domain_score, ties.method = "average"),
      transport_decile = ntile(transport_domain_score, 10)
    )
  
  return(transport_domain)
}

# Calculate transport domain for case studies
transport_domain_data <- create_transport_domain(gtfs_results)

#  IMD RECALCULATION FUNCTIONS 

# Original IMD domain weights (2019)
original_weights <- list(
  income = 0.225,
  employment = 0.225,
  education = 0.135,
  health = 0.135,
  crime = 0.093,
  barriers = 0.093,
  environment = 0.093
)

# Approach A: Equal weighting integration
approach_a_weights <- list(
  income = 0.20,
  employment = 0.20,
  education = 0.12,
  health = 0.12,
  crime = 0.08,
  barriers = 0.08,
  environment = 0.08,
  transport = 0.12
)

# Approach B: Reduced BHS integration
approach_b_weights <- list(
  income = 0.225,
  employment = 0.225,
  education = 0.135,
  health = 0.135,
  crime = 0.093,
  barriers = 0.04,  # Significantly reduced
  environment = 0.093,
  transport = 0.054
)

# Function to calculate modified IMD using exponentially transformed scores
calculate_modified_imd_exponential <- function(data, weights, include_transport = TRUE) {
  if (include_transport) {
    modified_score <- (
      data$exp_inc_score * weights$income +
        data$exp_emp_score * weights$employment +
        data$exp_edu_score * weights$education +
        data$exp_hdd_score * weights$health +
        data$exp_cri_score * weights$crime +
        data$exp_bhs_score * weights$barriers +
        data$exp_env_score * weights$environment +
        data$exp_transport_score * weights$transport
    )
  } else {
    # Original IMD calculation with exponential transformation
    modified_score <- (
      data$exp_inc_score * weights$income +
        data$exp_emp_score * weights$employment +
        data$exp_edu_score * weights$education +
        data$exp_hdd_score * weights$health +
        data$exp_cri_score * weights$crime +
        data$exp_bhs_score * weights$barriers +
        data$exp_env_score * weights$environment
    )
  }
  
  return(modified_score)
}

# Legacy function for simple weighted sum (for comparison)
calculate_modified_imd_simple <- function(data, weights, include_transport = TRUE) {
  if (include_transport) {
    modified_score <- (
      data$inc_score * weights$income +
        data$emp_score * weights$employment +
        data$edu_score * weights$education +
        data$hdd_score * weights$health +
        data$cri_score * weights$crime +
        data$bhs_score * weights$barriers +
        data$env_score * weights$environment +
        data$transport_domain_score * weights$transport
    )
  } else {
    # Original IMD calculation
    modified_score <- (
      data$inc_score * weights$income +
        data$emp_score * weights$employment +
        data$edu_score * weights$education +
        data$hdd_score * weights$health +
        data$cri_score * weights$crime +
        data$bhs_score * weights$barriers +
        data$env_score * weights$environment
    )
  }
  
  return(modified_score)
}

#  APPLY IMD MODIFICATIONS TO CASE STUDIES 

# First, we need to get the original IMD scores for our case studies
case_study_imd <- imd_lsoa %>%
  filter(lsoa11cd %in% transport_domain_data$lsoa11cd) %>%
  st_drop_geometry()

# Join transport domain scores
imd_with_transport <- case_study_imd %>%
  left_join(
    transport_domain_data %>% 
      select(lsoa11cd, transport_domain_score, transport_rank, transport_decile),
    by = "lsoa11cd"
  )

#  APPLY EXPONENTIAL TRANSFORMATION TO ALL DOMAINS 
message("Applying exponential transformation to domain scores...")

imd_with_transport <- imd_with_transport %>%
  mutate(
    # Apply exponential transformation to each domain
    exp_inc_score = apply_exponential_transformation(inc_score),
    exp_emp_score = apply_exponential_transformation(emp_score),
    exp_edu_score = apply_exponential_transformation(edu_score),
    exp_hdd_score = apply_exponential_transformation(hdd_score),
    exp_cri_score = apply_exponential_transformation(cri_score),
    exp_bhs_score = apply_exponential_transformation(bhs_score),
    exp_env_score = apply_exponential_transformation(env_score),
    exp_transport_score = apply_exponential_transformation(transport_domain_score)
  ) %>%
  # Calculate IMD scores using exponential transformation (methodologically correct)
  mutate(
    # Original IMD score using exponential transformation
    imd_original_exp = calculate_modified_imd_exponential(., original_weights, include_transport = FALSE),
    
    # Modified IMD scores using exponential transformation
    imd_approach_a_exp = calculate_modified_imd_exponential(., approach_a_weights, include_transport = TRUE),
    imd_approach_b_exp = calculate_modified_imd_exponential(., approach_b_weights, include_transport = TRUE),
    
    # Calculate ranks for exponential method
    rank_original_exp = rank(imd_original_exp, ties.method = "average"),
    rank_approach_a_exp = rank(imd_approach_a_exp, ties.method = "average"),
    rank_approach_b_exp = rank(imd_approach_b_exp, ties.method = "average"),
    
    # Calculate rank changes using exponential method
    rank_change_a_exp = rank_approach_a_exp - rank_original_exp,
    rank_change_b_exp = rank_approach_b_exp - rank_original_exp,
    
    # Calculate score changes using exponential method
    score_change_a_exp = ((imd_approach_a_exp - imd_original_exp) / imd_original_exp) * 100,
    score_change_b_exp = ((imd_approach_b_exp - imd_original_exp) / imd_original_exp) * 100
  ) %>%
  # Also calculate simple weighted sum method for comparison
  mutate(
    # Simple weighted sum calculations (for comparison only)
    imd_original_simple = calculate_modified_imd_simple(., original_weights, include_transport = FALSE),
    imd_approach_a_simple = calculate_modified_imd_simple(., approach_a_weights, include_transport = TRUE),
    imd_approach_b_simple = calculate_modified_imd_simple(., approach_b_weights, include_transport = TRUE),
    
    # Ranks for simple method
    rank_original_simple = rank(imd_original_simple, ties.method = "average"),
    rank_approach_a_simple = rank(imd_approach_a_simple, ties.method = "average"),
    rank_approach_b_simple = rank(imd_approach_b_simple, ties.method = "average"),
    
    # Rank changes for simple method
    rank_change_a_simple = rank_approach_a_simple - rank_original_simple,
    rank_change_b_simple = rank_approach_b_simple - rank_original_simple
  )

#  COMPARISON BETWEEN METHODS 
method_comparison <- imd_with_transport %>%
  select(lsoa11cd, 
         # Exponential transformation results (methodologically correct)
         rank_change_a_exp, rank_change_b_exp,
         # Simple weighted sum results (for comparison)
         rank_change_a_simple, rank_change_b_simple) %>%
  mutate(
    # Compare methods
    rank_diff_method_a = rank_change_a_exp - rank_change_a_simple,
    rank_diff_method_b = rank_change_b_exp - rank_change_b_simple
  )

message("=== COMPARISON: Simple Weighted Sum vs Exponential Transformation ===")
cat("Approach A - Mean absolute difference in rank changes:", 
    round(mean(abs(method_comparison$rank_diff_method_a), na.rm = TRUE), 2), "\n")
cat("Approach B - Mean absolute difference in rank changes:", 
    round(mean(abs(method_comparison$rank_diff_method_b), na.rm = TRUE), 2), "\n")
cat("Maximum difference in rank changes (Approach A):",
    max(abs(method_comparison$rank_diff_method_a), na.rm = TRUE), "\n")

#  ANALYSIS AND SUMMARY (Using Exponential Transformation Results) 

# Create summary of changes using exponentially transformed scores
imd_changes_summary <- imd_with_transport %>%
  select(
    lsoa11cd, 
    # Original scores and ranks (exponential method)
    imd_original_exp, rank_original_exp,
    # Transport accessibility
    transport_domain_score, transport_decile,
    # Modified scores and ranks (exponential method)
    imd_approach_a_exp, rank_approach_a_exp, rank_change_a_exp, score_change_a_exp,
    imd_approach_b_exp, rank_approach_b_exp, rank_change_b_exp, score_change_b_exp,
    # Context variables
    inc_score, bhs_score, gmal_score
  ) %>%
  # Rename for clarity
  rename(
    imd_original = imd_original_exp,
    rank_original = rank_original_exp,
    imd_approach_a = imd_approach_a_exp,
    rank_approach_a = rank_approach_a_exp,
    rank_change_a = rank_change_a_exp,
    score_change_a = score_change_a_exp,
    imd_approach_b = imd_approach_b_exp,
    rank_approach_b = rank_approach_b_exp,
    rank_change_b = rank_change_b_exp,
    score_change_b = score_change_b_exp
  ) %>%
  arrange(desc(abs(rank_change_a)))

# Print summary
message("=== IMD Integration Results (Exponential Transformation Method) ===")
print(imd_changes_summary)

# Statistical summary of changes
change_stats <- imd_changes_summary %>%
  summarise(
    mean_rank_change_a = mean(rank_change_a, na.rm = TRUE),
    sd_rank_change_a = sd(rank_change_a, na.rm = TRUE),
    mean_score_change_a = mean(score_change_a, na.rm = TRUE),
    mean_rank_change_b = mean(rank_change_b, na.rm = TRUE),
    sd_rank_change_b = sd(rank_change_b, na.rm = TRUE),
    mean_score_change_b = mean(score_change_b, na.rm = TRUE),
    max_rank_change_a = max(abs(rank_change_a), na.rm = TRUE),
    max_rank_change_b = max(abs(rank_change_b), na.rm = TRUE)
  )

print("Change Statistics (Exponential Transformation):")
print(change_stats)

#  DETAILED CASE ANALYSIS 

# Identify cases with largest changes
significant_changes <- imd_changes_summary %>%
  mutate(
    significant_change_a = abs(rank_change_a) > 2,  # More than 2 rank positions
    significant_change_b = abs(rank_change_b) > 2,
    change_direction_a = case_when(
      rank_change_a > 2 ~ "More Deprived",
      rank_change_a < -2 ~ "Less Deprived", 
      TRUE ~ "Minimal Change"
    ),
    change_direction_b = case_when(
      rank_change_b > 2 ~ "More Deprived",
      rank_change_b < -2 ~ "Less Deprived",
      TRUE ~ "Minimal Change"
    )
  )

# Analysis by change type
change_analysis <- significant_changes %>%
  count(change_direction_a, name = "approach_a_count") %>%
  full_join(
    significant_changes %>% count(change_direction_b, name = "approach_b_count"),
    by = c("change_direction_a" = "change_direction_b")
  ) %>%
  rename(change_direction = change_direction_a)

print("Distribution of Changes:")
print(change_analysis)

#  CORRELATION ANALYSIS 

# First, add gtfs_accessibility_index to our data
gtfs_accessibility_data <- readRDS(file.path(data_processed, "gtfs_accessibility_results.rds"))

imd_with_transport <- imd_with_transport %>%
  left_join(
    gtfs_accessibility_data %>% 
      select(lsoa11cd, gtfs_accessibility_index), 
    by = "lsoa11cd"
  )

# Examine relationships between transport accessibility and other domains
correlation_analysis <- imd_with_transport %>%
  select(
    transport_domain_score, 
    inc_score, emp_score, edu_score, hdd_score, 
    cri_score, bhs_score, env_score,
    gmal_score, gtfs_accessibility_index
  ) %>%
  cor(use = "complete.obs")

print("Correlation Matrix (Transport Domain vs Other Domains):")
print(round(correlation_analysis["transport_domain_score", ], 3))

#  VISUALISATION DATA PREPARATION 

# Prepare data for plotting using exponential transformation results
plot_data <- imd_with_transport %>%
  select(
    lsoa11cd,
    # Use exponential transformation results
    imd_original_exp, imd_approach_a_exp, imd_approach_b_exp,
    rank_original_exp, rank_approach_a_exp, rank_approach_b_exp,
    transport_domain_score, transport_decile,
    inc_score, bhs_score
  ) %>%
  rename(
    imd_original = imd_original_exp,
    imd_approach_a = imd_approach_a_exp,
    imd_approach_b = imd_approach_b_exp,
    rank_original = rank_original_exp,
    rank_approach_a = rank_approach_a_exp,
    rank_approach_b = rank_approach_b_exp
  ) %>%
  pivot_longer(
    cols = c(imd_original, imd_approach_a, imd_approach_b),
    names_to = "imd_version",
    values_to = "imd_score"
  ) %>%
  pivot_longer(
    cols = c(rank_original, rank_approach_a, rank_approach_b),
    names_to = "rank_version", 
    values_to = "imd_rank"
  ) %>%
  filter(
    (imd_version == "imd_original" & rank_version == "rank_original") |
      (imd_version == "imd_approach_a" & rank_version == "rank_approach_a") |
      (imd_version == "imd_approach_b" & rank_version == "rank_approach_b")
  ) %>%
  mutate(
    approach = case_when(
      imd_version == "imd_original" ~ "Original IMD",
      imd_version == "imd_approach_a" ~ "Approach A (Equal Weight)",
      imd_version == "imd_approach_b" ~ "Approach B (Reduced BHS)",
      TRUE ~ "Unknown"
    )
  )

#  DOMAIN CONTRIBUTION ANALYSIS 

# Calculate how much each domain contributes to the final score for each approach
# Using exponentially transformed scores
domain_contributions <- imd_with_transport %>%
  mutate(
    # Original IMD contributions (exponential)
    orig_income_contrib = exp_inc_score * original_weights$income,
    orig_emp_contrib = exp_emp_score * original_weights$employment,
    orig_edu_contrib = exp_edu_score * original_weights$education,
    orig_health_contrib = exp_hdd_score * original_weights$health,
    orig_crime_contrib = exp_cri_score * original_weights$crime,
    orig_barriers_contrib = exp_bhs_score * original_weights$barriers,
    orig_env_contrib = exp_env_score * original_weights$environment,
    
    # Approach A contributions (exponential)
    a_income_contrib = exp_inc_score * approach_a_weights$income,
    a_emp_contrib = exp_emp_score * approach_a_weights$employment,
    a_edu_contrib = exp_edu_score * approach_a_weights$education,
    a_health_contrib = exp_hdd_score * approach_a_weights$health,
    a_crime_contrib = exp_cri_score * approach_a_weights$crime,
    a_barriers_contrib = exp_bhs_score * approach_a_weights$barriers,
    a_env_contrib = exp_env_score * approach_a_weights$environment,
    a_transport_contrib = exp_transport_score * approach_a_weights$transport,
    
    # Calculate transport domain impact
    transport_impact_a = a_transport_contrib,
    total_change_a = imd_approach_a_exp - imd_original_exp,
    transport_proportion_a = transport_impact_a / imd_approach_a_exp
  )

# Summary of transport domain impact
transport_impact_summary <- domain_contributions %>%
  summarise(
    mean_transport_contrib = mean(a_transport_contrib, na.rm = TRUE),
    sd_transport_contrib = sd(a_transport_contrib, na.rm = TRUE),
    mean_transport_proportion = mean(transport_proportion_a, na.rm = TRUE),
    min_transport_proportion = min(transport_proportion_a, na.rm = TRUE),
    max_transport_proportion = max(transport_proportion_a, na.rm = TRUE)
  )

print("Transport Domain Impact Summary:")
print(transport_impact_summary)

#  SAVE RESULTS 

# Save comprehensive results
final_results <- list(
  imd_scores = imd_with_transport,
  imd_scores_exponential = imd_changes_summary,  # Primary results using exponential transformation
  method_comparison = method_comparison,
  changes_summary = imd_changes_summary,
  significant_changes = significant_changes,
  change_statistics = change_stats,
  correlation_matrix = correlation_analysis,
  domain_contributions = domain_contributions,
  transport_impact = transport_impact_summary,
  plot_data = plot_data
)

saveRDS(final_results, file.path(data_processed, "imd_integration_results.rds"))

# Save individual tables for easy access
write_csv(imd_changes_summary, file.path(tables_out, "imd_changes_summary.csv"))
write_csv(significant_changes, file.path(tables_out, "significant_changes.csv"))
write_csv(method_comparison, file.path(tables_out, "method_comparison.csv"))

#  CASE STUDY PROFILES 

# Create detailed profiles for each case study showing how transport affects their deprivation
case_profiles <- imd_changes_summary %>%
  left_join(
    gtfs_results %>% 
      select(lsoa11cd, selection_reason, quadrant, 
             stop_density, peak_frequency, route_diversity),
    by = "lsoa11cd"
  ) %>%
  mutate(
    profile_summary = paste0(
      "LSOA: ", lsoa11cd, "\n",
      "Selection: ", selection_reason, "\n",
      "Original IMD Rank: ", rank_original, "\n",
      "Approach A Rank: ", rank_approach_a, " (change: ", 
      ifelse(rank_change_a > 0, "+", ""), rank_change_a, ")\n",
      "Transport Decile: ", transport_decile, "/10 (worst)\n",
      "Stops/km²: ", round(stop_density, 1), "\n",
      "Peak services/hour: ", round(peak_frequency, 1)
    )
  )

# Save case profiles
write_csv(case_profiles, file.path(case_out, "case_study_profiles.csv"))

# Print final summary
message("=== FINAL SUMMARY (Exponential Transformation Method) ===")
message(paste("Case studies processed:", nrow(imd_changes_summary)))
message(paste("Cases with significant rank changes (Approach A):", 
              sum(abs(imd_changes_summary$rank_change_a) > 2, na.rm = TRUE)))
message(paste("Mean rank change (Approach A):", 
              round(mean(imd_changes_summary$rank_change_a, na.rm = TRUE), 2)))
message(paste("Max rank change (Approach A):", 
              max(abs(imd_changes_summary$rank_change_a), na.rm = TRUE)))

message("Note: Results now use exponential transformation following official IMD methodology")
message("Simple weighted sum results saved for comparison in method_comparison.csv")

message("✅ IMD integration analysis complete. Results saved to outputs folder.")

#  OPTIONAL: Quick visualization of rank changes 
if(require(ggplot2, quietly = TRUE)) {
  
  # Rank changes plot using exponential transformation results
  rank_change_plot <- ggplot(imd_changes_summary, aes(x = rank_original, y = rank_change_a)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_point(aes(color = transport_decile), size = 3) +
    scale_color_viridis_c(name = "Transport\nDecile", trans = "reverse") +
    labs(
      title = "IMD Rank Changes with Transport Accessibility Integration",
      subtitle = "Approach A: Equal Weighting Integration (Exponential Transformation)",
      x = "Original IMD Rank",
      y = "Change in Rank (positive = more deprived)",
      caption = "Points colored by transport accessibility decile (10 = worst access)\nUsing exponential transformation methodology"
    ) +
    theme_minimal()
  
  ggsave(file.path(maps_out, "rank_changes_scatter.png"), 
         plot = rank_change_plot, width = 10, height = 6, dpi = 300)
  
  message("✅ Visualization saved to maps folder.")
}