# Transport Accessibility and Deprivation Analysis

A reproducible R workflow for analyzing the relationship between public transport accessibility and socioeconomic deprivation in Greater Manchester, integrating GTFS-based transport accessibility metrics into the Index of Multiple Deprivation (IMD) framework.

## Overview

This project examines whether poor transit access correlates spatially with high levels of deprivation and tests the implications of incorporating transit accessibility as a new IMD domain. The analysis:

- Identifies "transit deserts" where transport demand exceeds supply
- Develops GTFS-based accessibility metrics beyond simple distance measures
- Constructs a transport accessibility domain following IMD exponential transformation methodology
- Tests integration approaches with different domain weightings
- Validates findings through sensitivity analysis and method comparison

## Key Features

### Methodological Innovation
- **Exponential transformation**: Implements official IMD 2019 methodology for domain scoring to prevent cancellation effects
- **GTFS-based metrics**: Calculates stop density, service frequency (peak/off-peak), route diversity, service span, and walking distances
- **Demand-supply framework**: Identifies transit deserts using composite demand indices based on demographics and deprivation
- **Strategic case study selection**: Systematic quadrant-based sampling across deprivation-accessibility combinations

### Research Questions Addressed

1. How can transit deserts be systematically identified in a way compatible with the IMD framework?
2. How do different accessibility measures (GMAL vs GTFS) influence transit desert identification?
3. To what extent do areas of high transport disadvantage overlap with existing deprivation patterns?
4. What impact does integrating transport accessibility into the IMD have on deprivation scores and rankings?

## Project Structure

```
├── 00_config.R                      # Project configuration and file paths
├── 01_data_import_and_cleaning.R    # Load and harmonize spatial data
├── 02_gmal_processing_and_mapping.R # GMAL aggregation and transit desert identification
├── 03_gtfs_case_study_setup.R       # Quadrant-based case study selection
├── 04_correlation_analysis.R        # Bivariate correlation analysis
├── 05_gtfs_network_analysis.R       # Detailed GTFS accessibility metrics
├── 06_imd_scenario_testing.R        # IMD integration with exponential transformation
└── 07_validation_and_reporting.R    # Validation, sensitivity analysis, reporting
```

## Requirements

### R Packages
```r
# Spatial analysis
sf, tmap, lwgeom

# Data manipulation
tidyverse, dplyr, janitor, lubridate

# Transport data
tidytransit, gtfsrouter

# Visualization
ggplot2, corrplot, knitr, kableExtra
```

### Data Sources
- **IMD 2019**: Index of Multiple Deprivation (Consumer Data Research Centre)
- **GMAL 2019**: Greater Manchester Accessibility Levels (data.gov.uk)
- **GTFS 2019**: Transport for Greater Manchester schedule data (transitfeeds.com)

## Key Results

- **Weak negative correlation** (r = -0.184, p < 0.001) between transit demand and accessibility, confirming spatial mismatch
- **55% of LSOAs** identified as overall transit deserts, with 49% showing BHS-specific transport disadvantage
- **Strong GTFS-GMAL correlation** (r = 0.807) validates GTFS-based approach
- **Exponential transformation essential**: Simple weighted sums produced zero rank changes, while exponential transformation revealed meaningful transport impacts
- **Selective ranking adjustments**: 70% of case studies experienced minimal rank changes, but extreme cases showed transport integration revealing hidden disadvantage (e.g., one area declined 6 positions)
- **Robust to weighting**: Alternative transport domain weights (5-15%) produced consistent results, suggesting inclusion decision matters more than specific weight

## Critical Findings

The analysis demonstrates that:
1. Transport accessibility captures distinct deprivation dimensions not fully represented in existing IMD domains
2. Exponential transformation methodology is not optional—it fundamentally determines whether domain integration produces interpretable results
3. Transport integration is most consequential for peripheral areas appearing non-deprived on traditional measures but experiencing severe transport poverty
4. The approach provides proof-of-concept for enhanced deprivation measurement ahead of the October 2025 IMD update

## Limitations

- Focus on peak/off-peak hours may underestimate evening/weekend service importance
- Heavy rail services excluded due to data availability
- Walking distances use Euclidean calculations without street network constraints
- Analysis based on 10 case studies; full implementation requires national-scale recalculation
- Establishes correlation, not causation, between transport and deprivation

## Reproducibility

All file paths use the `here` package. Place data files in `raw_data/` folder. Processed outputs save automatically to `data_processed/` and `outputs/` directories. Scripts execute sequentially (00-07).

## Citation

Methodology follows exponential transformation approach from:
> MHCLG (2019). *English indices of deprivation 2019: technical report*. Ministry of Housing, Communities and Local Government.
