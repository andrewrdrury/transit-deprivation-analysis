# Transport Accessibility and Deprivation Analysis Report

## Executive Summary

- **Total Case Studies Analyzed**: 10
- **GTFS-GMAL Correlation**: 0.779
- **Mean Rank Change**: 0 positions
- **Significant Changes**: 1 out of 10 cases
- **Maximum Rank Improvement**: 1 positions
- **Maximum Rank Worsening**: 6 positions
- **Mean Method Difference**: 1.2 positions
- **Maximum Method Difference**: 6 positions

## Methodological Approach

This analysis uses **exponential transformation** following the official IMD 2019 methodology. This approach prevents cancellation effects where high deprivation in one domain is offset by low deprivation in another.

### Exponential vs Simple Method Comparison

The mean difference between exponential transformation and simple weighted sum methods was 1.2 positions, with a maximum difference of 6 positions. This demonstrates the importance of using the methodologically correct exponential transformation.

## Key Findings

1. **Transport-Deprivation Relationship**: The correlation between GTFS-based and GMAL-based accessibility measures was 0.779, indicating strong agreement between methodologies.

2. **IMD Impact**: Including transport accessibility as a domain resulted in rank changes for 10% of case study areas.

3. **Domain Integration**: The transport accessibility domain showed varying correlations with existing IMD domains, with correlations detailed in the domain correlation matrix.

4. **Methodological Validation**: The exponential transformation method produces results that are methodologically consistent with official IMD calculations, with meaningful differences from simple weighted sum approaches.

## Methodology Validation

- GTFS-based accessibility measures were validated against existing GMAL scores
- Exponential transformation was validated to ensure proper implementation
- Sensitivity analysis tested alternative weighting schemes
- Method comparison demonstrated the importance of exponential transformation
- Case study selection ensured representation across deprivation-accessibility combinations

## Data Sources

- IMD 2019 (Consumer Data Research Centre)
- Greater Manchester Access Levels 2019 (data.gov.uk)
- GTFS Data 2019 (Transport for Greater Manchester)

## Outputs Generated

- Modified IMD scores with transport accessibility integration using exponential transformation
- Comparative analysis of exponential vs simple weighted sum approaches
- Sensitivity analysis with different weighting schemes
- Spatial maps showing rank changes and accessibility levels
- Correlation analysis between transport and other deprivation domains
- Method comparison analysis demonstrating exponential transformation impact


