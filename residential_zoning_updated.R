setwd("/Users/melodies/Desktop/Policy Lab '25/code")
library(tidyverse)
library(scales)
library(readr)

# Read the data
assessment_and_city_owned_new_data <- read_csv("assessment_and_city_owned_new_data.csv")

# 1. Filter data
residential_analysis_data <- assessment_and_city_owned_new_data %>%
  select(
    # Basic property information
    pin,
    `Community Area Name`,
    `Zoning Classification`,
    `Property Status`,
    `Date of Acquisition`,
    `Date of Disposition`,
    
    # Size information
    `Square Footage - City Estimate`,
    
    # Choose land value: certified_land
    certified_land,
    
    # Economic incentive zones
    tax_tif_district_num,
    econ_qualified_opportunity_zone_num,
    econ_enterprise_zone_num
  )

# 2. Filter for  six target communities and residential zoning
target_communities <- c(
  "ENGLEWOOD", "WEST ENGLEWOOD", "NEW CITY",
  "EAST GARFIELD PARK", "WEST GARFIELD PARK", "NORTH LAWNDALE"
)

residential_lots <- residential_analysis_data %>%
  filter(
    toupper(`Community Area Name`) %in% toupper(target_communities),
    str_detect(`Zoning Classification`, "^R")  # Only classifications starting with R
  )

# 3. Create  variables for analysis
residential_analysis <- residential_lots %>%
  mutate(
    # Create incentive zone indicators
    in_tif = !is.na(tax_tif_district_num),
    in_oz = !is.na(econ_qualified_opportunity_zone_num),
    in_ez = !is.na(econ_enterprise_zone_num),
    
    # Calculate incentive score (1 point for each zone)
    incentive_score = in_tif + in_oz + in_ez,
    
    # Calculate years owned (for properties still owned by city)
    acquisition_date = as.Date(`Date of Acquisition`),
    years_owned = ifelse(
      `Property Status` == "Owned by City",
      as.numeric(difftime(Sys.Date(), acquisition_date, units = "days")) / 365.25,
      NA
    ),
    
    # Use single land value metric
    land_value = certified_land,
    
    # Calculate value per square foot (account for dividing by 0)
    value_per_sqft = case_when(
      is.na(`Square Footage - City Estimate`) ~ NA_real_,
      `Square Footage - City Estimate` <= 0 ~ NA_real_,
      TRUE ~ land_value / `Square Footage - City Estimate`
    )
  )

# 4. Create community-level summary with means and medians
residential_summary <- residential_analysis %>%
  group_by(`Community Area Name`) %>%
  summarise(
    # Counting properties
    `City Owned Residential Properties` = n(),
    
    # Square footage stats - filter out zeros before calculating
    `Square Footage Observations` = sum(!is.na(`Square Footage - City Estimate`) & 
                                          `Square Footage - City Estimate` > 0),
    `Average Square Footage` = mean(`Square Footage - City Estimate`[`Square Footage - City Estimate` > 0], 
                                    na.rm = TRUE),
    `Median Square Footage` = median(`Square Footage - City Estimate`[`Square Footage - City Estimate` > 0], 
                                     na.rm = TRUE),
    `Total Square Footage` = sum(`Square Footage - City Estimate`, na.rm = TRUE),
    
    # Land value stats - filter out zeros before calculating
    `Land Value Observations` = sum(!is.na(land_value) & land_value > 0),
    `Average Land Value` = mean(land_value[land_value > 0], na.rm = TRUE),
    `Median Land Value` = median(land_value[land_value > 0], na.rm = TRUE),
    `Total Land Value` = sum(land_value, na.rm = TRUE),
    
    # Value per square foot stats 
    `Average Value Per Sqft` = mean(value_per_sqft[!is.na(value_per_sqft) & 
                                                     !is.infinite(value_per_sqft) & 
                                                     value_per_sqft > 0], 
                                    na.rm = TRUE),
    `Median Value Per Sqft` = median(value_per_sqft[!is.na(value_per_sqft) & 
                                                      !is.infinite(value_per_sqft) & 
                                                      value_per_sqft > 0], 
                                     na.rm = TRUE),
    
    # Add count of valid value per sqft calculations
    `Valid Value Per Sqft Calculations` = sum(!is.na(value_per_sqft) & 
                                                !is.infinite(value_per_sqft) & 
                                                value_per_sqft > 0),
    
    # Incentive zone counts
    `Number in TIF` = sum(in_tif, na.rm = TRUE),
    `Number in OZ` = sum(in_oz, na.rm = TRUE),
    `Number in EZ` = sum(in_ez, na.rm = TRUE),
    
    # Incentive score
    `Average Property Score` = mean(incentive_score, na.rm = TRUE),
    `Median Property Score` = median(incentive_score, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  rename(`Community Area Region` = `Community Area Name`) %>%
  arrange(desc(`City Owned Residential Properties`))

# Check median values
diagnostic_check <- residential_summary %>%
  select(`Community Area Region`, 
         `Average Value Per Sqft`,
         `Median Value Per Sqft`,
         `Valid Value Per Sqft Calculations`)

# Print diagnostic information
print("Diagnostic Values:")
print(diagnostic_check)

# Replace any remaining NAs with 0 for graphs
residential_summary <- residential_summary %>%
  mutate(across(where(is.numeric), ~ifelse(is.infinite(.) | is.na(.), 0, .)))

# Add Total row sum values
total_row <- residential_summary %>%
  summarise(
    `Community Area Region` = "Total",
    `City Owned Residential Properties` = sum(`City Owned Residential Properties`),
    `Square Footage Observations` = sum(`Square Footage Observations`),
    `Average Square Footage` = mean(`Average Square Footage`),
    `Median Square Footage` = median(`Median Square Footage`),
    `Total Square Footage` = sum(`Total Square Footage`),
    `Land Value Observations` = sum(`Land Value Observations`),
    `Average Land Value` = mean(`Average Land Value`),
    `Median Land Value` = median(`Median Land Value`),
    `Total Land Value` = sum(`Total Land Value`),
    `Average Value Per Sqft` = mean(`Average Value Per Sqft`),
    `Median Value Per Sqft` = median(`Median Value Per Sqft`),
    `Number in TIF` = sum(`Number in TIF`),
    `Number in OZ` = sum(`Number in OZ`),
    `Number in EZ` = sum(`Number in EZ`),
    `Average Property Score` = mean(`Average Property Score`),
    `Median Property Score` = median(`Median Property Score`),
    `Valid Value Per Sqft Calculations` = sum(`Valid Value Per Sqft Calculations`)
  )

# Combine with the summary
residential_summary <- bind_rows(residential_summary, total_row)

# Means and medians comparison plots
means_medians_comparison <- residential_summary %>%
  filter(`Community Area Region` != "Total") %>%
  select(
    `Community Area Region`,
    `Average Land Value`, 
    `Median Land Value`,
    `Average Square Footage`,
    `Median Square Footage`,
    `Average Value Per Sqft`,
    `Median Value Per Sqft`
  ) %>%
  pivot_longer(
    cols = -`Community Area Region`,
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    type = case_when(
      str_detect(metric, "Average") ~ "Mean",
      str_detect(metric, "Median") ~ "Median",
      TRUE ~ NA_character_
    ),
    metric = case_when(
      str_detect(metric, "Land Value") ~ "Land Value",
      str_detect(metric, "Square Footage") ~ "Square Footage",
      str_detect(metric, "Value Per Sqft") ~ "Value Per Sqft",
      TRUE ~ NA_character_
    ),
    # Replace NA and infinite values with 0 for visualization
    value = ifelse(is.infinite(value) | is.na(value), 0, value)
  )

# A comparison plot
means_medians_plot <- ggplot(means_medians_comparison,
                             aes(x = `Community Area Region`, 
                                 y = value, 
                                 fill = type)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9),
           width = 0.8) +
  facet_wrap(~metric, scales = "free_y") +
  scale_fill_manual(values = c("Mean" = "#E41A1C", "Median" = "#377EB8")) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    strip.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Comparison of Means and Medians by Community Area",
    subtitle = "Note: Zero values may indicate missing or unavailable data",
    x = "Community Area",
    y = "Value ($)",
    fill = "Measure"
  )

# Create separate value per sqft plot
value_per_sqft_plot <- means_medians_comparison %>%
  filter(metric == "Value Per Sqft") %>%
  ggplot(aes(x = `Community Area Region`, 
             y = value,
             fill = type)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9),
           width = 0.8) +
  scale_y_continuous(
    labels = scales::dollar_format(),
    name = "Value per Square Foot ($)"
  ) +
  scale_fill_manual(values = c("Mean" = "#E41A1C", "Median" = "#377EB8")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Land Value per Square Foot by Community Area",
    subtitle = "Values shown in dollars per square foot\nNote: Mean and median values are very similar",
    x = "Community Area",
    fill = "Measure"
  ) +
  geom_text(aes(label = scales::dollar(value, accuracy = 0.01)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3)

# Save the plots
ggsave("images/means_medians_comparison_new.png", 
       means_medians_plot, 
       width = 15, 
       height = 8, 
       dpi = 300)

ggsave("images/value_per_sqft_dollars.png", 
       value_per_sqft_plot, 
       width = 12, 
       height = 7, 
       dpi = 300)

# Calculate percentage differences between means and medians
comparison_stats <- residential_summary %>%
  filter(`Community Area Region` != "Total") %>%
  mutate(
    `Land Value % Difference` = (`Average Land Value` - `Median Land Value`) / `Median Land Value` * 100,
    `Square Footage % Difference` = (`Average Square Footage` - `Median Square Footage`) / `Median Square Footage` * 100,
    `Value Per Sqft % Difference` = (`Average Value Per Sqft` - `Median Value Per Sqft`) / `Median Value Per Sqft` * 100
  ) %>%
  select(
    `Community Area Region`,
    `Land Value % Difference`,
    `Square Footage % Difference`,
    `Value Per Sqft % Difference`
  )


# Create comprehensive summary table
comprehensive_summary <- residential_summary %>%
  select(
    `Community Area Region`,
    `City Owned Residential Properties`,
    `Square Footage Observations`,
    `Average Square Footage` = `Average Square Footage`,
    `Total Square Footage` = `Total Square Footage`,
    `Land Value Observations`,
    `Average Land Value`,
    `Total Land Value` = `Total Land Value`,
    `Number in TIF`,
    `Number in OZ`,
    `Number in EZ`,
    `Average Property Score`,
    `Average Value Per Sqft`  
  ) %>%
  # Formatting
  mutate(
    `Average Square Footage` = round(`Average Square Footage`, 2),
    `Total Square Footage` = round(`Total Square Footage`, 2),
    `Average Land Value` = round(`Average Land Value`, 2),  # Keep as numeric for CSV
    `Total Land Value` = round(`Total Land Value`, 2),      # Keep as numeric for CSV
    `Average Value Per Sqft` = round(`Average Value Per Sqft`, 2),
    `Average Property Score` = round(`Average Property Score`, 2)
  )

# Save to csv
write_csv(comprehensive_summary, "output/residential_summary_new.csv")

# Adding $ to summary
comprehensive_summary_formatted <- comprehensive_summary %>%
  mutate(
    `Average Land Value` = scales::dollar(`Average Land Value`),
    `Total Land Value` = scales::dollar(`Total Land Value`),
    `Average Value Per Sqft` = scales::dollar(`Average Value Per Sqft`)
  )

# Print the formatted version to verify
print("Comprehensive Summary Table:")
print(comprehensive_summary_formatted)

# 5. Check data quality for valid values
data_quality <- data.frame(
  Metric = c(
    "Total Residential Properties", 
    "With Square Footage", 
    "With Land Value",
    "With Value Per Square Foot",
    "With Years Owned",
    "With Incentive Score",
    "In TIF District",
    "In Opportunity Zone",
    "In Enterprise Zone"
  ),
  Count = c(
    nrow(residential_analysis),
    sum(!is.na(residential_analysis$`Square Footage - City Estimate`)),
    sum(!is.na(residential_analysis$land_value)),
    sum(!is.na(residential_analysis$value_per_sqft)),
    sum(!is.na(residential_analysis$years_owned)),
    sum(!is.na(residential_analysis$incentive_score)),
    sum(residential_analysis$in_tif, na.rm = TRUE),
    sum(residential_analysis$in_oz, na.rm = TRUE),
    sum(residential_analysis$in_ez, na.rm = TRUE)
  )
)

data_quality$Percentage <- (data_quality$Count / data_quality$Count[1]) * 100




# 6. Visualization: Updated to show both mean and median values
residential_lots_plot <- ggplot(residential_summary %>% 
                                  filter(`Community Area Region` != "Total"), 
                                aes(x = reorder(`Community Area Region`, 
                                                `City Owned Residential Properties`), 
                                    y = `City Owned Residential Properties`)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Residential Vacant Lots by Community Area",
    subtitle = paste0("Total of ", 
                      sum(residential_summary$`City Owned Residential Properties`[
                        residential_summary$`Community Area Region` != "Total"]), 
                      " lots"),
    x = "Community Area",
    y = "Number of Residential Vacant Lots"
  )

# 7. Updated land value visualization to show mean and median
land_value_comparison <- residential_summary %>%
  filter(`Community Area Region` != "Total") %>%
  select(`Community Area Region`, `Average Land Value`, `Median Land Value`) %>%
  pivot_longer(cols = c(`Average Land Value`, `Median Land Value`),
               names_to = "Metric",
               values_to = "Value")

land_value_plot <- ggplot(land_value_comparison, 
                          aes(x = reorder(`Community Area Region`, Value), 
                              y = Value,
                              fill = Metric)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal() +
  labs(
    title = "Land Value Comparison of Residential Vacant Lots",
    subtitle = "Comparing Mean and Median Values",
    x = "Community Area",
    y = "Land Value ($)"
  )


# 8. Visualization: Incentive zones distribution
incentive_zones_data <- residential_summary %>%
  filter(`Community Area Region` != "Total") %>%
  select(`Community Area Region`, `Number in TIF`, `Number in OZ`, `Number in EZ`) %>%
  pivot_longer(
    cols = c(`Number in TIF`, `Number in OZ`, `Number in EZ`),
    names_to = "zone_type",
    values_to = "count"
  ) %>%
  mutate(
    zone_type = case_when(
      zone_type == "Number in TIF" ~ "TIF District",
      zone_type == "Number in OZ" ~ "Opportunity Zone",
      zone_type == "Number in EZ" ~ "Enterprise Zone"
    )
  )

incentive_zones_plot <- ggplot(incentive_zones_data,
                               aes(x = `Community Area Region`, y = count, fill = zone_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Economic Incentive Zones by Community Area",
    subtitle = "Distribution of TIF, Opportunity Zone, and Enterprise Zone properties",
    x = "Community Area",
    y = "Number of Properties",
    fill = "Zone Type"
  )

# 9. Zoning breakdown analysis
zoning_breakdown <- residential_lots %>%
  count(`Community Area Name`, `Zoning Classification`) %>%
  group_by(`Community Area Name`) %>%
  mutate(
    total = sum(n),
    percentage = n / total * 100
  ) %>%
  ungroup()

# Visualize zoning breakdown
zoning_breakdown_plot <- ggplot(zoning_breakdown, 
                                aes(x = `Community Area Name`, y = percentage, fill = `Zoning Classification`)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 10)
  ) +
  labs(
    title = "Residential Zoning Distribution by Community Area",
    subtitle = "Percentage breakdown of residential zoning types",
    x = "Community Area",
    y = "Percentage of Lots",
    fill = "Zoning Classification"
  )


# 10. Updated formatting for the new summary including mean and median
formatted_summary <- residential_summary %>%
  mutate(across(where(is.numeric), ~round(., 2)))

# 11. Save all outputs
# Create directories if they don't exist
dir.create("output", showWarnings = FALSE)
dir.create("images", showWarnings = FALSE)

# Save summaries
write_csv(formatted_summary, "output/residential_summary_combined_metrics.csv")
write_csv(data_quality, "output/data_quality_report.csv")
write_csv(comparison_stats, "output/mean_median_comparison_stats.csv")

# Save plots
ggsave("images/residential_lots.png", residential_lots_plot, 
       width = 10, height = 6, dpi = 300)
ggsave("images/land_value_comparison.png", land_value_plot, 
       width = 10, height = 6, dpi = 300)
ggsave("images/value_per_sqft_comparison.png", value_per_sqft_plot, 
       width = 10, height = 6, dpi = 300)
ggsave("images/means_medians_comparison.png", means_medians_plot, 
       width = 15, height = 8, dpi = 300)

# Print summary to console
print("Analysis Complete - Including Mean and Median Metrics")
print(paste("Total residential lots analyzed:", nrow(residential_analysis)))
print(paste("Data quality metrics saved to:", "output/data_quality_report.csv"))
print(paste("Summary table saved to:", "output/residential_summary_combined_metrics.csv"))
print(paste("Comparison statistics saved to:", "output/mean_median_comparison_stats.csv"))
print(paste("Visualizations saved to:", "images/"))

