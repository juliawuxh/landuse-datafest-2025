library(sf)
library(dplyr)
library(stringr)
library(data.table)

big_data <- fread("super_merged_data.csv")

# List of columns to normalize
cols_to_normalize <- c(
  "avg_dust_column_mass_avg",
  "avg_sulfur_dioxide_column_mass_avg",
  "avg_sulfate_column_mass_avg",
  "avg_black_carbon_optical_depth_avg",
  "avg_dust_optical_depth_avg",
  "avg_organic_carbon_optical_depth_avg",
  "avg_dust_column_mass_2022_avg",
  "avg_sulfur_dioxide_column_mass_2022_avg",
  "avg_sulfate_column_mass_2022_avg",
  "avg_black_carbon_optical_depth_2022_avg",
  "avg_dust_optical_depth_2022_avg",
  "avg_organic_carbon_optical_depth_2022_avg"
)

# Normalize and create new columns
big_data <- big_data %>%
  mutate(across(
    all_of(cols_to_normalize),
    ~ .x / ppltn_t,
    .names = "{.col}_normalized"
  ))
