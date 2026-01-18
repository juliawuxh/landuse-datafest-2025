library(tidyverse)  
library(here)     
library(lubridate)  
library(stringr)
library(data.table)
library(readr)     # for CSVs
library(sf)        # for spatial data
library(dplyr)     # for wrangling
library(readxl)
library(ggplot2)
library(writexl)

municipal_data <- fread("municipal_main.csv") %>%
  mutate(
    AGS = sprintf("%08d", as.integer(AGS))  # guarantees 8-digit string with leading 0s
  ) %>%
  filter(AGS != "000000NA") %>%
  mutate(
    AGS = as.character(AGS),
    AGS = case_when(
      str_detect(AGS, "e\\+|E\\+") ~ suppressWarnings(format(as.numeric(AGS), scientific = FALSE, trim = TRUE)),
      TRUE ~ AGS
    ),
    AGS = str_pad(AGS, width = 8, pad = "0")
  ) %>%
  mutate(
    admin_id = str_sub(AGS, 1, 5)
  ) %>%
  relocate(admin_id, .after = AGS)

municipal_data <- municipal_data %>%
  mutate(Total_income_EUR_2020 = Total_income_EUR_2020 * 1000)

# Step 1: Calculate district-level mean income (excluding NAs)
income_means <- municipal_data %>%
  group_by(admin_id) %>%
  summarise(
    income_mean = mean(Total_income_EUR_2020, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Join district-level means back to municipal data
municipal_data <- municipal_data %>%
  left_join(income_means, by = "admin_id") %>%
  mutate(
    Total_income_EUR_2020 = if_else(
      is.na(Total_income_EUR_2020),
      income_mean,
      Total_income_EUR_2020
    )
  ) %>%
  select(-income_mean)  # Optional: remove helper column


check_na <- municipal_data %>%
  filter(is.na(Total_income_EUR_2020))
length(unique(check_na$admin_id))#177

what_berlin <- municipal_data %>%
  filter(GEN == "Hannover")
nrow(check_na)#332

# Load municipality geometries (VG250)
municipal_geom <- st_read("DE_VG250.gpkg", layer = "vg250_gem") %>%
  st_transform(crs = 3035) %>%
  mutate(AGS = as.character(AGS)) 

final_municipal_sf <- municipal_geom %>%
  left_join(municipal_data, by = "AGS") %>%
  mutate(
    mean_livespace = livespace_MEAN_grid_2022,
    mean_vacancy = vac_MEAN_grid_2022
  )

final_municipal_sf <- final_municipal_sf %>%
  mutate(
    GEN = coalesce(as.character(GEN.x), as.character(GEN.y)),
    ARS = coalesce(as.character(ARS.x), as.character(ARS.y))
  ) %>%
  select(-GEN.x, -GEN.y, -ARS.x, -ARS.y)

# aggregate the mean per district
# Calculate district-level means
district_summary <- final_municipal_sf %>%
  st_drop_geometry() %>%
  mutate(
    income_per_capita = Total_income_EUR_2020 / Total_POP_2022
  ) %>%
  group_by(admin_id) %>%
  summarise(
    lan_name = first(na.omit(lan_name)),
    east_west = first(na.omit(east_west)),
    mean_livespace_district = mean(mean_livespace, na.rm = TRUE),
    mean_vacancy_district = mean(mean_vacancy, na.rm = TRUE),
    total_population = sum(Total_POP_2022, na.rm = TRUE),
    income_weighted_sum = sum(income_per_capita * Total_POP_2022, na.rm = TRUE),
    mean_income_district = ifelse(total_population > 0, income_weighted_sum / total_population, NA_real_),
    mean_green_area_district = mean(Green_urban_area_2018, na.rm = TRUE),
    .groups = "drop"
  )


write_csv(district_summary, "district_summary.csv")

# Join back to the spatial dataset
final_municipal_sf <- final_municipal_sf %>%
  left_join(district_summary, by = "admin_id") %>%
  mutate(
    east_west = coalesce(east_west.x, east_west.y),
    lan_name = coalesce(lan_name.x, lan_name.y)
  ) %>%
  select(-east_west.x, -east_west.y, -lan_name.x, -lan_name.y)

write_csv(final_municipal_sf, "cleaned_muncipal_2.csv")

# Plot: Living Space
ggplot(final_municipal_sf) +
  geom_sf(aes(fill = mean_livespace_district), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(title = "Avg Living Space per Person by District", fill = "m²/person")

# Plot: Income
ggplot(final_municipal_sf) +
  geom_sf(aes(fill = income_per_capita), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "cividis", trans = "log", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Mean Income per Capita by District", 
    fill = "€ per person (log)")

# Plot: Vacancy
ggplot(final_municipal_sf) +
  geom_sf(aes(fill = mean_vacancy_district), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "cividis", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Mean Vacancy Rate (%) by District (2022)",
    fill = "Vacancy Rate (%)"
  )

library(ggplot2)
library(dplyr)

# Optional: Clean up the variable names for ease
df <- super_merged_data %>%
  filter(!is.na(migrant_share), !is.na(mean_livespace))  # ensure no NA issues

# Plot: Scatter + Linear Fit, faceted by east/west
ggplot(df, aes(x = migrant_share, y = mean_livespace, color = east_west)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~east_west) +
  scale_color_manual(values = c("East" = "#c51b8a", "West" = "#2c7fb8")) +
  theme_minimal() +
  labs(
    title = "Migration Share vs. Average Living Space per Person",
    subtitle = "Comparison between East and West Germany",
    x = "Migrant Share (%)",
    y = "Average Living Space (m² per person)",
    color = "Region"
  )

df <- super_merged_data %>%
  filter(!is.na(migrant_share), !is.na(mean_vacancy))  # or whatever the real name is

ggplot(df, aes(x = migrant_share, y = mean_vacancy, color = east_west)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~east_west) +
  scale_color_manual(values = c("East" = "#c51b8a", "West" = "#2c7fb8")) +
  theme_minimal() +
  labs(
    title = "Migration Share vs. Vacancy Rate",
    subtitle = "Comparison between East and West Germany",
    x = "Migrant Share (%)",
    y = "Vacancy Rate (%)",
    color = "Region"
  )

