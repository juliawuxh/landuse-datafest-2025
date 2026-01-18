# not using

# Convert grid data to spatial points
grid_data <- fread("d.csv")
grid_sf <- st_as_sf(grid_data, coords = c("x_map", "y_map"), crs = 3035)

# Perform the spatial join to assign AGS
grid_with_mun <- st_join(grid_sf, municipal_geom, left = TRUE)

# Join to census data
final_municipal <- municipal_data %>%
  left_join(grid_with_mun, by = "AGS") 

grid_summary_by_muni <- final_municipal %>%
  st_drop_geometry() %>%
  group_by(AGS) %>%
  summarise(
    mean_livespace = mean(livespace_pp_2022, na.rm = TRUE),
    mean_vacancy = mean(vac_2022, na.rm = TRUE),
    total_income = sum(Total_income_EUR_2020, na.rm = TRUE),
    total_pop = sum(Total_POP_2022, na.rm = TRUE),
    income_per_capita = ifelse(total_pop > 0, total_income / total_pop, NA_real_),
    n_cells = n(),
    .groups = "drop"
  )

final_municipal_sf <- municipal_geom %>%
  left_join(final_municipal, by = "AGS") %>%
  st_as_sf()

# Create derived metric
final_municipal_sf <- final_municipal_sf %>%
  mutate(
    income_per_capita = Total_income_EUR_2020 / Total_POP_2022
  )

# Check what grid cell data points are available
#-------------------------------------------------------------------------------
mun_with_grids <- grid_with_mun %>%
  st_drop_geometry() %>%
  count(AGS) %>%
  mutate(has_grids = TRUE)

coverage_map <- municipal_geom %>%
  left_join(mun_with_grids, by = "AGS") %>%
  mutate(has_grids = ifelse(is.na(has_grids), FALSE, has_grids))

ggplot(coverage_map) +
  geom_sf(aes(fill = has_grids), color = NA) +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "grey80")) +
  theme_minimal() +
  labs(title = "Municipalities That Received Grid Cell Data")
#-------------------------------------------------------------------------------


final_municipal_sf_space <- final_municipal_sf %>%
  filter(!is.na(livespace_pp_2022)) %>%
  mutate(mean_livespace_capped = pmin(mean_livespace, 80))

final_municipal_sf_clean <- final_municipal_sf %>%
  filter(!is.na(mean_livespace))  # or mean_vacancy

grid_summary_by_muni <- grid_with_mun %>%
  group_by(AGS, GEN) %>%
  summarise(
    mean_livespace = if (all(is.na(livespace_pp_2022))) NA else mean(livespace_pp_2022, na.rm = TRUE)
  )

final_municipal_sf <- final_municipal_sf %>%
  mutate(mean_livespace = replace_na(mean_livespace, 0))

ggplot(final_municipal_sf %>% mutate(has_data = !is.na(mean_livespace))) +
  geom_sf(aes(fill = has_data), color = NA) +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "grey80")) +
  theme_minimal() +
  labs(title = "Municipalities with Valid Living Space Data", fill = "Has Data?")

# Compare how many municipalities are in each dataset
length(unique(municipal_geom$AGS))               # Total municipalities (probably ~11,000)
length(unique(grid_with_mun$AGS))                # Municipalities that received grid cells



# Vacancy Rate (mean across grid cells)
# Filter only grid cells that have vacancy data
grid_vacant <- grid_with_mun %>%
  filter(!is.na(vac_2022))


ggplot() +
  geom_sf(data = municipal_geom, fill = "grey95", color = "white", size = 0.1) +
  geom_sf(data = grid_vacant, aes(color = vac_2022), size = 0.3, alpha = 0.7) +
  scale_color_viridis_c(option = "magma") +
  theme_minimal() +
  labs(
    title = "Grid Cells with Vacancy Data (2022)",
    color = "Vacancy (%)"
  )



final_municipal_sf <- final_municipal_sf %>%
  mutate(income_per_capita = Total_income_EUR_2020 / Total_POP_2022)

# Total income
ggplot(final_municipal_sf) +
  geom_sf(aes(fill = income_per_capita), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "cividis", trans = "log", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Income per Capita by Municipality (2020)",
    fill = "€ per person (log)"
  )

library(tidyr)

# Gather variables for faceted plot
plot_data <- final_municipal_sf %>%
  select(geometry, mean_livespace, mean_vacancy, Total_income_EUR_2020) %>%
  pivot_longer(
    cols = c(mean_livespace, mean_vacancy, Total_income_EUR_2020),
    names_to = "variable", values_to = "value"
  )

ggplot(plot_data) +
  geom_sf(aes(fill = value), color = NA) +
  facet_wrap(~ variable, scales = "free") +
  scale_fill_viridis_c(option = "inferno", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Municipality-Level Indicators: Living Space, Vacancy, and Income",
    fill = "Value"
  )





#Load Berlin's Boundary Geometry
admin_gdf <- st_read("DE_VG250.gpkg", layer = "vg250_gem")
berlin_geom <- admin_gdf %>% filter(GEN == "Berlin")
berlin_geom_re <- st_transform(berlin_geom, crs = 3035)

#Get Grid Cells Inside Berlin
berlin_grids <- st_intersection(grid_sf, berlin_geom_re)
nrow(berlin_grids)  # How many grid points are inside Berlin?

#Check and Plot the Geometry
plot(st_geometry(berlin_geom_re), col = "lightgrey", main = "Berlin Grid Cells")
plot(st_geometry(berlin_grids), add = TRUE, col = "blue", pch = 16, cex = 0.5)

berlin_summary <- berlin_grids %>%
  summarise(
    total_pop_2005 = sum(pop_05, na.rm = TRUE),
    total_pop_2020 = sum(pop_20, na.rm = TRUE),
    avg_lupp_change = mean(lupp20 - lupp05, na.rm = TRUE),
    avg_livespace = mean(livespace_pp_2022, na.rm = TRUE)
  )

ggplot(berlin_grids) +
  geom_sf(aes(color = livespace_pp_2022), size = 0.8) +
  scale_color_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(title = "Living Space per Person in Berlin (2022)",
       color = "m² / person")

foreigners_data <- read_excel("12521-0041_en.xlsx")
View(foreigners_data)


grid_summary_table <- grid_with_mun %>%
  st_drop_geometry() %>%
  group_by(AGS, GEN) %>%
  summarise(
    mean_livespace = mean(livespace_pp_2022, na.rm = TRUE),
    n_cells = n(),
    .groups = "drop"
  )

# View the first few rows
head(grid_summary_table)

# Count how many municipalities have valid living space data
table(!is.na(grid_summary_table$mean_livespace))

# How many unique municipalities total?
n_distinct(grid_summary_table$AGS)





