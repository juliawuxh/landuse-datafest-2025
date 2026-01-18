library(sf)
library(dplyr)
library(stringr)

nasa_data <- fread("nasa_combined_2005_2022.csv")

nasa_sf <- st_as_sf(nasa_data, coords = c("x_map", "y_map"), crs = 3035)

municipal_geom <- st_read("DE_VG250.gpkg", layer = "vg250_gem") %>%
  st_transform(crs = 3035) %>%
  left_join(municipal_data %>% select(AGS, lan_name, east_west), by = "AGS") %>%
  mutate(AGS = as.character(AGS))

nasa_with_mun <- st_join(nasa_sf, municipal_geom[, c("AGS", "lan_name", "east_west")], left = TRUE)

nasa_with_mun <- nasa_with_mun %>%
  mutate(
    AGS = str_pad(AGS, width = 8, pad = "0"),
    admin_id = str_sub(AGS, 1, 5)
  )

nasa_with_mun_district <- nasa_with_mun %>%
  st_drop_geometry() %>%
  select(-AGS, -lan_name, -east_west, -lon, -lat, -OID_) %>%  # Drop non-descriptive vars
  group_by(admin_id) %>%
  summarise(across(
    everything(),
    ~ mean(.x, na.rm = TRUE),
    .names = "avg_{.col}"
  ), .groups = "drop")

district_geom <- municipal_geom %>%
  mutate(admin_id = str_sub(AGS, 1, 5)) %>%
  group_by(admin_id) %>%
  summarise(geometry = st_union(st_geometry(.)), .groups = "drop") %>%
  st_as_sf()

nasa_with_mun_district_sf <- district_geom %>%
  left_join(nasa_with_mun_district, by = "admin_id")

ggplot(nasa_with_mun_district_sf) +
  geom_sf(aes(fill = avg_dust_optical_depth_2022_avg), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Average Dust Optical Depth by District (2022)",
    fill = "Optical Depth"
  )


