# Load shapefile of administrative districts (Kreise)
municipalities_districts <- st_read('kreis_files')

# Load real migration data from CSV
living_space_data <- read_csv('super_merged_data.csv')

clean_column <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_to_lower()
}

kreis_cleaned <- municipalities_districts %>%
  mutate(across(c(lan_code, lan_name, krs_code, krs_name), clean_column)) %>%
  mutate(krs_code = as.character(krs_code))

kreis_cleaned <- kreis_cleaned |>
  rename(admin_id = krs_code)

shape_merge <- left_join(kreis_cleaned, living_space_data, by = "admin_id")

# Custom Berlin-style palette
berlin_palette <- c("#5e3476", "#993284", "#ca4e79", "#ec7f60", "#f9b44d", "#fff193")

# Living space map with proper log scale and visible legend
base_map <- ggplot() +
  geom_sf(data = shape_merge, aes(fill = avg_livespace_pp_2022), color = '#2a2a2a', size = 0.1) +  
  scale_fill_gradientn(
    colors = berlin_palette,
    trans = "log",  # Apply log scale here
    name = "Average Living Space (Log Scale)",
    breaks = c(10, 100, 1000, 10000),  # No log10() here!
    labels = c("10", "100", "1k", "10k"),
    guide = guide_colorbar(
      title.position = "top",
      barwidth = 0.8,
      barheight = 15,
      ticks.linewidth = 1,
      title.hjust = 0.5
    ),
    na.value = "#1a1a1a"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = 'black', color = NA),
    plot.title = element_text(color = 'white', size = 18, hjust = 0.5, face = "bold"),
    legend.position = "right",
    legend.title = element_text(color = "white", size = 10),
    legend.text = element_text(color = "white", size = 8)
  ) +
  ggtitle('Living Space Across Germany (Artistic Palette)')

# Save the final map
output_file <- '~/Desktop/DataFest2025/Germany_Living_Space_Map.png'
ggsave(output_file, plot = base_map, width = 10, height = 12, dpi = 300)

# Display the map
print(base_map)