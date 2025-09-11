
library(readxl)
library(tidyr)
library(dplyr)

setwd("G:/Shared drives/WSC_Yukon/CEtool_component3/demogData")


### Mortality 
file_path <- "Yukon_NMC_Mortality_23Jan2025.xlsx"

sheets <- excel_sheets(file_path)

morts_long <- map_dfr(
  sheets,
  ~ read_excel(file_path, sheet = .x) %>%
    rename(herd = 1) %>%   # rename first column to 'herd' no matter its name
    pivot_longer(
      -herd,
      names_to = "harvest_year",
      values_to = "morts"
    ) %>%
    mutate(source_sheet = .x)  # track which sheet it came from
)


# Summarize data
morts_summary <- morts_long %>%
  group_by(herd) %>%
  summarise(
    total_morts = sum(morts, na.rm = TRUE),
    years_with_morts = sum(!is.na(morts) & morts > 0))

### Demography
# Read and format data
comp <- read_excel("AllYears_FallComp_NMC_YT_23Jan2025.xlsx") %>%
  mutate(
    # Convert Excel serial to Date
    Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
    year = year(Date),
    calf_cow = (Tot_Calf / Ad_cow) * 100,
    bull_cow = ((Imm_bull + Mat_bull) / Ad_cow) * 100
  ) %>%
  group_by(Herd) %>%
  summarise(
    years_with_counts = n_distinct(year, na.rm = TRUE),
    mean_calf_cow = mean(calf_cow, na.rm = TRUE),
    mean_bull_cow = mean(bull_cow, na.rm = TRUE)
  )


# Mortality location data from cleaned data
morts_lastfix_sf <- dat %>%
  filter(deployment_end_type == "dead") %>%
  group_by(id) %>%
  slice_max(order_by = t_, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  st_as_sf(coords = c("x_", "y_"), crs = 3154) %>%
  st_transform(4326)

# Count mortality points per herd
nmcherds <- nmcherds %>% st_make_valid()
morts_counts <- st_join(
  morts_lastfix_sf,
  nmcherds %>% select(HERD)   # just need HERD for join
) %>%
  st_drop_geometry() %>%
  count(HERD, name = "mortality_points")


### Join to herd data
# 1. Identify herds with duplicates (excluding NA)
dupe_herds <- nmcherds %>%
  st_drop_geometry() %>%
  filter(!is.na(HERD)) %>%
  count(HERD) %>%
  filter(n > 1) %>%
  pull(HERD)

# 2. Join your data first
nmcherds_joined <- nmcherds %>%
  left_join(comp,  by = c("HERD" = "Herd")) %>%
  left_join(morts_summary, by = c("HERD" = "herd")) %>%
  left_join(morts_counts,  by = "HERD") %>%
  mutate(mortality_points = replace_na(mortality_points, 0))

# 3. Merge only the duplicate named herds
merged_named <- nmcherds_joined %>%
  filter(HERD %in% dupe_herds) %>%
  group_by(HERD) %>%
  summarise(
    years_with_counts = max(years_with_counts, na.rm = TRUE),
    total_morts       = sum(total_morts, na.rm = TRUE) + sum(mortality_points, na.rm = TRUE),
    mean_calf_cow     = mean(mean_calf_cow, na.rm = TRUE),
    mean_bull_cow     = mean(mean_bull_cow, na.rm = TRUE),
    years_with_morts  = max(years_with_morts, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Keep all other herds (singletons + NAs) as they are
others <- nmcherds_joined %>%
  filter(!(HERD %in% dupe_herds)) %>%
  mutate(
    years_with_counts = ifelse(is.na(years_with_counts), NA_real_, years_with_counts),
    total_morts       = total_morts + mortality_points
  )

# 5. Combine back together
nmcherds_demo <- bind_rows(merged_named, others)




### Plot
library(patchwork)  # for side-by-side layout

# Panel 1: coloured herds + mortality labels
# Panel 1
p1 <- ggplot() +
  geom_sf(data = ytbc.wgs, fill = "gray95", color = "black") +
  geom_sf(data = nmcherds_demo,
          aes(fill = years_with_counts, color = years_with_counts),
          alpha = 0.5) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "white",
    guide = guide_colourbar(
      direction = "horizontal",
      order = 1,
      barheight = unit(0.3, "cm")   # slimmer bar
    )
  ) +
  scale_color_viridis_c(
    option = "plasma",
    na.value = "grey50",
    guide = guide_colourbar(
      direction = "horizontal",
      order = 1,
      barheight = unit(0.3, "cm")   # match fill bar height
    )
  ) +
  geom_point(
    data = data.frame(x = NA, y = NA, type = "n = Number of mortalities"),
    aes(x = x, y = y, shape = type),
    color = "black"
  ) +
  scale_shape_manual(
    values = c("n = Number of mortalities" = NA),
    guide = guide_legend(
      title = NULL,
      order = 2,
      override.aes = list(shape = NA)
    )
  ) +
  geom_text(
    data = st_centroid(nmcherds_demo),
    aes(x = st_coordinates(geometry)[,1],
        y = st_coordinates(geometry)[,2],
        label = total_morts),
    color = "black"
  ) +
  coord_sf(xlim = c(-141, -118), ylim = c(55, 68), expand = FALSE) +
  theme_minimal() +
  labs(
    fill = "Years with counts",
    color = "Years with counts",
    title = "Total Mortalities",
    x = NULL, y = NULL
  ) +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    legend.box = "vertical",
    legend.box.just = "right",
    legend.background = element_rect(fill = alpha("white", 0.8), color = NA),
    legend.spacing.y = unit(0.1, "cm")
  )

# Panel 2
p2 <- ggplot() +
  geom_sf(data = ytbc.wgs, fill = "gray95", color = "black") +
  geom_sf(data = nmcherds_demo,
          aes(fill = years_with_counts, color = years_with_counts),
          alpha = 0.5) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "white",
    guide = guide_colourbar(
      direction = "horizontal",
      order = 1,
      barheight = unit(0.3, "cm")   # slimmer bar
    )
  ) +
  scale_color_viridis_c(
    option = "plasma",
    na.value = "grey50",
    guide = guide_colourbar(
      direction = "horizontal",
      order = 1,
      barheight = unit(0.3, "cm")   # match fill bar height
    )
  ) +
  geom_sf(data = morts_lastfix_sf,
          aes(shape = "Mortality location"),
          color = "black", size = 1, stroke = 1) +
  scale_shape_manual(
    values = c("Mortality location" = 4),
    guide = guide_legend(
      title = NULL,
      order = 2,
      override.aes = list(size = 1, stroke = 1)
    )
  ) +
  coord_sf(xlim = c(-141, -118), ylim = c(55, 68), expand = FALSE) +
  theme_minimal() +
  labs(
    fill = "Years with counts",
    color = "Years with counts",
    title = "Telemetry records of mortality",
    x = NULL, y = NULL
  ) +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    legend.box = "vertical",
    legend.box.just = "right",
    legend.background = element_rect(fill = alpha("white", 0.8), color = NA),
    legend.spacing.y = unit(0.1, "cm")
  )

# Combine side-by-side
map_demo <- p1 + p2
ggsave("map_demo.png", plot = map_demo, width = 12, height = 6, units = "in", dpi = 300, device = "png")

