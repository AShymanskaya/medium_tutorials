library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(gganimate)
library(gifski)
library(transformr)
library(RColorBrewer)
library(magick)
library(httr)
library(ggplot2)
library(elevatr)
library(terra)
library(rayshader)
library(rayrender)
library(dplyr)
library(tidyr)
library(readr)
library(viridis)


# 2. DEFINE HISTORICAL PERIODS
#-----------------------------
historical_periods <- list(
  list(-1800, -1200, "Bronze Age"),
  list(-1199, -800, "Early Iron Age"),
  list(-799, -500, "Archaic Period"),
  list(-499, -323, "Classical Period"),
  list(-322, -31, "Hellenistic Period"),
  list(-30, 284, 'Pax Romana'),
  list(285, 476, 'Roman Dominate'),
  list(477, 1000, "Early Middle Ages"),
  list(1001, 1500, "High-Late Middle Ages")
)

# 3. LOAD AND PREPARE DATA
#-------------------------
extracted_df <- read_csv('shipwrecks_with_historical_regions.csv')
all_shipwreck_locations <- extracted_df %>%
  select(start_year,end_year, Latitude, Longitude, historical_region)

# 4. GET RIVER DATA
#------------------
get_river_data <- function() {
  url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_eu_shp.zip"
  res <- httr::GET(
    url,
    write_disk("eu_rivers.zip"),
    progress()
  )
  unzip("eu_rivers.zip")
  filenames <- list.files("HydroRIVERS_v10_eu_shp",
                          pattern = "*.shp", full.names = T
  )
  
  list_riv <- lapply(filenames, sf::st_read)
  eu_riv <- list_riv[[1]] |>
    sf::st_cast("MULTILINESTRING")
  
  eu_riv_width <- eu_riv |>
    dplyr::filter(ORD_FLOW <= 4) |>  # Keep only the largest rivers (adjust this value as needed)
    dplyr::mutate(
      width = as.numeric(ORD_FLOW),
      width = dplyr::case_when(
        width == 3 ~ 1,
        width == 4 ~ 0.8,
        width == 5 ~ 0.6,
        TRUE ~ 0.4
      )
    ) |>
    sf::st_as_sf()
  
  return(eu_riv_width)
}

eu_rivers <- get_river_data()
world <- ne_countries(scale = "medium", returnclass = "sf")
lookup_table <- c("Bronze Age" = "Bronze Age (1800BCE, 1200BCE)", 
                  "Early Iron Age" = "Early Iron Age (1200BCE, 800BCE)",
                  "Archaic Period" = "Archaic Period (800BCE, 500BCE)",
                  "Classical Period" = "Classical Period (500BCE, 323BCE)",
                  "Hellenistic Period" = "Hellenistic Period (323BCE, 31BCE)",
                  "Pax Romana" = "Pax Romana (31BCE, 284CE)",
                  "Roman Dominate" = "Roman Dominate (284CE, 476CE)",
                  "Early Middle Ages" = "Early Middle Ages (476CE, 1000CE)",
                  "High-Late Middle Ages" = "High-Late Middle Ages (1000CE, 1500CE)"
)
period_order <-  c("Bronze Age (1800BCE, 1200BCE)", 
                   "Early Iron Age (1200BCE, 800BCE)",
                   "Archaic Period (800BCE, 500BCE)",
                   "Classical Period (500BCE, 323BCE)",
                   "Hellenistic Period (323BCE, 31BCE)",
                   "Pax Romana (31BCE, 284CE)",
                   "Roman Dominate (284CE, 476CE)",
                   "Early Middle Ages (476CE, 1000CE)",
                   "High-Late Middle Ages (1000CE, 1500CE)")

generate_distinct_colors <- function(n) {
  colors <- c(
    "#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF",
    "#800000", "#008000", "#000080", "#808000", "#800080", "#008080",
    "#FFA500", "#FFC0CB", "#7FFFD4", "#A52A2A", "#DEB887", "#5F9EA0",
    "#7FFF00", "#D2691E", "#FF7F50", "#6495ED", "#DC143C", "#00008B"
  )
  
  if (n > length(colors)) {
    additional_colors <- colorRampPalette(colors)(n - length(colors))
    colors <- c(colors, additional_colors)
  }
  
  return(colors[1:n])
}
all_regions <- unique(all_shipwreck_locations$historical_region)
region_colors <- generate_distinct_colors(length(all_regions))
names(region_colors) <- all_regions


# Create cumulative data for each period
animation_data <- map_df(historical_periods, function(period) {
  start_year <- period[[1]]
  end_year <- period[[2]]
  period_name <- period[[3]]
  
  df_period <- all_shipwreck_locations %>%
    filter(start_year >= period[[1]], start_year <= period[[2]]) %>%
    mutate(
      period = period_name,
      period_end = end_year,
      period_label = sprintf("%s\n(%s to %s)",
                             period_name,
                             ifelse(start_year < 0, paste0(abs(start_year), " BCE"), paste0(start_year, " CE")),
                             ifelse(end_year < 0, paste0(abs(end_year), " BCE"), paste0(end_year, " CE"))),
      region_color = region_colors[historical_region]
    )
  
  return(df_period)
})

# Create a factor with levels in the order of historical_periods

animation_data$period_label <- factor(lookup_table[animation_data$period], levels = period_order)

# Create a large color palette avoiding blue
color_palettes <- c(
  brewer.pal(8, "Set2"),
  brewer.pal(8, "Set1")[-6],  # Removing blue
  brewer.pal(8, "Dark2"),
  brewer.pal(8, "Accent")[-4],  # Removing blue
  brewer.pal(8, "Paired")[-c(1,2,7,8)],  # Removing blues
  brewer.pal(11, "Spectral")[-c(8,9,10)],  # Removing blues
  brewer.pal(11, "PiYG"),
  brewer.pal(11, "PRGn"),
  brewer.pal(11, "RdYlBu")[-c(9,10,11)]  # Removing blues
)

# Remove any remaining blue-ish colors
color_palettes <- color_palettes[!sapply(color_palettes, function(x) {
  rgb_values <- col2rgb(x)
  rgb_values[3] > rgb_values[1] && rgb_values[3] > rgb_values[2]
})]

# Ensure we have at least 66 colors
while(length(color_palettes) < 66) {
  color_palettes <- c(color_palettes, colorRampPalette(color_palettes)(66 - length(color_palettes)))
}

# Limit to 66 colors
color_palettes <- color_palettes[1:66]
periods_regions <- animation_data %>%
  group_by(period_label) %>%
  summarise(regions = list(unique(historical_region))) %>%
  deframe()


# Find the epoch with the most regions
max_regions <- max(sapply(periods_regions, length))
max_regions_period <- names(which.max(sapply(periods_regions, length)))

# Set fixed coordinates for all plots
fixed_xlim <- range(animation_data$Longitude) + c(-1, 1)
fixed_ylim <- range(animation_data$Latitude) + c(-1, 1)

# Assign colors and shapes to each unique region
all_regions <- unique(animation_data$historical_region)
n_regions <- length(all_regions)

region_colors <- setNames(color_palettes[1:n_regions], all_regions)

# Function to bin shipwrecks into 200km² areas
bin_shipwrecks <- function(data, bin_size = 200) {
  bin_size_deg <- bin_size / 111
  data %>%
    mutate(
      bin_lon = floor(Longitude / bin_size_deg) * bin_size_deg,
      bin_lat = floor(Latitude / bin_size_deg) * bin_size_deg
    ) %>%
    group_by(bin_lon, bin_lat, historical_region) %>%
    summarise(
      count = n(),
      Longitude = mean(Longitude),
      Latitude = mean(Latitude),
      .groups = 'drop'
    )
}

# Set fixed breaks and labels for all periods
size_breaks <- c(1,2, 5, 10, 20)
size_labels <- c("1", "2","5", "10", "20+")

# Find the maximum count across all periods
max_overall_count <- max(map_dbl(periods_regions, function(period) {
  period_data <- animation_data[animation_data$period_label == period,]
  binned_data <- bin_shipwrecks(period_data)
  max(binned_data$count)
}))

# Create a plot for each period
plot_list <- list()
for (period in names(periods_regions)) {
  period_data <- animation_data[animation_data$period_label == period,]
  current_regions <- periods_regions[[period]]
  
  # Bin the shipwrecks
  binned_data <- bin_shipwrecks(period_data)
  
  p <- ggplot() +
    geom_sf(data = world, fill = "#153041", color = "#00509E", linewidth = 0.3) +
    geom_sf(data = eu_rivers, aes(size = width), color = "#4292c6", alpha = 0.9, show.legend = FALSE) +
    geom_point(data = binned_data,
               aes(x = Longitude, y = Latitude, color = historical_region, 
                   size = pmin(count, max(size_breaks))),
               alpha = 0.8) +
    scale_color_manual(values = region_colors, breaks = current_regions) +
    scale_size_continuous(range = c(4, 10),  # Increased the minimum size to make smallest dots larger
                          breaks = size_breaks,
                          labels = size_labels,
                          limits = c(1, max(size_breaks))) +
    coord_sf(xlim = fixed_xlim, ylim = fixed_ylim, expand = FALSE) +
    labs(title = period) +
    theme_void() +
    theme(plot.title = element_text(color = "#F7F7F7", hjust = 0.5, size = 12, margin = margin(t = 10, r = 0, b = 5, l = 0)),
          plot.background = element_rect(fill = "#0f2231"),
          legend.position = "bottom",
          legend.background = element_rect(fill = "#0f2231", color = NA),
          legend.text = element_text(color = "#F7F7F7", size = 8),
          legend.key = element_rect(fill = "#0f2231"),
          legend.key.size = unit(0.5, "cm"),
          legend.spacing.x = unit(0.05, "cm"),
          legend.spacing.y = unit(0.05, "cm"),
          legend.margin = margin(1, 0, 1, 0),
          plot.margin = margin(2, 0, 2, 0),
          legend.box.margin = margin(0, 0, 0, 0),
          legend.box = "vertical",
          legend.title = element_text(color = "#F7F7F7", size = 10)) +
    guides(color = guide_legend(title = "Historical Region",
                                nrow = 10,
                                byrow = TRUE,
                                keywidth = unit(0.5, "cm"),  # Increased size of color legend dots
                                keyheight = unit(0.5, "cm"),  # Increased size of color legend dots
                                label.position = "right",
                                title.position = "top",
                                ncol = 2,
                                label.hjust = 0,
                                label.vjust = 0.5,
                                override.aes = list(size = 3)),
           size = guide_legend(title = "Shipwrecks per 200 km²",
                               nrow = 1,
                               byrow = TRUE,
                               label.position = "bottom",
                               title.position = "top",
                               ncol = 5,
                               label.hjust = 0.5,
                               label.vjust = 1,
                               order = 1))  # This puts the size legend above the color legend
  
  plot_list[[period]] <- p
}

# Create a function to save individual frames
save_plot <- function(plot, filename) {
  ggsave(filename, plot, width = 6, height = 10, units = "in", dpi = 300)
}

# Save individual frames
dir.create("period_maps/frames", showWarnings = FALSE)
for (i in seq_along(plot_list)) {
  save_plot(plot_list[[i]], sprintf("period_maps/frames/frame_%03d.png", i))
}


# Create faded transition frames
transition_frames <- 0  # Number of transition frames between each period
all_frames <- list()
for (i in 1:(length(plot_list) - 1)) {
  from_file <- sprintf("period_maps/frames/frame_%03d.png", i)
  to_file <- sprintf("period_maps/frames/frame_%03d.png", i + 1)
  
  faded_frames <- create_faded_frames(from_file, to_file, transition_frames)
  
  all_frames <- c(all_frames,
                  list(image_read(from_file)),
                  as.list(faded_frames))
}

# Add the last frame
all_frames <- c(all_frames, list(image_read(sprintf("period_maps/frames/frame_%03d.png", length(plot_list)))))

# Combine all frames into a single animation
animation <- image_join(all_frames)

# Write the animated GIF
image_write_gif(animation, "period_maps/Shipwrecks_All_Regions_Animated_Faded_Mobile.gif", delay = 1)


# Read the CSV data
data <- read_csv("shipwrecks_with_historical_regions.csv")

# Function to assign period based on start_year
assign_period <- function(year) {
  for (period in historical_periods) {
    if (year >= period[[1]] && year <= period[[2]]) {
      return(period[[3]])
    }
  }
  return(NA)
}

# Assign periods to the data
data$assigned_period <- sapply(data$start_year, assign_period)

# Count shipwrecks by assigned period and historical region
shipwreck_counts <- data %>%
  filter(!is.na(assigned_period)) %>%
  group_by(assigned_period, historical_region) %>%
  summarise(count = n()) %>%
  ungroup()

# Calculate total shipwrecks per region
region_totals <- shipwreck_counts %>%
  group_by(historical_region) %>%
  summarise(total = sum(count)) %>%
  filter(total >= 20)

# Filter the shipwreck_counts to include only regions with 20+ shipwrecks
shipwreck_counts_filtered <- shipwreck_counts %>%
  filter(historical_region %in% region_totals$historical_region)

# Spread the filtered data to wide format
shipwreck_counts_wide <- shipwreck_counts_filtered %>%
  spread(key = historical_region, value = count, fill = 0)

# Define the order of periods
period_order <- sapply(historical_periods, function(x) x[[3]])

# Reorder the data based on the defined period order
shipwreck_counts_wide <- shipwreck_counts_wide %>%
  mutate(assigned_period = factor(assigned_period, levels = period_order)) %>%
  arrange(assigned_period)

# Prepare data for ggplot
plot_data <- shipwreck_counts_wide %>%
  gather(key = "region", value = "count", -assigned_period)

# Create the stacked area plot optimized for mobile
# Create the stacked area plot optimized for mobile
p <- ggplot(plot_data, aes(x = assigned_period, y = count, fill = region, group = region)) +
  geom_area(position = "stack") +
  scale_fill_viridis_d() +
  scale_y_continuous(expand = c(0, 0), labels = scales::comma) +
  labs(title = "Shipwrecks by Historical Period and Region",
       subtitle = "(20+ shipwrecks)",
       x = NULL,
       y = "Number of Shipwrecks",
       fill = "Historical Region") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

# Try to save as SVG, if fails, save as PNG
tryCatch({
  ggsave("shipwrecks_by_historical_period_and_region_mobile.svg", plot = p, width = 9, height = 6, dpi = 300)
}, error = function(e) {
  message("Failed to save as SVG. Saving as PNG instead.")
  ggsave("shipwrecks_by_historical_period_and_region_mobile.png", plot = p, width = 9, height = 6, dpi = 300)
})

# Display the plot
print(p)
