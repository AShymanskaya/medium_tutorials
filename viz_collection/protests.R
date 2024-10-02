library(tidyverse)
library(usmap)
library(scales)
library(showtext)
library(cowplot)
library(magick)
library(RColorBrewer)
library(colorspace)
library(sf)
library(ggtext)



# Load custom font
font_add(family = "Chillax", regular = "assets/fonts/Chillax-Light.ttf", bold = "assets/fonts/Chillax-Regular.ttf")
showtext_auto()

# Load and prepare data
state_issues <- read_csv('results_main_issues.csv')

# Define swing states
swing_states <- c('PA', 'NV', 'GA', 'MI', 'NC', 'AZ', 'WI')

# Function to extract main issue
extract_main_issue <- function(issues) {
  main_issue <- strsplit(issues, " / ")[[1]][1]
  return(main_issue)
}

# Prepare data: extract main issues
state_issues <- state_issues %>%
  mutate(main_issue = sapply(main_issue, extract_main_issue))

# Define main issues based on the data
main_issues <- unique(state_issues$main_issue)

# Create a color palette for the main issues
set.seed(123)  # for reproducibility
main_colors <- c(
  brewer.pal(9, "Set1"),
  brewer.pal(8, "Set2"),
  brewer.pal(12, "Set3"),
  brewer.pal(12, "Paired")
)

# Function to check if a color is too close to white
is_too_light <- function(color) {
  rgb <- col2rgb(color)
  luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
  return(luminance > 0.8)
}

# Filter out colors that are too close to white
main_colors <- main_colors[!sapply(main_colors, is_too_light)]

# If we don't have enough colors, generate more
while(length(main_colors) < length(main_issues)) {
  new_color <- colorRampPalette(main_colors)(1)
  if(!is_too_light(new_color)) {
    main_colors <- c(main_colors, new_color)
  }
}

main_colors <- sample(main_colors, length(main_issues))  # Shuffle colors

# Create the color palette
color_palette <- setNames(main_colors, main_issues)

# Function to create exploded map
create_exploded_map <- function(scale_factor = 0.3) {
  # Get the US map data
  us_map <- us_map(regions = "states")
  
  # Convert to sf object
  us_sf <- st_as_sf(us_map)
  
  # Calculate centroids
  centroids <- st_centroid(us_sf)
  
  # Calculate the direction to move each state
  directions <- st_geometry(centroids) - st_geometry(st_centroid(st_union(us_sf)))
  
  # Scale the directions
  scaled_directions <- directions * scale_factor
  
  # Move the states
  exploded_map <- st_geometry(us_sf) + scaled_directions
  
  # Create a new sf object with the exploded geometry
  exploded_sf <- st_set_geometry(us_sf, exploded_map)
  
  return(exploded_sf)
}

# Function to create plot for all states

# Function to create plot for all states
# Function to create plot for all states
create_state_plot <- function(data, year, color_palette, swing_states, exploded_map) {
  year_data <- data %>% filter(year == !!year)
  
  # Count occurrences of each issue in this year
  year_issue_counts <- year_data %>%
    group_by(main_issue) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  # Get the main issue for this year
  main_issue <- year_issue_counts$main_issue[1]
  
  # Prepare data for plotting
  plot_data <- exploded_map %>%
    left_join(year_data, by = c("abbr" = "state")) %>%
    mutate(fill_color = main_issue,
           is_swing = abbr %in% swing_states,
           different_issue = is_swing & (main_issue != main_issue))
  
  # Get the current x-axis and y-axis limits
  x_limits <- st_bbox(plot_data)[c(1,3)]
  y_limits <- st_bbox(plot_data)[c(2,4)]
  
  # Calculate the amount to shift (e.g., 20% of the x-axis range)
  shift_amount <- diff(x_limits) * 0.2
  
  # Prepare swing state information
  swing_state_info <- year_data %>% 
    filter(state %in% swing_states) %>%
    select(state, main_issue) %>%
    mutate(different = main_issue != !!main_issue)
  
  swing_state_text <- swing_state_info %>%
    mutate(state_text = ifelse(different,
                               paste0("<strong>", state, ": ", main_issue, "</strong>"),
                               paste(state, ":", main_issue))) %>%
    pull(state_text) %>%
    paste(collapse = " | ")
  
  p <- ggplot() +
    geom_sf(data = plot_data, aes(fill = fill_color), color = "black", size = 0.3) +
    # Add a thick border for swing states
    geom_sf(data = filter(plot_data, is_swing), 
            fill = NA, 
            color = "red",  # Changed to yellow for better visibility
            size = 2,  # Increased size for thicker border
            alpha = 0.8)+
    geom_sf_text(data = plot_data, aes(label = abbr),
                 size = 18, fontface = "bold", family = "Chillax", color = "black") +
    scale_fill_manual(
      values = color_palette, 
      name = NULL,
      breaks = year_issue_counts$main_issue,
      labels = paste(year_issue_counts$main_issue, " (", year_issue_counts$count, ")", sep = "")
    ) +
    labs(title = "What Americans Cared About?",
         subtitle = paste0("Year ", year, ": ", main_issue)) +
    theme_void() +
    theme(
      text = element_text(family = "Chillax"),
      plot.title = element_text(size = 120, face = "bold", hjust = 1, vjust = 1),
      plot.subtitle = element_text(size = 80, hjust = 1, vjust = 1),
      legend.position = c(1, 0.5),
      legend.justification = c(1, 0.5),
      legend.text = element_text(size = 60),
      legend.title = element_blank(),
      plot.background = element_rect(fill = "#F5F5DC", color = NA),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")  # Adjusted bottom margin
    ) +
    coord_sf(xlim = c(x_limits[1], x_limits[2] + shift_amount),
             ylim = c(y_limits[1] - diff(y_limits) * 0.1, y_limits[2]),
             expand = FALSE) +  # Slightly extended the plot area to the bottom
    geom_richtext(aes(x = mean(x_limits), y = y_limits[1] - diff(y_limits) * 0.05, 
                      label = swing_state_text),
                  size = 16, family = "Chillax", hjust = 0.5, vjust = 1, 
                  lineheight = 1.2, fill = NA, label.color = NA)
  
  return(p)
}
# Create exploded map
exploded_map <- create_exploded_map(scale_factor = 0.3)

# Get unique years
years <- unique(state_issues$year)

# Create and save a plot for each year
for (year in years) {
  plot <- create_state_plot(state_issues, year, color_palette, swing_states, exploded_map)
  
  ggsave(paste0("usa_map_main_issue_", year, ".png"),
         plot = plot, width = 24, height = 12, dpi = 300)  # Increased width to 24
  print(paste("Map for", year, "has been created."))
}

# Create GIF
#png_files <- list.files(pattern = "usa_map_main_issue_\\d+\\.png")
#png_files <- sort(png_files)
#images <- image_read(png_files)
#gif <- image_animate(images, fps = 1)
#image_write(gif, "usa_map_main_issue.gif")
#print("GIF has been created: usa_map_main_issue.gif")

# Print the color palette for reference
#print(color_palette)