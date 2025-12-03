#' Map Coverage Effort
#'
#' Maps the survey effort (hours) for a specific country using SABAP2 data.
#' It automatically handles ADM1 boundaries and dynamic coloring based on effort hours.
#'
#' @param country A character string representing the country name (e.g., "Nigeria").
#' @param scale Scale of the map boundaries, default is "large".
#' @return Generates a PNG file in the working directory and prints the map.
#' @import ggplot2
#' @import sf
#' @import dplyr
#' @importFrom ggspatial annotation_scale annotation_north_arrow
#' @importFrom rnaturalearth ne_states ne_countries
#' @export
map_coverage <- function(country, scale = "large") {

  # Sanitize country name for API call
  sanitized_country <- gsub(" ", "", country, fixed = TRUE)

  # --- 1. SETUP AND DATA ACQUISITION ---

  tryCatch({
    coverage_data <- suppressMessages(sf::st_read(
      paste0('https://api.birdmap.africa/sabap2/v2/coverage/country/', sanitized_country, '?format=geoJSON')
    ))
  }, error = function(e) {
    stop(paste("Failed to retrieve coverage data for", country, ". Error:", e$message))
  })

  if (nrow(coverage_data) == 0) {
    stop(paste("No coverage data found for country:", country))
  }

  # --- ADM1 BOUNDARY LOADING AND FALLBACK ---
  adm1_boundaries <- rnaturalearth::ne_states(country = country, returnclass = "sf")

  if (nrow(adm1_boundaries) == 0) {
    warning(paste0("ADM1 boundaries not found for '", country, "'. Falling back to ADM0 (country outline only)."))

    africa_boundary <- rnaturalearth::ne_countries(scale = scale, returnclass = "sf")
    geoboundary_country <- africa_boundary %>%
      dplyr::filter(name == country)

    if (nrow(geoboundary_country) == 0) {
      stop(paste0("ADM0 boundary for '", country, "' also not found."))
    }
  } else {
    geoboundary_country <- adm1_boundaries
  }

  # Filter out Adhoc pentads (0 cards)
  coverage_data <- coverage_data %>%
    dplyr::filter(full.protocol_total_hours >= 0) # Keep all for hours calculation

  if (nrow(coverage_data) == 0) {
    warning(paste("No data found for", country, ". Plotting only boundaries."))
  }

  # --- 2. DYNAMIC CATEGORIZATION SCHEME (TOTAL HOURS) ---

  # Use the column full.protocol_total_hours
  data_column <- coverage_data$full.protocol_total_hours
  max_protocol <- max(data_column, na.rm = TRUE)

  # Define the 10-category scheme (1 Adhoc + 9 colored bins)
  full_breaks <- c(-Inf, 2, 4, 8, 12, 26, 51, 101, 1000, Inf)
  full_labels <- c("Ad-hoc (0-1 hrs)", "2-3", "4-7", "8-11", "12-25", "26-50", "51-100", "101-999", "1000+")
  full_colors <- c("#B2B1AF", "#FEF67E", "#FFBF00", "#FF9F00", "#FF7F00", "#FF0000", "#D00062", "#97216B", "#75214B", "#4A002F")

  # Conditional Selection (9 or 10 categories total)
  if (max_protocol >= 1000) {
    # SCENARIO A: 10 categories (Adhoc to 1000+).
    breaks_final <- full_breaks
    labels_final <- full_labels
    colors_final <- full_colors
  } else {
    # SCENARIO B: 9 categories (Adhoc to 100+).
    breaks_final <- full_breaks[1:9] # Stops at 1000
    labels_final <- full_labels[1:8] # Stops at 101-999
    labels_final[9] <- "100+" # Rename the last category to 100+
    colors_final <- full_colors[1:9]
  }

  # Apply the cut() logic
  coverage_data$full.protocol.factor <- cut(
    data_column,
    breaks = breaks_final,
    labels = labels_final,
    include.lowest = TRUE,
    right = FALSE
  )

  # --- 3. DYNAMIC PLOT SETUP ---

  # Calculate dynamic plot dimensions
  bbox <- sf::st_bbox(geoboundary_country)
  width_geo <- bbox["xmax"] - bbox["xmin"]
  height_geo <- bbox["ymax"] - bbox["ymin"]
  asp_ratio <- height_geo / width_geo

  # Custom color scale
  custom_color_scale <- scale_fill_manual(
    name = "Total Hours",
    values = colors_final,
    labels = labels_final,
    na.value = "transparent"
  )

  # Copyright Caption
  current_year <- format(Sys.Date(), "%Y")
  caption_text <- paste0("© ", current_year, " ", country, " Bird Atlas,\n a member of the African Bird Atlas Project")

  # --- 4. PLOT GENERATION ---

  map <- ggplot() +
    # Plot country outline and ADM1 boundaries
    # *** CRITICAL CHANGE: Lighter color and thinner line for publication ***
    geom_sf(data = geoboundary_country, fill = NA, color = "gray60", linewidth = 0.3) +

    # Plot coverage pentads (using the hours factor)
    geom_sf(data = coverage_data, aes(fill = full.protocol.factor), color = NA) +

    labs(
      title = NULL,
      subtitle = paste("Effort Map for", country),
      caption = caption_text
    ) +
    xlab(NULL) +
    ylab(NULL) +

    # Conditional Coordinate Limits for South Africa
    {
      if (country == "South Africa") {
        coord_sf(xlim = sf::st_bbox(geoboundary_country)[c(1, 3)],
                 ylim = c(-40, -20))
      } else {
        coord_sf(xlim = sf::st_bbox(geoboundary_country)[c(1, 3)],
                 ylim = sf::st_bbox(geoboundary_country)[c(2, 4)])
      }
    } +

    theme_minimal() +
    custom_color_scale +
    theme(
      plot.title = element_text(size = 15, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.title = element_text(size = 10, face = "italic"),
      legend.key.size = unit(0.8, "lines"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      plot.margin = unit(c(1,1,1,1), "cm"),
      panel.border = element_rect(fill = NA, color = "gray70"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "gray50", linewidth = 0.2),
      axis.text = element_text(size = 10, color = "gray20"),
      axis.title = element_text(size = 12, color = "gray20")
    ) +
    annotation_scale(location = "br", width_hint = 0.2) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           height = unit(1.0, "cm"), width = unit(1.0, "cm"),
                           pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"))

  # Display map
  print(map)

  # --- 5. SAVE ---

  fixed_width <- 7.09
  dynamic_height <- fixed_width * asp_ratio + 1.25

  ggsave(
    filename = paste0(sanitized_country, "_Effort_Map.png"),
    plot = map,
    width = fixed_width,
    height = dynamic_height,
    units = "in",
    dpi = 300,
    type = "cairo"
  )
}
