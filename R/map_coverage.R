#' @title Generate Atlas Effort (Coverage) Map
#' @description Downloads, processes, and plots pentad-level coverage effort
#' ?? (Total Hours) for a specified country from the African Bird Atlas Project (ABAP) API.
#'
#' @param country A character string specifying the country name (e.g., "South Africa").
#' @param scale A character string indicating the scale of the natural earth boundaries.
#' ?? (e.g., "large", "medium"). Defaults to "large".
#' @param save_dir A character string specifying the directory to save the output PNG file.
#' ?? Defaults to the current working directory.
#' @return The file path to the generated map image (invisibly).
#' @importFrom rnaturalearth ne_states ne_countries
#' @importFrom sf st_read st_bbox st_as_sf
#' @importFrom dplyr filter select mutate
#' @importFrom ggplot2 ggplot aes geom_sf labs coord_sf theme_minimal theme scale_fill_manual unit element_text element_rect element_blank element_line ggsave
#' @importFrom ggspatial annotation_scale annotation_north_arrow
#' @export
map_coverage <- function(country, scale = "large", save_dir = getwd()) {

  # Sanitize country name for API call
  sanitized_country <- gsub(" ", "", country, fixed = TRUE)
  coverage_url <- paste0(.abap_base_url, "coverage/country/", sanitized_country, "?format=geoJSON")

  # --- 1. SETUP AND DATA ACQUISITION ---

  tryCatch({
    coverage_data <- suppressMessages(sf::st_read(coverage_url))
  }, error = function(e) {
    stop(paste("Failed to retrieve coverage data for", country, ". Error:", e$message))
  })

  if (nrow(coverage_data) == 0) {
    stop(paste("No coverage data found for country:", country))
  }

  # --- ADM1 BOUNDARY LOADING AND FALLBACK ---
  adm1_boundaries <- rnaturalearth::ne_states(country = country, returnclass = "sf")

  if (nrow(adm1_boundaries) == 0) {
    base::warning(paste0("ADM1 boundaries not found for '", country, "'. Falling back to ADM0 (country outline only)."))

    africa_boundary <- rnaturalearth::ne_countries(scale = scale, returnclass = "sf")
    geoboundary_country <- africa_boundary |>
      dplyr::filter(.data$name == country)

    if (nrow(geoboundary_country) == 0) {
      stop(paste0("ADM0 boundary for '", country, "' also not found."))
    }
  } else {
    geoboundary_country <- adm1_boundaries
  }

  # Filter out Adhoc pentads (0 cards)
  coverage_data <- coverage_data |>
    dplyr::filter(.data$full.protocol_total_hours >= 0)

  if (nrow(coverage_data) == 0) {
    base::warning(paste("No data found for", country, ". Plotting only boundaries."))
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
    # SCENARIO B: Dynamic categories, max is based on actual data
    max_break_index <- max(which(full_breaks <= max_protocol), 1) + 1

    breaks_final <- full_breaks[1:max_break_index]
    labels_final <- full_labels[1:(max_break_index - 1)]

    # Rename the last category label for cleaner display (e.g., 101-999 becomes 100+)
    if (max_break_index > 2) {
      last_label_index <- length(labels_final)
      if (grepl("101", labels_final[last_label_index])) {
        labels_final[last_label_index] <- "100+"
      } else if (grepl("51", labels_final[last_label_index])) {
        labels_final[last_label_index] <- "50+"
      }
    }
    colors_final <- full_colors[1:length(breaks_final)]
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
  custom_color_scale <- ggplot2::scale_fill_manual(
    name = "Total Hours",
    values = colors_final[1:length(labels_final)], # Ensure color vector matches labels
    labels = labels_final,
    na.value = "transparent"
  )

  # Copyright Caption
  current_year <- base::format(base::Sys.Date(), "%Y")
  caption_text <- paste0("?? ", current_year, " ", country, " Bird Atlas,\n a member of the African Bird Atlas Project")

  # --- 4. PLOT GENERATION ---

  map <- ggplot2::ggplot() +
    # Plot country outline and ADM1 boundaries
    ggplot2::geom_sf(data = geoboundary_country, fill = NA, color = "gray60", linewidth = 0.3) +

    # Plot coverage pentads (using the hours factor)
    ggplot2::geom_sf(data = coverage_data, ggplot2::aes(fill = .data$full.protocol.factor), color = NA) +

    ggplot2::labs(
      title = NULL,
      subtitle = paste("Effort Map for", country),
      caption = caption_text
    ) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +

    # Conditional Coordinate Limits for South Africa
    (
      if (country == "South Africa") {
        ggplot2::coord_sf(xlim = sf::st_bbox(geoboundary_country)[c(1, 3)],
                          ylim = c(-40, -20))
      } else {
        ggplot2::coord_sf(xlim = sf::st_bbox(geoboundary_country)[c(1, 3)],
                          ylim = sf::st_bbox(geoboundary_country)[c(2, 4)])
      }
    ) +

    ggplot2::theme_minimal() +
    custom_color_scale +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 15, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 10, face = "italic"),
      legend.key.size = ggplot2::unit(0.8, "lines"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      plot.margin = ggplot2::unit(c(1,1,1,1), "cm"),
      panel.border = ggplot2::element_rect(fill = NA, color = "gray70"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "gray50", linewidth = 0.2),
      axis.text = ggplot2::element_text(size = 10, color = "gray20"),
      axis.title = ggplot2::element_text(size = 12, color = "gray20")
    ) +
    ggspatial::annotation_scale(location = "br", width_hint = 0.2) +
    ggspatial::annotation_north_arrow(location = "tl", which_north = "true",
                                      height = ggplot2::unit(1.0, "cm"), width = ggplot2::unit(1.0, "cm"),
                                      pad_x = ggplot2::unit(0.25, "cm"), pad_y = ggplot2::unit(0.25, "cm"))

  # Display map
  base::print(map)

  # --- 5. SAVE ---

  fixed_width <- 7.09
  dynamic_height <- fixed_width * asp_ratio + 1.25
  filename <- file.path(save_dir, paste0(sanitized_country, "_Effort_Map.png"))

  ggplot2::ggsave(
    filename = filename,
    plot = map,
    width = fixed_width,
    height = dynamic_height,
    units = "in",
    dpi = 300,
    type = "cairo"
  )
  return(invisible(filename))
}
