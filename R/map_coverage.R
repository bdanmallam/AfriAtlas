#' Map Coverage Effort
#'
#' Maps the survey effort (hours) for a specific country using African Bird Atlas (ABAP) data.
#' It automatically handles ADM1 boundaries and dynamic coloring based on effort hours.
#' Generates summary statistics (Mean and SE) and exports them to a CSV file.
#'
#' @param country A character string representing the country name (e.g., "Nigeria").
#' @param scale Scale of the map boundaries, default is "large".
#' @return Generates a PNG map and a CSV summary file in the working directory.
#' @import ggplot2
#' @import sf
#' @import dplyr
#' @importFrom stats sd
#' @importFrom ggspatial annotation_scale annotation_north_arrow
#' @importFrom rnaturalearth ne_states ne_countries
#' @importFrom utils txtProgressBar setTxtProgressBar write.csv
#' @export
map_coverage <- function(country, scale = "large") {

  # Sanitize country name for API call and filenames
  sanitized_country <- gsub(" ", "", country, fixed = TRUE)

  # --- INITIALIZE PROGRESS BAR (5 Steps) ---
  pb <- utils::txtProgressBar(min = 0, max = 5, style = 3)

  # --- 1. SETUP AND DATA ACQUISITION ---

  coverage_data <- tryCatch({
    # SILENCE OUTPUT: quiet = TRUE stops "Reading layer..." messages
    suppressMessages(suppressWarnings(sf::st_read(
      paste0('https://api.birdmap.africa/sabap2/v2/coverage/country/', sanitized_country, '?format=geoJSON'),
      quiet = TRUE
    )))
  }, error = function(e) {
    close(pb)
    stop(paste("Failed to retrieve coverage data for", country, ". Error:", e$message))
  })

  utils::setTxtProgressBar(pb, 1)

  if (nrow(coverage_data) == 0) {
    close(pb)
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
      close(pb)
      stop(paste0("ADM0 boundary for '", country, "' also not found."))
    }
  } else {
    geoboundary_country <- adm1_boundaries
  }

  utils::setTxtProgressBar(pb, 2)

  # Filter out Adhoc pentads (0 cards) for plotting, but keep 0 hours if valid
  coverage_data <- coverage_data %>%
    dplyr::filter(full.protocol_total_hours >= 0)

  if (nrow(coverage_data) == 0) {
    warning(paste("No data found for", country, ". Plotting only boundaries."))
  }

  # --- 2. DYNAMIC CATEGORIZATION SCHEME (TOTAL HOURS) ---

  data_column <- coverage_data$full.protocol_total_hours

  if (length(data_column) > 0 && !all(is.na(data_column))) {
    max_protocol <- max(data_column, na.rm = TRUE)
  } else {
    max_protocol <- 0
  }

  # Define the full 9-category scheme
  full_breaks <- c(-Inf, 2, 4, 8, 12, 26, 51, 101, 1000, Inf)

  # *** FIX: Added "hrs" to labels to prevent Excel from converting "2-3" to "Feb-3" ***
  full_labels <- c("Ad-hoc (0-1 hrs)", "2-3 hrs", "4-7 hrs", "8-11 hrs", "12-25 hrs", "26-50 hrs", "51-100 hrs", "101-999 hrs", "1000+ hrs")

  full_colors <- c("#B2B1AF", "#FEF67E", "#FFBF00", "#FF9F00", "#FF7F00", "#FF0000", "#D00062", "#97216B", "#75214B", "#4A002F")

  # --- Truncation Logic ---
  upper_bounds <- full_breaks[2:(length(full_breaks)-1)]
  cutoff_idx <- which(upper_bounds > max_protocol)[1]

  if (is.na(cutoff_idx)) {
    breaks_final <- full_breaks
    labels_final <- full_labels
    colors_final <- full_colors[1:9]
  } else {
    n_cats <- cutoff_idx
    breaks_final <- full_breaks[1:(n_cats + 1)]
    labels_final <- full_labels[1:n_cats]
    colors_final <- full_colors[1:n_cats]

    if (n_cats > 1) {
      last_break_val <- full_breaks[n_cats]
      # Dynamic label update (keeps "hrs" consistency)
      if (last_break_val == 101) {
        labels_final[n_cats] <- "100+ hrs"
      } else {
        labels_final[n_cats] <- paste0(last_break_val, "+ hrs")
      }
    }
  }

  # Apply the cut() logic
  coverage_data$full.protocol.factor <- cut(
    data_column,
    breaks = breaks_final,
    labels = labels_final,
    include.lowest = TRUE,
    right = FALSE
  )

  utils::setTxtProgressBar(pb, 3)

  # --- 2.5. STATISTICS & CSV EXPORT ---

  summary_stats <- coverage_data %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(full.protocol.factor) %>%
    dplyr::summarise(
      n_pentads = dplyr::n(),
      mean_hours = mean(full.protocol_total_hours, na.rm = TRUE),
      sd_hours = sd(full.protocol_total_hours, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      se_hours = sd_hours / sqrt(n_pentads)
    ) %>%
    dplyr::mutate(
      mean_hours = round(mean_hours, 2),
      se_hours = round(se_hours, 2)
    ) %>%
    dplyr::select(full.protocol.factor, n_pentads, mean_hours, se_hours)

  # Print to console
  message(paste0("\n\n--- Effort Summary for ", country, " ---"))
  print(as.data.frame(summary_stats), row.names = FALSE)
  message("--------------------------------------------")

  # *** EXPORT TO CSV ***
  csv_filename <- paste0(sanitized_country, "_Effort_Summary.csv")
  utils::write.csv(summary_stats, file = csv_filename, row.names = FALSE)
  message(paste("Summary stats saved to:", csv_filename))

  # --- 3. DYNAMIC PLOT SETUP ---

  bbox <- sf::st_bbox(geoboundary_country)
  width_geo <- bbox["xmax"] - bbox["xmin"]
  height_geo <- bbox["ymax"] - bbox["ymin"]
  asp_ratio <- height_geo / width_geo

  custom_color_scale <- scale_fill_manual(
    name = "Total Hours",
    values = colors_final,
    labels = labels_final,
    na.value = "transparent"
  )

  current_year <- format(Sys.Date(), "%Y")
  project_name <- if (country == "Kenya") "Bird Map" else "Bird Atlas"
  caption_text <- paste0("\u00A9 ", current_year, " ", country, " ", project_name, ",\n a member of the African Bird Atlas Project")

  # --- 4. PLOT GENERATION ---

  map <- ggplot() +
    geom_sf(data = geoboundary_country, fill = NA, color = "gray60", linewidth = 0.3) +
    geom_sf(data = coverage_data, aes(fill = full.protocol.factor), color = NA) +
    labs(
      title = NULL,
      subtitle = paste("Effort Map for", country),
      caption = caption_text
    ) +
    xlab(NULL) + ylab(NULL) +
    {
      if (country == "South Africa") {
        coord_sf(xlim = sf::st_bbox(geoboundary_country)[c(1, 3)], ylim = c(-40, -20))
      } else {
        coord_sf(xlim = sf::st_bbox(geoboundary_country)[c(1, 3)], ylim = sf::st_bbox(geoboundary_country)[c(2, 4)])
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

  utils::setTxtProgressBar(pb, 4)
  suppressWarnings(print(map))

  # --- 5. SAVE MAP ---

  fixed_width <- 7.09
  dynamic_height <- fixed_width * asp_ratio + 1.25
  map_filename <- paste0(sanitized_country, "_Effort_Map.png")

  suppressWarnings(ggsave(
    filename = map_filename,
    plot = map,
    width = fixed_width,
    height = dynamic_height,
    units = "in",
    dpi = 300,
    type = "cairo"
  ))

  utils::setTxtProgressBar(pb, 5)
  close(pb)

  message(paste("\nMap saved to:", map_filename))
}
