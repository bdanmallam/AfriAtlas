#' @title Generate Species Distribution Map
#' @description Downloads, processes, and plots pentad-level sighting and
#' ?? coverage data for one or more species in a specified country from the African
#' ?? Bird Atlas Project (ABAP) API. Includes a widespreadness pie chart inset.
#'
#' @param species_codes A numeric vector of one or more species codes (max 10).
#' @param country A character string specifying the country name (e.g., "South Africa").
#' @param save_dir A character string specifying the directory to save the output PNG file(s).
#' ?? Defaults to the current working directory.
#' @param verbose Logical. If TRUE, prints status messages and warnings.
#' @return A character vector of file paths to the generated map images (invisibly).
#' @importFrom rnaturalearth ne_states ne_countries
#' @importFrom sf st_read st_bbox st_as_sf
#' @importFrom dplyr filter select mutate bind_rows
#' @importFrom cowplot ggdraw draw_plot
#' @importFrom ggplot2 ggplot aes geom_sf labs coord_sf theme_minimal theme scale_color_manual scale_fill_manual guides guide_legend geom_bar coord_polar geom_text position_stack theme_void ggsave unit
#' @importFrom ggspatial annotation_scale annotation_north_arrow
#' @export
map_species <- function(species_codes, country, save_dir = getwd(), verbose = TRUE) {

  # --- BATCH LIMIT RESTRICTION ---
  if (length(species_codes) > 10) {
    stop(paste("The number of species codes (", length(species_codes), ") exceeds the maximum limit of 10. Execution stopped."))
  }
  is_single_map <- length(species_codes) == 1
  # ------------------------------------------------

  # --- CRITICAL FIX: SANITIZE COUNTRY NAME FOR API ---
  sanitized_country <- gsub(" ", "", country, fixed = TRUE)
  saved_files <- c()

  # --- 1. INITIAL SETUP AND BOUNDARY FILTERING ---

  if (verbose) message("Loading country boundaries...")
  adm1_boundaries <- rnaturalearth::ne_states(country = country, returnclass = "sf")

  if (nrow(adm1_boundaries) == 0) {
    if (verbose) warning(paste0("ADM1 boundaries not found for '", country, "'. Falling back to ADM0 (country outline only)."))
    africa_boundary <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
    geoboundary_country <- africa_boundary |>
      dplyr::filter(.data$name == country)
    if (nrow(geoboundary_country) == 0) { stop(paste0("ADM0 boundary for '", country, "' also not found.")) }
  } else {
    geoboundary_country <- adm1_boundaries
  }

  # Calculate aspect ratio
  bbox <- sf::st_bbox(geoboundary_country)
  width_geo <- bbox["xmax"] - bbox["xmin"]
  height_geo <- bbox["ymax"] - bbox["ymin"]
  asp_ratio <- height_geo / width_geo

  # Load all coverage pentads once
  if (verbose) message("Loading coverage data...")
  coverage_url <- paste0(.abap_base_url, "coverage/country/", sanitized_country, "?format=geoJSON")
  coverage_data <- suppressMessages(sf::st_read(coverage_url)) |>
    dplyr::select(.data$pentad, .data$geometry) |>
    dplyr::mutate(Legend = "Coverage")

  # Load package species data
  species_codes_df <- species_data

  # Start loop through all species codes
  for (code in species_codes) {

    # --- 2. DATA ACQUISITION AND CLEANING ---
    commoName <- species_codes_df |>
      dplyr::filter(.data$species_code == code) |>
      dplyr::select(.data$commonName)

    if (nrow(commoName) == 0) {
      warning(paste0("Species code '", code, "' not found in internal list. Skipping."))
      next
    }
    commoName <- commoName$commonName[1]
    if (verbose) message(paste0("Processing species: ", commoName, " (", code, ")"))

    species_url <- paste0(.abap_base_url, "summary/species/", code, "/country/", sanitized_country, "?format=geoJSON")
    bird_data <- suppressMessages(sf::st_read(species_url))

    if (nrow(bird_data) == 0) {
      if (verbose) message(paste0("No data found for species code: ", code, " in ", country))
      next
    }

    bird_data <- bird_data |>
      dplyr::select(.data$pentad, .data$geometry) |>
      dplyr::mutate(Legend = "Sightings")

    # --- 3. WIDESPREADNESS CALCULATION (PIE CHART DATA) ---

    covered_pentads <- unique(coverage_data$pentad)
    sighted_pentads <- unique(bird_data$pentad)

    n_coverage <- length(covered_pentads)
    n_sighted_in_coverage <- length(base::intersect(covered_pentads, sighted_pentads))
    n_not_sighted <- n_coverage - n_sighted_in_coverage

    pie_data <- data.frame(
      Category = c("Not Sighted", "Sighted"),
      Count = c(n_not_sighted, n_sighted_in_coverage)
    ) |>
      dplyr::mutate(
        Percentage = .data$Count / sum(.data$Count),
        Label = paste0(round(.data$Percentage * 100, 1), "%")
      )

    # Create corrected pie chart
    pie_chart <- ggplot2::ggplot(pie_data, ggplot2::aes(x = "", y = .data$Percentage, fill = .data$Category)) +
      ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
      ggplot2::coord_polar("y", start = 0) +
      ggplot2::scale_fill_manual(values = c("Sighted" = "#f03b20", "Not Sighted" = "#ffeda0")) +
      ggplot2::geom_text(data = subset(pie_data, .data$Category == "Sighted"),
                         ggplot2::aes(label = .data$Label),
                         position = ggplot2::position_stack(vjust = 0.5),
                         size = 3.0,
                         color = "black") +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")

    # --- 4. MAP DATA AND AESTHETICS ---

    map_data <- dplyr::bind_rows(coverage_data, bird_data)

    current_year <- base::format(base::Sys.Date(), "%Y")
    caption_text <- paste0("?? ", current_year, " ", country, " Bird Atlas,\n a member of the African Bird Atlas Project")

    # Main map
    map <- ggplot2::ggplot() +
      # Lighter, thinner ADM1 boundaries
      ggplot2::geom_sf(data = geoboundary_country, fill = NA, color = "gray60", linewidth = 0.3) +

      ggplot2::geom_sf(data = map_data, ggplot2::aes(fill = .data$Legend, color = .data$Legend), alpha = 0.7) +
      ggplot2::scale_color_manual(values = c("Coverage" = "#ffeda0", "Sightings" = "#f03b20")) +
      ggplot2::scale_fill_manual(values = c("Coverage" = "#ffeda0", "Sightings" = "#f03b20")) +
      ggplot2::labs(title = paste0("Observations of ", commoName, " in ", country),
                    caption = caption_text, x = NULL, y = NULL) +

      # Conditional Coordinate Limits for South Africa context
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
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 15, face = "bold"),
        legend.key = ggplot2::element_rect(colour = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = ggplot2::element_text(face="italic"),
        plot.margin=ggplot2::unit(c(10,5,5,5),"mm"),
        panel.border = ggplot2::element_rect(fill = NA, color = "gray70"),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
        plot.background = ggplot2::element_rect(fill = "transparent", color = NA)
      ) +
      ggspatial::annotation_scale(location = "br", width_hint = 0.2) +
      ggspatial::annotation_north_arrow(location = "tl", which_north = "true",
                                        height = ggplot2::unit(1.0, "cm"), width = ggplot2::unit(1.0, "cm"),
                                        pad_x = ggplot2::unit(0.25, "cm"), pad_y = ggplot2::unit(0.25, "cm")) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "Legend"))

    # --- 5. COMBINE, DISPLAY, AND SAVE ---

    final_plot <- cowplot::ggdraw() +
      cowplot::draw_plot(map) +
      # PIE CHART POSITION (Top Right, INSIDE map area)
      cowplot::draw_plot(pie_chart,
                         x = 0.78, y = 0.72,
                         width = 0.18, height = 0.18)

    # Conditional Display
    if (is_single_map) {
      base::print(final_plot)
    }

    # Save to file with DYNAMIC HEIGHT
    fixed_width <- 6.45
    dynamic_height <- fixed_width * asp_ratio + 1.25
    filename <- file.path(save_dir, paste0(commoName, "_", country, ".png"))

    ggplot2::ggsave(filename,
                    plot = final_plot,
                    width = fixed_width,
                    height = dynamic_height,
                    dpi = 150,
                    type = "cairo",
                    bg = "transparent")
    saved_files <- c(saved_files, filename)
    if (verbose) message(paste0("Map saved to: ", filename))
  }
  return(invisible(saved_files))
}
