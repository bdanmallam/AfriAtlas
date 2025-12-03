#' Map Species Distribution
#'
#' Generates a map of species sightings and coverage for a specific country.
#'
#' @param species_codes A numeric vector of species codes (max 10).
#' @param country A character string representing the country name (e.g., "Nigeria").
#' @import ggplot2
#' @import sf
#' @import dplyr
#' @importFrom ggspatial annotation_scale annotation_north_arrow
#' @importFrom cowplot ggdraw draw_plot
#' @importFrom rnaturalearth ne_states ne_countries
#' @export
map_species <- function(species_codes, country) {

  # --- BATCH LIMIT RESTRICTION ---
  if (length(species_codes) > 10) {
    stop(paste("The number of species codes (", length(species_codes), ") exceeds the maximum limit of 10. Execution stopped. Use explore_species_codes() to review the list."))
  }
  is_single_map <- length(species_codes) == 1

  # --- CRITICAL FIX: SANITIZE COUNTRY NAME FOR API ---
  sanitized_country <- gsub(" ", "", country, fixed = TRUE)

  # --- 1. INITIAL SETUP AND BOUNDARY FILTERING ---
  adm1_boundaries <- rnaturalearth::ne_states(country = country, returnclass = "sf")

  if (nrow(adm1_boundaries) == 0) {
    warning(paste0("ADM1 boundaries not found for '", country, "'. Falling back to ADM0."))
    africa_boundary <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
    geoboundary_country <- africa_boundary %>% dplyr::filter(name == country)
    if (nrow(geoboundary_country) == 0) { stop(paste0("ADM0 boundary for '", country, "' also not found.")) }
  } else {
    geoboundary_country <- adm1_boundaries
  }

  bbox <- sf::st_bbox(geoboundary_country)
  width_geo <- bbox["xmax"] - bbox["xmin"]
  height_geo <- bbox["ymax"] - bbox["ymin"]
  asp_ratio <- height_geo / width_geo

  # Load coverage
  coverage_url <- paste0("https://api.birdmap.africa/sabap2/v2/coverage/country/", sanitized_country, "?format=geoJSON")
  coverage_data <- suppressMessages(sf::st_read(coverage_url)) %>%
    dplyr::select(pentad, geometry) %>%
    dplyr::mutate(Legend = "Coverage")

  # --- DATA CHANGE: Use internal package data ---
  # No longer reading CSV. 'species_data' is lazy-loaded from data/
  species_codes_df <- AfriAtlas::species_data

  for (code in species_codes) {

    bird_url <- paste0("https://api.birdmap.africa/sabap2/v2/summary/species/", code, "/country/", sanitized_country, "?format=geoJSON")
    bird_data <- suppressMessages(sf::st_read(bird_url))

    if (nrow(bird_data) == 0) {
      message(paste0("No data found for species code: ", code, " in ", country))
      next
    }

    bird_data <- bird_data %>%
      dplyr::select(pentad, geometry) %>%
      dplyr::mutate(Legend = "Sightings")

    # --- WIDESPREADNESS CALCULATION ---
    covered_pentads <- unique(coverage_data$pentad)
    sighted_pentads <- unique(bird_data$pentad)
    n_coverage <- length(covered_pentads)
    n_sighted_in_coverage <- length(base::intersect(covered_pentads, sighted_pentads))
    n_not_sighted <- n_coverage - n_sighted_in_coverage

    pie_data <- data.frame(
      Category = c("Not Sighted", "Sighted"),
      Count = c(n_not_sighted, n_sighted_in_coverage)
    ) %>%
      dplyr::mutate(
        Percentage = Count / sum(Count),
        Label = paste0(round(Percentage * 100, 1), "%")
      )

    pie_chart <- ggplot(pie_data, aes(x = "", y = Percentage, fill = Category)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("Sighted" = "#f03b20", "Not Sighted" = "#ffeda0")) +
      geom_text(data = subset(pie_data, Category == "Sighted"),
                aes(label = Label), position = position_stack(vjust = 0.5),
                size = 3.0, color = "black") +
      theme_void() + theme(legend.position = "none")

    # --- MAP ---
    map_data <- dplyr::bind_rows(coverage_data, bird_data)

    # Lookup name using internal data
    commoName <- species_codes_df$commonName[species_codes_df$species_code == code]

    current_year <- format(Sys.Date(), "%Y")
    caption_text <- paste0("© ", current_year, " ", country, " Bird Atlas,\n a member of the African Bird Atlas Project")

    map <- ggplot() +
      geom_sf(data = geoboundary_country, fill = NA, color = "gray60", linewidth = 0.3) +
      geom_sf(data = map_data, aes(fill = Legend, color = Legend), alpha = 0.7) +
      scale_color_manual(values = c("Coverage" = "#ffeda0", "Sightings" = "#f03b20")) +
      scale_fill_manual(values = c("Coverage" = "#ffeda0", "Sightings" = "#f03b20")) +
      labs(title = paste0("Observations of ", commoName, " in ", country),
           caption = caption_text, x = NULL, y = NULL) +
      theme_minimal() +
      annotation_scale(location = "br", width_hint = 0.2) +
      annotation_north_arrow(location = "tl", which_north = "true",
                             height = unit(1.0, "cm"), width = unit(1.0, "cm"))

    if (country == "South Africa") {
      map <- map + coord_sf(xlim = st_bbox(geoboundary_country)[c(1, 3)], ylim = c(-40, -20))
    } else {
      map <- map + coord_sf(xlim = st_bbox(geoboundary_country)[c(1, 3)], ylim = st_bbox(geoboundary_country)[c(2, 4)])
    }

    final_plot <- cowplot::ggdraw() +
      cowplot::draw_plot(map) +
      cowplot::draw_plot(pie_chart, x = 0.78, y = 0.72, width = 0.18, height = 0.18)

    if (is_single_map) print(final_plot)

    fixed_width <- 6.45
    dynamic_height <- fixed_width * asp_ratio + 1.25

    ggsave(paste0(commoName, "_", country, ".png"), plot = final_plot,
           width = fixed_width, height = dynamic_height, dpi = 150, type = "cairo", bg = "transparent")
  }
}
