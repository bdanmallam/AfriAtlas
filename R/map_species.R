#' Map Species Distribution
#'
#' Generates a map of species sightings and coverage for a specific country
#' using the AfriAtlas aesthetic standards. Handles API failures gracefully.
#'
#' @param species_codes A numeric vector of species codes (max 10).
#' @param country A character string representing the country name (e.g., "Nigeria").
#' @import ggplot2
#' @import sf
#' @import dplyr
#' @importFrom ggspatial annotation_scale annotation_north_arrow
#' @importFrom cowplot ggdraw draw_plot
#' @importFrom rnaturalearth ne_states ne_countries
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
map_species <- function(species_codes, country) {

  # --- BATCH LIMIT RESTRICTION ---
  if (length(species_codes) > 10) {
    stop(paste("The number of species codes (", length(species_codes), ") exceeds the maximum limit of 10. Execution stopped. Use explore_species_codes() to review the list."))
  }

  # Check if this is a single map request
  is_single_map <- length(species_codes) == 1

  # --- SANITIZE COUNTRY NAME FOR API ---
  sanitized_country <- gsub(" ", "", country, fixed = TRUE)

  # --- INITIALIZE PROGRESS BAR ---
  if (is_single_map) {
    pb <- utils::txtProgressBar(min = 0, max = 5, style = 3)
  } else {
    message("Initializing batch process...")
    pb <- utils::txtProgressBar(min = 0, max = length(species_codes), style = 3)
  }

  # --- 1. INITIAL SETUP AND BOUNDARY FILTERING ---
  adm1_boundaries <- rnaturalearth::ne_states(country = country, returnclass = "sf")

  if (nrow(adm1_boundaries) == 0) {
    warning(paste0("ADM1 boundaries not found for '", country, "'. Falling back to ADM0."))
    africa_boundary <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
    geoboundary_country <- africa_boundary %>% dplyr::filter(name == country)
    if (nrow(geoboundary_country) == 0) {
      close(pb)
      stop(paste0("ADM0 boundary for '", country, "' also not found."))
    }
  } else {
    geoboundary_country <- adm1_boundaries
  }

  # Calculate Aspect Ratio for Dynamic Height
  bbox <- sf::st_bbox(geoboundary_country)
  width_geo <- bbox["xmax"] - bbox["xmin"]
  height_geo <- bbox["ymax"] - bbox["ymin"]
  asp_ratio <- height_geo / width_geo

  # Prepare Coverage URL but DO NOT LOAD yet (Lazy Loading)
  coverage_url <- paste0("https://api.birdmap.africa/sabap2/v2/coverage/country/", sanitized_country, "?format=geoJSON")
  coverage_data <- NULL # Initialize empty container

  # Use internal package data
  species_codes_df <- AfriAtlas::species_data

  # --- MAIN LOOP ---
  for (i in seq_along(species_codes)) {
    code <- species_codes[i]

    # 2. DATA ACQUISITION (BIRD DATA FIRST)
    bird_url <- paste0("https://api.birdmap.africa/sabap2/v2/summary/species/", code, "/country/", sanitized_country, "?format=geoJSON")

    # --- DEFENSIVE PROGRAMMING: tryCatch for Bird Data ---
    bird_data <- tryCatch({
      suppressMessages(sf::st_read(bird_url, quiet = TRUE))
    }, error = function(e) {
      # Log the error but don't stop execution
      message(paste0("\n[API Error] Could not retrieve data for species ", code, ": ", e$message))
      return(NULL)
    })

    # Update PB Step 1 (Bird Data Loaded) for single map
    if (is_single_map) utils::setTxtProgressBar(pb, 1)

    # CHECK: If API failed (NULL) or returned empty data (nrow=0)
    if (is.null(bird_data) || nrow(bird_data) == 0) {
      if (is.null(bird_data)) {
        # Message already printed by tryCatch
      } else {
        message(paste0("\nNo sightings records found for species code: ", code, " in ", country))
      }

      if (is_single_map) {
        utils::setTxtProgressBar(pb, 5)
        close(pb)
        return(invisible(NULL))
      } else {
        # Skip to next species in batch
        utils::setTxtProgressBar(pb, i)
        next
      }
    }

    # Data exists: Now process it (Change "Sightings" to "Observations")
    bird_data <- bird_data %>%
      dplyr::select(pentad, geometry) %>%
      dplyr::mutate(Legend = "Observations")

    # LAZY LOAD COVERAGE DATA (Only if it hasn't been loaded yet)
    if (is.null(coverage_data)) {

      # --- DEFENSIVE PROGRAMMING: tryCatch for Coverage Data ---
      coverage_result <- tryCatch({
        suppressMessages(sf::st_read(coverage_url, quiet = TRUE))
      }, error = function(e) {
        message(paste0("\n[API Error] Could not retrieve coverage data for ", country, ": ", e$message))
        return(NULL)
      })

      # If coverage failed, we cannot proceed with ANY map for this country
      if (is.null(coverage_result)) {
        close(pb)
        stop("Critical failure: Unable to download country coverage data. Check internet connection.")
      }

      coverage_data <- coverage_result %>%
        dplyr::select(pentad, geometry) %>%
        dplyr::mutate(Legend = "Coverage")

      # Update PB Step 2 (Coverage Loaded) for single map
      if (is_single_map) utils::setTxtProgressBar(pb, 2)
    }

    # 3. WIDESPREADNESS CALCULATION (PIE CHART)
    # Note: Pie chart logic remains "Sighted/Not Sighted" for internal logic
    # unless you want the Legend of the PIE chart to change too.
    # Usually the map legend is the priority.
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

    # Pie Chart Aesthetics
    pie_chart <- ggplot(pie_data, aes(x = "", y = Percentage, fill = Category)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("Sighted" = "#f03b20", "Not Sighted" = "#ffeda0")) +
      geom_text(data = subset(pie_data, Category == "Sighted"),
                aes(label = Label),
                position = position_stack(vjust = 0.5),
                size = 3.0,
                color = "black") +
      theme_void() +
      theme(legend.position = "none")

    # Update PB Step 3 (Stats/Pie Ready) for single map
    if (is_single_map) utils::setTxtProgressBar(pb, 3)

    # 4. MAP CONSTRUCTION
    map_data <- dplyr::bind_rows(coverage_data, bird_data)

    # Handle missing common name safely
    species_name <- species_codes_df$commonName[species_codes_df$species_code == code]
    if (length(species_name) == 0) species_name <- paste("Species", code)
    commoName <- species_name

    # --- DYNAMIC COPYRIGHT LOGIC ---
    current_year <- format(Sys.Date(), "%Y")

    # Logic: If Kenya -> "Bird Map", Else -> "Bird Atlas"
    project_name <- if (country == "Kenya") "Bird Map" else "Bird Atlas"

    caption_text <- paste0("\u00A9 ", current_year, " ", country, " ", project_name, ",\n a member of the African Bird Atlas Project")
    # -------------------------------

    # Base Map
    map <- ggplot() +
      # Aesthetic: Lighter, thinner ADM1 boundaries
      geom_sf(data = geoboundary_country, fill = NA, color = "gray60", linewidth = 0.3) +
      geom_sf(data = map_data, aes(fill = Legend, color = Legend), alpha = 0.7) +

      # Aesthetic: Specific Color Palette (UPDATED TO OBSERVATIONS)
      scale_color_manual(values = c("Coverage" = "#ffeda0", "Observations" = "#f03b20")) +
      scale_fill_manual(values = c("Coverage" = "#ffeda0", "Observations" = "#f03b20")) +

      labs(title = paste0("Observations of ", commoName, " in ", country),
           caption = caption_text, x = NULL, y = NULL) +

      # Aesthetic: Specific Theme Details
      theme_minimal() +
      theme(
        plot.title = element_text(size = 15, face = "bold"),
        legend.key = element_rect(colour = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(face="italic"),
        plot.margin = unit(c(10,5,5,5),"mm"),
        panel.border = element_rect(fill = NA, color = "gray70"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA)
      ) +
      annotation_scale(location = "br", width_hint = 0.2) +
      annotation_north_arrow(location = "tl", which_north = "true",
                             height = unit(1.0, "cm"), width = unit(1.0, "cm"),
                             pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm")) +
      guides(fill = guide_legend(title = "Legend"))

    # Aesthetic: Coordinate Exceptions
    if (country == "South Africa") {
      map <- map + coord_sf(xlim = st_bbox(geoboundary_country)[c(1, 3)], ylim = c(-40, -20))
    } else {
      map <- map + coord_sf(xlim = st_bbox(geoboundary_country)[c(1, 3)], ylim = st_bbox(geoboundary_country)[c(2, 4)])
    }

    # 5. FINAL COMPOSITION
    final_plot <- cowplot::ggdraw() +
      cowplot::draw_plot(map) +
      # Aesthetic: Precise Pie Chart Positioning
      cowplot::draw_plot(pie_chart, x = 0.78, y = 0.72, width = 0.18, height = 0.18)

    # Update PB Step 4 (Map Ready) for single map
    if (is_single_map) utils::setTxtProgressBar(pb, 4)

    # --- PRINT AND SAVE LOGIC ---

    # 1. Always Save (Requirement for Atlas generation)
    fixed_width <- 6.45
    dynamic_height <- fixed_width * asp_ratio + 1.25

    filename <- paste0(commoName, "_", country, ".png")

    # Suppress warnings to hide "ragg device" conflicts
    suppressWarnings(ggsave(filename,
                            plot = final_plot,
                            width = fixed_width,
                            height = dynamic_height,
                            dpi = 150,
                            type = "cairo",
                            bg = "transparent"))

    # Update PB Step 5 (Saved) for single map, or Loop Step for batch
    if (is_single_map) {
      utils::setTxtProgressBar(pb, 5)
    } else {
      utils::setTxtProgressBar(pb, i)
    }

    # 2. Print ONLY if it is a single map query
    if (is_single_map) {
      print(final_plot)
      message(paste("\nMap saved to:", filename))
    }
  }

  # Close Progress Bar
  close(pb)
}
