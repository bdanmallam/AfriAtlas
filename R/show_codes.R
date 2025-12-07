#' Show Species Codes
#'
#' A utility to review a list of species codes before batch processing.
#' Checks if the list exceeds the batch limit of 10.
#'
#' @param species_codes A numeric vector of species codes.
#' @return Prints the number of codes and lists them.
#' @export
show_codes <- function(species_codes) {
  n <- length(species_codes)
  cat("--- Batch Processing Review ---\n")
  cat(paste("Total Species Count:", n, "\n"))

  if (n > 10) {
    message("WARNING: This list exceeds the limit of 10 codes.")
    message("map_species() will stop execution if you use this list.")
  } else {
    message("Status: OK (Within limit)")
  }

  print(species_codes)
}
