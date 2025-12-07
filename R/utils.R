#' Explore Species Codes
#'
#' Opens the package's species code data in a viewer.
#' @export
explore_species_codes <- function() {
  if (interactive()) {
    utils::View(AfriAtlas::species_data)
  } else {
    message("View() works only in interactive R sessions.")
  }
}

#' Get Species Code
#'
#' Looks up the numeric species code for a given common name or scientific name.
#' @param species_name A character string representing either the case-sensitive
#'   common name or the scientific name.
#' @return Integer species code. Returns \code{NA_integer_} if not found.
#' @export
get_species_code <- function(species_name) {
  data <- AfriAtlas::species_data

  # 1. Try to find a match in Common Names
  row_index <- which(data$commonName == species_name)

  # 2. If no match found, try to find a match in Scientific Names
  if (length(row_index) == 0) {
    row_index <- which(data$scientificName == species_name)
  }

  # 3. If still no match, warn and return NA
  if (length(row_index) == 0) {
    warning(paste("Species", sQuote(species_name), "not found in dataset."))
    return(NA_integer_)
  }

  return(as.integer(data$species_code[row_index[1]]))
}
