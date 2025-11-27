#' @title Explore Package Species Codes
#' @description Opens the package's species code data (\code{species_data}) in a viewer.
#'   This dataset is available when the \code{AfriAtlas} package is loaded.
#' @export
explore_species_codes <- function() {
  if (interactive()) {
    # Note: 'species_data' is a globally available object once the package is loaded
    utils::View(AfriAtlas::species_data)
  } else {
    message("View() works only in interactive R sessions. Access the data using AfriAtlas::species_data.")
  }
}

#' @title Get Species Code by Common Name
#' @description Looks up the numeric species code for a given common name from
#'   the package's species data.
#'
#' @param commonName A character string of the species common name (case-sensitive).
#' @return The numeric species code as an integer, or \code{NA} if not found.
#' @export
get_species_code <- function(commonName) {
  data <- AfriAtlas::species_data

  # Find the index of the row where commonName matches the input
  row_index <- which(data$commonName == commonName)

  if (length(row_index) == 0) {
    warning("Common name not found in the dataset.")
    return(NA_integer_)
  }

  # Return the species code from the found index, ensuring it is integer type
  # We use the first index [1] in case of duplicate names
  return(as.integer(data$species_code[row_index[1]]))
}
