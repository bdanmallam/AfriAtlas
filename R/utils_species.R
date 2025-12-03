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
#' Looks up the numeric species code for a given common name.
#' @param commonName Case-sensitive common name.
#' @return Integer species code.
#' @export
get_species_code <- function(commonName) {
  data <- AfriAtlas::species_data
  row_index <- which(data$commonName == commonName)
  if (length(row_index) == 0) {
    warning("Common name not found in the dataset.")
    return(NA_integer_)
  }
  return(as.integer(data$species_code[row_index[1]]))
}
