#' @title Global Variable Definitions
#' @description This file defines the necessary global variables used for tidy
#'   evaluation (\code{.data}) to prevent "no visible binding" NOTES during \code{R CMD check}.
#'
#' @param libname Path to the package library.
#' @param pkgname Package name.
#' @keywords internal
#' @importFrom utils globalVariables
.onLoad <- function(libname, pkgname) {
  # ADD 'species_data' to the list
  utils::globalVariables(c(".data", ".", "species_data"))
}
