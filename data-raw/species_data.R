## code to prepare `DATASET` dataset goes here

library(dplyr)
library(usethis)

# 1. Load the Unicode-escaped CSV file (which contains Spp, commonName, scientificName)
species_data <- read.csv("data-raw/species_code.csv", stringsAsFactors = FALSE)

# 2. RENAME the column from 'Spp' to 'species_code', keeping ALL other columns.
# This aligns the data structure with the logic in map_species() and get_species_code().
species_data <- species_data %>%
  dplyr::rename(species_code = Spp)

# 3. Save the corrected data object into the package's data directory.
# This creates the data/species_data.rda file used by your package.
usethis::use_data(species_data, internal = TRUE, overwrite = TRUE)
