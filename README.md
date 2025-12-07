
<p align="center">
<img src="man/figures/logo.jpg" alt="AfriAtlas R Package Logo" width="180">
</p>

# AfriAtlas: Visualizing African Bird Atlas Data

The **`AfriAtlas`** R package provides robust, publication-ready tools
for ecologists and enthusiasts to download, process, and visualize
pentad-level bird atlas data from the **African Bird Atlas Project
(ABAP)** and its associated country projects (like Nigeria Bird Atlas
Project).

The package is designed for portability and efficiency, handling dynamic
boundaries via the `rnaturalearth` package and enforcing API safety
limits.

------------------------------------------------------------------------

## 🚀 Installation

You can install the development version of `AfriAtlas` directly from
GitHub using the `remotes` package.

``` r
# Install necessary dependencies if you haven't already
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install AfriAtlas 
remotes::install_github("bdanmallam/AfriAtlas")
```

# Quick Usage Examples

## 1. Finding and reviewing species codes

The mapping functions require numeric Species Codes (e.g., 11491 for
Common Bulbul). AfriAtlas provides three tools to help you manage these
codes.

## 1.1. Lookup a code by name: `get_species_code()`

If you know the English common name or scientific name, you can quickly
retrieve the code. This function is case-sensitive.

``` r
# Find code for Barn Swallow (Hirundo rustica)
code <- get_species_code("Barn Swallow")
code <- get_species_code("Hirundo rustica")
print(code)

# Find code for Hooded vulture (Necrosyrtes monachus)
get_species_code("Hooded vulture")
get_species_code("Necrosyrtes monachus")
```

## 1.2. Browse all species: `explore_species_codes()`

If you are unsure of the spelling or want to browse the full species
codes, this function opens the internal dataset in the RStudio Viewer.

``` r
explore_species_codes()
```

## 1.3. Reviewing species codes: `show_codes()`

Before running a batch process, it is best practice to review the list
of codes being used. The `show_codes()` function is a simple utility for
this. Note that the primary mapping function, `map_species()`, will stop
execution if the list exceeds Spp 10 codes.

``` r
# Example list of species codes (e.g., common African species)
my_species_list <- c(129, 311, 437, 846, 11491, 1168)

# Check the list size
show_codes(my_species_list)

# Example of a list exceeding the limit (Will stop map_species() execution)
# show_codes(c(1:12))
```

# 2. Map species distribution: `map_species()`

Generates a map of species sightings versus coverage, including a
widespreadness pie chart. Execution is halted if more than 10 species
are requested.

``` r
library(AfriAtlas)

# Generate a single species map for South Africa (Pin-tailed Whydah)
map_species(species_codes = 846, country = "Nigeria")

# Generate maps for a batch of species in Kenya (saved as PNGs)
map_species(species_codes = c(129, 311), country = "Kenya")
```

# 3. Map survey effort `map_coverage()`

The `map_coverage()` function generates a map showing the intensity of
bird atlas survey effort, categorized by total hours spent per pentad
(e.g., 0–1 hr, 2–3 hrs, etc.). It also produces accompanying summary
statistics, including the number of pentads in each effort category and
the mean ± SE of accumulated hours, which are automatically exported to
a CSV file.

``` r
# Generate effort map for Nigeria 
map_coverage("Nigeria")

# Generate effort map for South Africa 
map_coverage("South Africa")
```

# Further Documentation

For further guides, please refer to the package vignette:

``` r
vignette("AfriAtlas_tutorial")
```
