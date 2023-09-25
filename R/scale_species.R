#' @title Scale data
#'
#' @description This function scales the species data individually by applying a log transformation and
#' standardizing the columns.
#'
#' @param ... Numeric vectors or matrices of species data.
#' @param rnames Character vector of row names for the resulting scaled data.
#' @param species_names Character vector of species names.
#' @return A named list of scaled data.
#' @import dplyr
#' @export
scale_species <- function(..., rnames, species_names) {
 # library(tidyverse)
  species_list <- list(...)
  species_scaled_list <- lapply(species_list, function(species) {
    species_scaled <- stats::na.omit(log(species + 1)) %>% t() %>% scale() %>% t()
  })
  for (i in 1:length(species_scaled_list)) {
    row.names(species_scaled_list[[i]]) <- rnames
  }
  names(species_scaled_list) <- species_names
  return(species_scaled_list)
}
