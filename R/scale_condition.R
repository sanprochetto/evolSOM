#' @title Scale data
#'
#' @description This function scales each control or treatment condition data individually by applying a log transformation and
#' standardizing the columns.
#'
#' @param ... Numeric vectors or matrices of condition data.
#' @param rnames Character vector of row names for the resulting scaled data.
#' @param condition_names Character vector of condition names.
#' @return A named list of scaled data.
#' @import dplyr
#' @export
scale_condition <- function(..., rnames, condition_names) {
 # library(tidyverse)
  condition_list <- list(...)
  condition_scaled_list <- lapply(condition_list, function(condition) {
    condition_scaled <- stats::na.omit(log(condition + 1)) %>% t() %>% scale() %>% t()
  })
  for (i in 1:length(condition_scaled_list)) {
    row.names(condition_scaled_list[[i]]) <- rnames
  }
  names(condition_scaled_list) <- condition_names
  return(condition_scaled_list)
}
