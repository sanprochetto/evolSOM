#' Optimal SOM map size determination
#'
#' This function determines the optimal self-organizing map (SOM) grid size for a given dataset based on neuron correlations.
#'
#' @param data species data for build reference SOM. A data matrix or data frame without NAs.
#' @param threshold The neuron correlation threshold for reducing the SOM grid size.
#' @param init_dim The initial SOM grid dimensions.
#' @param max_iterations The maximum number of iterations for SOM training.
#' @return A list with the resulting SOM grid size, dimensions, and correlation matrix.
#' @export
opt_map_size <- function(data, threshold = 0.8, init_dim = 5, max_iterations = 1000) {
  # Error checking
  #if (nrow(data) == 0) {
  #  stop("Input dataset is empty.")
  #}
  if (any(is.na(data))) {
    stop("Input dataset contains missing values.")
  }

  # Select variables
  train.data <- as.matrix(data)
  n_var <- ncol(train.data)
  # Determine initial SOM grid size
  init_size <- init_dim^2 + 1
  set.seed(100) # For reproducibility
  x<- 1
  while(sum(x) > 0) {
    init_size <- init_size - 1
    dim1<- ceiling(sqrt(init_size))
    dim2<- floor(sqrt(init_size))
    # Train SOM model
    init <- aweSOM::somInit(train.data, dim1, dim2)
    model.som <- kohonen::som(train.data,
                              grid = kohonen::somgrid(dim1, dim2, "hexagonal"),
                              rlen = max_iterations,
                              alpha = c(0.1, 0.001),
                              dist.fcts = "euclidean",
                              init = init)
    # Reduce SOM grid size based on neuron correlation
    data.cor <- t(model.som[["codes"]][[1]]) # Correlation between neurons
    cor_matrix <- replace(stats::cor(data.cor), stats::cor(data.cor) == 1, NA)
    colnames(cor_matrix) <- paste0("Neu", 1:(dim1*dim2))
    row.names(cor_matrix) <-colnames(cor_matrix)
    x <-  stats::na.omit(as.vector(cor_matrix > threshold))
  }
  # Return SOM grid size and dimensions
  result <- list(grid_size = init_size,
                 grid_dim1 = dim1,
                 grid_dim2 = dim2,
                 cor_matrix = cor_matrix)
  names(result) <- c("grid_size", "grid_dim1", "grid_dim2", "correlation_matrix")

  # Add a message to the result
  message(paste("The optimum grid size for this data is", dim1, "x", dim2))
  return(result)
}
