#' Create mappings and class assignments
#'
#' This function creates mappings and class assignments based on a trained
#' model and input data. The return includes the number of biological variables mapped to
#'  each neuron.
#'
#' @param model A trained SOM model.
#' @param train_data Dataframe used for build "model"
#' @param data_list A list of matrices or data frames with scale expression data.
#' @param condition_names Character vector of condition names corresponding to the data.
#' @param classes A data frame with biological variables class tags.
#' @return A named list containing the mappings, classes, and neuron sizes.
#' @export
create_mappings <- function(model, train_data, data_list, condition_names, classes) {
  # Check inputs
  stopifnot(is.list(data_list), length(data_list) == length(condition_names))
  # Initialize mappings and classes
  mappings <- list()
  # Loop through data and map onto model
  for (i in seq_along(data_list)) {
    mappings[[condition_names[i]]] <- kohonen::map(model, data_list[[i]])
    classes[,as.character(condition_names[i])] <- mappings[[condition_names[i]]][["unit.classif"]]
  }
  # Populate class assignments data frame
  x <- cbind(rownames(train_data), model[["unit.classif"]])
  classes$control_condition <- NA
  for (i in 1:nrow(classes)) {
    matching_rows <- which(x[, 1] == classes[i, 1])
    if (length(matching_rows) > 0) {
      classes[i, "control_condition"] <- x[matching_rows, 2]
    }
  }
  #Create neuron size table
  # Create empty data frame with rows (neurons * condition) and 3 columns
  num_neurons <- model[["grid"]][["xdim"]]*model[["grid"]][["ydim"]]
  neuron_size <- as.data.frame(matrix(nrow = length(data_list)*num_neurons, ncol = 3))
  names(neuron_size) <- c("Neuron", "size", "condition") # Set column names
  # Populate the Neuron and condition columns
  neuron_size$Neuron <- 1:num_neurons
  all_condition <- c() # Initialize an empty vector to hold the condition names
  # Iterate over the condition names and repeat each name num_neurons times
  for (i in seq_along(condition_names)) {
    all_condition <- c(all_condition, rep(condition_names[i], num_neurons))
  }
  neuron_size$condition <- all_condition
  # Loop through each neuron and condition combination, and count the number of rows
  #in the classes data frame where the neuron matches the current iteration.
  n=1
  for (i in 1:length(data_list)) {
    for (j in 1:num_neurons) {
      neuron_size[n, "size"] <- length(stats::na.omit(classes[classes[, condition_names[i]] == j, condition_names[i]]))
      n=n+1
    }
  }

  # Return mappings and classes as a named list
  return(list(mappings = mappings, classes = classes, neurons_sizes = neuron_size))
}
