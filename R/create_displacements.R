#' Automatic detection of displacement types
#'
#' This function automatically detects displacement types in a trained SOM model.
#'
#' @param model A trained SOM model.
#' @param flip_threshold The threshold for detecting flip displacements.
#' @param delay_threshold The threshold for detecting delay displacements.
#' @return A data frame containing the detected displacement types.
#' @import dplyr
#' @export

create_displacements <- function(model,
                                 flip_threshold = -0.85,
                                 delay_threshold = 0.85) {

  # Extract neuron codes from the model
  neurons <- as.data.frame(t(model[["codes"]][[1]]))
  colnames(neurons) <- 1:ncol(neurons)
  n_neurons <- ncol(neurons)

  # Identify flips
  flip_displacements <- data.frame(neuron_ori = 1:n_neurons,
                                   neuron_end = NA,
                                   disp_type = "flip")
  for (i in 1:n_neurons) {
    correlations <- stats::cor(neurons, (neurons[,i]), use = "complete.obs")

    if (min(correlations) <= flip_threshold) {
      flip_displacements[i,"neuron_end"] <- which.min(correlations)
    }
  }

  # Identify delays
  delay_displacements <- data.frame(neuron_ori = 1:n_neurons,
                                    neuron_end = NA,
                                    disp_type = "delay")
  for (i in 1:n_neurons) {
    correlations <- stats::cor(neurons[,-i], dplyr::lag(neurons[,i]), use = "complete.obs")
    if (max(correlations) >= delay_threshold){
      delay_displacements[i, "neuron_end"] <- as.numeric(row.names(correlations)[which.max(correlations)])
    }
  }

  # Identify early displacements
  early_displacements <- data.frame(neuron_ori = 1:n_neurons,
                                    neuron_end = NA,
                                    disp_type = "early")
  for (i in 1:n_neurons){
    if (i %in% delay_displacements$neuron_end) {
      early_displacements[i,"neuron_end"] <- delay_displacements[delay_displacements[1:n_neurons,2] %in% i,1][1]
    }
  }

  # Identify conservation
  none_displacements <- data.frame(neuron_ori = 1:n_neurons,
                                   neuron_end = 1:n_neurons,
                                   disp_type = "none")

  # Combine all displacement data frames and remove NA rows
  displacements <- stats::na.omit(rbind(flip_displacements, delay_displacements, early_displacements, none_displacements))

  # Check for duplicated and inconsistent rows
  if (anyDuplicated(displacements[,1:2])) {
    # Print a warning message
    warning("Duplicated or inconsistent rows in displacement, try higher tresholds")
  } else {
    # Print a comment message
    message(paste("A total of",nrow(displacements), "displacement types found"))
  }

  # Return the displacements data frame
  return(displacements)
}
