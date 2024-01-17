#' Create network edges
#'
#' This function creates network edges based on mappings and class assignments.
#'
#' @param mappings A named list containing mappings, classes, and neuron sizes.Output from create_mappings
#' @param reference_name The name of the species or samples to be used as reference.
#' @param test_name The name of the species or samples to be used as test variable.
#' @param add_disp_type Logical value indicating whether to add displacement types to the network edges.
#' @param disp_type A data frame containing the displacement types. Valid if add_disp_type = TRUE
#' @return A data frame containing the network links and edges. Object used for displacements visualization.
#' @import dplyr
#' @export
net_edges <- function(mappings,
                             reference_name,
                             test_name,
                             add_disp_type = FALSE,
                             disp_type = displacements){
  num_neurons = length(unique(mappings[["neurons_sizes"]][["Neuron"]]))
  # create the empty data frame
  net_edges <- as.data.frame(matrix(nrow = num_neurons^2, ncol = 7))
  # set column names
  names(net_edges) <- c("Source", "Target", "Type", "Id","weight", "disp%", "disp_type")
  # assign values to columns
  net_edges$Source <- rep(1:num_neurons, each=num_neurons)
  net_edges$Target<- 1:num_neurons
  net_edges$Type <- "Directed"
  # calculate weight and disp%
  n=1
  for (i in 1:num_neurons){
    for (m in 1:num_neurons){
      x<-mappings[["classes"]][mappings[["classes"]][,reference_name] %in% i,]
      net_edges[n,5] <- nrow(x[x[,test_name] %in% m,])
      n=n+1
    }  }
  n=1
  for (i in 1:num_neurons){
    for (m in 1:num_neurons){
      net_edges[n,6] <-  net_edges[n,5]*100/ sum(net_edges[net_edges[,1] %in% i,"weight"])
      n=n+1
    } }
  #net_edges$disp_type <- c("none","flip","delay","early","other")
  if (add_disp_type == TRUE) {
    displacements = displacements
    n=1
    for (i in 1:num_neurons) {
      for (m in 1:num_neurons) {
        a <- displacements[which(displacements$neuron_ori == i & displacements$neuron_end == m),"disp_type"]
        if (identical(a, character(0)) == FALSE){
          net_edges[n,"disp_type"] <- a
        }
        n=n+1 }    }
    #replace NA for "other"
    net_edges$disp_type <- net_edges$disp_type %>% tidyr::replace_na("other")
  }

  return(net_edges)
}
