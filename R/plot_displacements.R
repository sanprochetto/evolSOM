#' Create a Graph Visualization of Displacements
#'
#' This function creates a graph visualization of displacements based on the mappings and links data.
#'
#' @param mappings A list containing the mappings and neuron sizes. From create_mappings().
#' @param links A data frame containing the network edges. From create_net_edges().
#' @param reference_species A character vector specifying the reference species.
#' @param layout The layout of the graph. From igraph parameter: "circle", "linear", "grid", etc. Default is "circle".
#' @param node_order An optional vector specifying the order of nodes in the graph. Default is NULL.
#' @param color_edges A character string specifying the column name for edge colors.
#' @param color_scale An optional vector specifying the color scale for the edges. Default is NULL.
#'
#' @return A ggplot object representing the graph visualization of displacements.
#' @import ggraph
#' @export

plot_displacements <- function(mappings,
                                links,
                                reference_species,
                                layout="circle",
                                node_order=NULL,
                                color_edges,
                                color_scale=NULL) {
  nodes <- mappings[["neurons_sizes"]][mappings[["neurons_sizes"]]$species %in% reference_species,1:2]
  # Set node order if specified
  if (!is.null(node_order)) {
    nodes$Neuron <- factor(nodes$Neuron, levels = node_order)
    nodes <- nodes[order(nodes$Neuron), ]
  }
  # Create graph object
  net <- igraph::graph_from_data_frame(d = links, vertices = nodes, directed = T)
  net <- igraph::simplify(net, remove.multiple = F, remove.loops = T) # Removing loops
  # Set edge width based on weight
  igraph::E(net)$width <- igraph::E(net)$weight
  # Create ggplot2 graph
  figure <- ggraph::ggraph(net, layout = layout) +
    # Add edges
    ggraph::geom_edge_fan(ggplot2::aes(color = !!rlang::sym(color_edges),
                      width = igraph::E(net)$width),
                  strength = 0.5, lineend = "round",
                  arrow = ggplot2::arrow(angle = 20, length = ggplot2::unit(0.35, "inches"),
                                ends = "last", type = "closed")) +
    ggraph::scale_edge_width_continuous(range = c(0.5, 8), guide = "none") +
    # Add nodes
    ggraph::geom_node_point(ggplot2::aes(size = nodes$size), color = 'black', fill = '#F5F3EE', shape = 21) +
    ggplot2::scale_size_continuous(range = c(8.3, 26), name = "Neuron Population") +
    # Add node labels
    ggraph::geom_node_text(ggplot2::aes(label = nodes$Neuron), color = "black", repel = F, size = 7) +
    ggplot2::theme_void()

  # Add color scale if specified
  if (!is.null(color_scale)) {
    figure <- figure + ggraph::scale_edge_color_manual(values = color_scale, name = "Displacement Type")
  }

  return(figure)
}
