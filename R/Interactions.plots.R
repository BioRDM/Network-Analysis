#' @export
plot_graph.Interactions <- function(interactions, output_file = "output/graph.png") {
  comm <- get_communities(interactions)
  colors <- grDevices::rainbow(length(unique(comm$membership)), alpha = 0.4)

  centrality <- get_centrality(interactions)$degree
  vertex_size <- 1 + (centrality / max(centrality)) * 15

  grDevices::png(filename = output_file, width = 2500, height = 1800, res = 360)

  graphics::par(mfrow = c(1, 1), mar = c(1, 1, 1, 10))
  graphics::plot(
    interactions$graph,
    vertex.label = NA,
    vertex.size = vertex_size,
    vertex.color = colors[comm$membership],
    edge.width = 0.8,
    edge.color = "gray",
    layout = interactions$layout_coords
  )

  most_central_authors <- get_most_central_per_community(interactions)
  most_central_authors <- format_names(most_central_authors)
  if (length(most_central_authors) <= 18) {
    add_graph_legend(leg_x = 1.3, leg_y = 0, leg_items = most_central_authors, leg_colors = colors, leg_title = "Most central author")
  } else {
    print(paste0("Graph plot: removing legend because there are too many communities (", length(most_central_authors), ")."))
  }

  dev.off()
  return(output_file)
}

plot_graph <- function(interactions, output_file) {
  UseMethod("plot_graph", interactions)
}

#' @export
plot_cutpoints.Interactions <- function(interactions, output_file = "output/cutpoint_graph.png") {
  cutpoints <- get_cutpoints(interactions)
  vertex_colors <- ifelse(cutpoints, "red", "lightblue")
  vertex_colors <- grDevices::adjustcolor(vertex_colors, alpha.f = 0.4)

  centrality <- get_centrality(interactions)$degree
  vertex_size <- 1 + (centrality / max(centrality)) * 15

  grDevices::png(filename = output_file, width = 2500, height = 1800, res = 360)

  graphics::par(mfrow = c(1, 1), mar = c(1, 1, 1, 10))
  graphics::plot(
    interactions$graph,
    vertex.label = NA,
    vertex.size = vertex_size,
    vertex.color = vertex_colors,
    edge.width = 0.8,
    edge.color = "gray",
    layout = interactions$layout_coords
  )

  add_graph_legend(leg_x = 1.3, leg_y = 0, leg_items = c("Other node", "Cutpoint"), leg_colors = c("lightblue", "red"), leg_title = "")

  dev.off()
  return(output_file)
}

plot_cutpoints <- function(interactions, output_file) {
  UseMethod("plot_cutpoints", interactions)
}

#' @export
plot_top_authors.Interactions <- function(interactions, n = 10, output_file = "output/top_authors.png") {
  # Identify the top n authors based on centrality
  centrality <- get_centrality(interactions)$degree
  top_authors <- order(centrality, decreasing = TRUE)[1:n]

  # Create a subgraph with the top n authors
  subgraph <- igraph::induced_subgraph(interactions$graph, vids = top_authors)

  # Get community membership for the top authors
  comm <- get_communities(interactions)
  top_authors_communities <- comm$membership[top_authors]
  colors <- grDevices::rainbow(length(unique(comm$membership)), alpha = 0.4)
  vertex_colors <- colors[top_authors_communities]

  # Calculate label degrees to position labels outside the circle
  # Adjust vertex-label distance based on the label's position
  layout_coords <- igraph::layout_in_circle(subgraph)
  label_degrees <- -atan2(layout_coords[, 2], layout_coords[, 1])
  vertex_label_dist <- 1 + abs(cos(label_degrees)) * 2

  # Set up the plot
  grDevices::png(filename = output_file, width = 3000, height = 2500, res = 400)
  graphics::par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))

  # Plot the subgraph in a circular layout with weighted edges
  graphics::plot(
    subgraph,
    layout = igraph::layout_in_circle(subgraph),
    vertex.label = format_names(igraph::V(subgraph)$name),
    vertex.label.dist = vertex_label_dist,
    vertex.label.degree = label_degrees,
    vertex.label.color = "black",
    vertex.size = 10,
    vertex.color = vertex_colors,
    vertex.frame.color = "black",
    edge.width = igraph::E(subgraph)$weight,
    edge.color = "gray",
    edge.curved = 0.2
  )

  dev.off()
  return(output_file)
}

plot_top_authors <- function(interactions, n, output_file) {
  UseMethod("plot_top_authors", interactions)
}

#' @export
add_graph_legend <- function(leg_x, leg_y, leg_items, leg_colors, leg_title = "") {
  par(xpd = NA)
  leg_spread <- 0.12 * length(leg_items) / 2
  leg_y <- seq(leg_y - leg_spread, leg_y + leg_spread, length.out = length(leg_items))
  text(x = leg_x,
       y = max(leg_y) + 0.12,
       labels = leg_title,
       adj = c(0, 0.5),
       cex = 1,
       font = 2)
  points(x = rep(leg_x, length(leg_items)),
         y = leg_y,
         col = "black",
         bg = leg_colors,
         pch = 21,
         cex = 2)
  text(x = rep(leg_x + 0.1, length(leg_items)),
       y = leg_y,
       col = "black",
       labels = leg_items,
       adj = c(0, 0.5),
       cex = 0.8)
}

#' @export
get_graph_coords <- function(graph) {
  return(igraph::layout_(graph, igraph::with_drl(options = list(simmer.attraction = 0))))
}
