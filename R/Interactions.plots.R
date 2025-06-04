#' @export
plot_graph.Interactions <- function(interactions, centrality = "degree", output_file = "output/graph.png") {
  comm <- interactions$communities
  colors <- get_palette(alpha = 0.6)

  size_metric <- switch(centrality,
                        "degree" = get_centrality(interactions)$degree,
                        "betweenness" = get_centrality(interactions)$betweenness,
                        "harmonic" = get_centrality(interactions)$harmonic,
                        "none" = rep(1, igraph::vcount(interactions$graph)))

  if (centrality == "none") {
    vertex_size <- 4 * size_metric
  } else {
    vertex_size <- 3 + (size_metric / max(size_metric)) * 10
  }

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

  most_central_authors <- get_most_central_per_community(interactions, centrality = centrality)
  most_central_authors <- format_names(most_central_authors)
  if (length(most_central_authors) > 20) {
    print(paste0("Graph plot: removing legend because there are too many communities (", length(most_central_authors), ")."))
  } else if (length(most_central_authors) > 0) {
    add_graph_legend(leg_x = 1.3, leg_y = 0, leg_items = most_central_authors, leg_colors = colors, leg_title = "Most central author")
  }

  grDevices::dev.off()
  output_file
}

plot_graph <- function(interactions, centrality, output_file) {
  UseMethod("plot_graph", interactions)
}

#' @export
plot_cutpoints.Interactions <- function(interactions, centrality = "degree", output_file = "output/cutpoint_graph.png") {
  cutpoints <- sna::cutpoints(interactions$network, mode = "graph", return.indicator = TRUE)
  cutpoint_names <- get_cutpoints(interactions)
  colors <- get_palette(alpha = 1)

  if (sum(cutpoints) == 0) {
    grDevices::png(filename = output_file, width = 100, height = 100, res = 72)
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot(1, type = "n", xlab = "", ylab = "", axes = FALSE, xlim = c(0, 1), ylim = c(0, 1))
    graphics::rect(0, 0, 1, 1, col = "white", border = "white")
    grDevices::dev.off()
  } else {
    vertex_colors <- rep(grDevices::adjustcolor("grey", alpha.f = 0.4), length(cutpoints))
    vertex_colors[cutpoints] <- colors[1:sum(cutpoints)]

    size_metric <- switch(centrality,
                          "degree" = get_centrality(interactions)$degree,
                          "betweenness" = get_centrality(interactions)$betweenness,
                          "harmonic" = get_centrality(interactions)$harmonic,
                          "none" = rep(1, igraph::vcount(interactions$graph)))
    if (centrality == "none") {
      vertex_size <- 4 * size_metric
    } else {
      vertex_size <- 3 + (size_metric / max(size_metric)) * 10
    }

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

    if (length(cutpoint_names) > 20) {
      print(paste0("Cutpoint plot: removing legend because there are too many cutpoint authors (", length(cutpoint_names), ")."))
    } else if (length(cutpoint_names) > 0) {
      add_graph_legend(leg_x = 1.3, leg_y = 0, leg_items = cutpoint_names, leg_colors = colors[1:sum(cutpoints)], leg_title = "Cutpoint authors")
    }

    grDevices::dev.off()
  }
  output_file
}

plot_cutpoints <- function(interactions, centrality, output_file) {
  UseMethod("plot_cutpoints", interactions)
}

#' @export
plot_top_authors.Interactions <- function(interactions, n = 10, output_file = "output/top_authors.png") {
  centrality <- get_centrality(interactions)$degree
  if (length(centrality) < n) {
    n <- length(centrality)
  }
  top_authors <- order(centrality, decreasing = TRUE)[1:n]
  subgraph <- igraph::induced_subgraph(interactions$graph, vids = top_authors)

  comm <- interactions$communities
  top_authors_communities <- comm$membership[top_authors]
  colors <- get_palette(alpha = 0.6)
  vertex_colors <- colors[top_authors_communities]

  layout_coords <- igraph::layout_in_circle(subgraph)
  label_degrees <- -atan2(layout_coords[, 2], layout_coords[, 1])
  vertex_label_dist <- 1 + abs(cos(label_degrees)) * 2

  grDevices::png(filename = output_file, width = 3000, height = 2500, res = 400)
  graphics::par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))

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
    edge.width = log2(igraph::E(subgraph)$weight + 0.1),
    edge.color = "gray",
    edge.curved = 0.2
  )

  grDevices::dev.off()
  output_file
}

plot_top_authors <- function(interactions, n, output_file) {
  UseMethod("plot_top_authors", interactions)
}

#' @export
add_graph_legend <- function(leg_x, leg_y, leg_items, leg_colors, leg_title = "") {
  graphics::par(xpd = NA)
  leg_spread <- 0.11 * length(leg_items) / 2
  leg_y <- seq(leg_y - leg_spread, leg_y + leg_spread, length.out = length(leg_items))
  graphics::text(x = leg_x,
                 y = max(leg_y) + 0.12,
                 labels = leg_title,
                 adj = c(0, 0.5),
                 cex = 1,
                 font = 2)
  graphics::points(x = rep(leg_x, length(leg_items)),
                   y = leg_y,
                   col = "black",
                   bg = leg_colors,
                   pch = 21,
                   cex = 2)
  graphics::text(x = rep(leg_x + 0.1, length(leg_items)),
                 y = leg_y,
                 col = "black",
                 labels = leg_items,
                 adj = c(0, 0.5),
                 cex = 0.8)
}

#' @export
get_graph_coords <- function(graph) {
  igraph::layout_(graph, igraph::with_drl(options = list(simmer.attraction = 0)))
}

#' @export
get_palette <- function(alpha = 1) {
  grDevices::adjustcolor(c(
    "#1E90FF", "#E31A1C", "#008000", "#6A3D9A", "#FF7F00",
    "#FFD700", "#87CEEB", "#FB9A99", "#98FB98", "#CAB2D6",
    "#FDBF6F", "#B3B3B3", "#F0E68C", "#800000", "#DA70D6",
    "#FF1493", "#0000FF", "#000000", "#4682B4", "#00CED1",
    "#00FF00", "#808000", "#FFFF00", "#FF8C00", "#A52A2A"
  ), alpha.f = alpha)
}
