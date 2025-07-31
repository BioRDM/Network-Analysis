#' @export
plot <- function(graph, ...) UseMethod("plot")
#' @export
plot.graph <- function(graph, centrality_method = "degree") {
  colors <- get_palette(graph, attr = "community", alpha = 0.6)
  centrality <- get_centrality(graph, method = centrality_method)

  if (centrality_method == "none") {
    vertex_size <- 4 * rep(1, igraph::vcount(graph$graph))
  } else {
    vertex_size <- 3 + (centrality / max(centrality)) * 10
  }
  igraph::V(graph$graph)$size <- vertex_size

  layout_df <- graph$layout

  ggraph::ggraph(graph$graph, layout = "manual", x = layout_df$x, y = layout_df$y) +
    ggraph::geom_edge_link(color = "gray", width = 0.8, alpha = 0.7) +
    ggraph::geom_node_point(ggplot2::aes(color = community, size = size), show.legend = FALSE) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::theme_void()
}


#' @export
plot_cutpoints <- function(graph, ...) UseMethod("plot_cutpoints")
#' @export
plot_cutpoints.graph <- function(graph, centrality_method = "degree") {
  cutpoints <- get_cutpoints(graph)
  n_cutpoints <- length(cutpoints)

  vertex_colors <- rep(grDevices::adjustcolor("grey", alpha.f = 0.6), igraph::vcount(graph$graph))
  if (n_cutpoints > 0) {
    colors <- get_palette(alpha = 1)[seq_len(n_cutpoints)]
    vertex_colors[as.integer(cutpoints)] <- colors
  } else {
    colors <- character(0)
  }
  igraph::V(graph$graph)$cutpoint_color <- vertex_colors

  centrality <- get_centrality(graph, method = centrality_method)
  if (centrality_method == "none") {
    vertex_size <- 4 * rep(1, igraph::vcount(graph$graph))
  } else {
    vertex_size <- 3 + (centrality / max(centrality)) * 10
  }
  igraph::V(graph$graph)$size <- vertex_size

  layout_df <- graph$layout
  rownames(layout_df) <- igraph::V(graph$graph)$name

  p <- ggraph::ggraph(graph$graph, layout = "manual", x = layout_df$x, y = layout_df$y) +
    ggraph::geom_edge_link(color = "gray", width = 0.8, alpha = 0.7) +
    ggraph::geom_node_point(ggplot2::aes(color = cutpoint_color, size = size), show.legend = TRUE) +
    ggplot2::scale_color_identity(guide = "legend",
                                  labels = names(cutpoints),
                                  breaks = igraph::V(graph$graph)$cutpoint_color[as.integer(cutpoints)]) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom", legend.title.position = "top")

  if (n_cutpoints > 0) {
    legend_df <- data.frame(
      name = names(cutpoints),
      color = igraph::V(graph$graph)$cutpoint_color[as.integer(cutpoints)]
    )
    p <- p + ggplot2::guides(
      size = "none",
      color = ggplot2::guide_legend(
        ncol = 3,
        override.aes = list(
          color = legend_df$color,
          size = 4
        ),
        title = "Cutpoint authors",
        label.theme = ggplot2::element_text(size = 10)
      )
    )
  }
  p
}


#' @export
plot_top_authors <- function(graph, ...) UseMethod("plot_top_authors")
#' @export
plot_top_authors.graph <- function(graph, n = 10) {
  centrality <- get_centrality(graph, method = "degree")
  if (length(centrality) < n) {
    n <- length(centrality)
  }
  top_authors <- order(centrality, decreasing = TRUE)[1:n]
  subgraph <- igraph::induced_subgraph(graph$graph, vids = top_authors)

  comm <- get_communities(graph)
  top_authors_communities <- comm[top_authors]
  igraph::V(subgraph)$community <- as.factor(top_authors_communities)

  colors <- get_palette(graph, attr = "community", alpha = 0.6)

  layout_coords <- igraph::layout_in_circle(subgraph)
  colnames(layout_coords) <- c("x", "y")
  rownames(layout_coords) <- igraph::V(subgraph)$name
  label_degrees <- -atan2(layout_coords[, 2], layout_coords[, 1])
  vertex_label_dist <- 1 + abs(cos(label_degrees)) * 2

  layout_df <- as.data.frame(layout_coords)
  layout_df$name <- rownames(layout_coords)
  layout_df$label_degree <- label_degrees
  layout_df$label_dist <- vertex_label_dist

  edge_df <- igraph::as_data_frame(subgraph, what = "edges")
  if ("weight" %in% colnames(edge_df)) {
    igraph::E(subgraph)$edge_width <- log2(igraph::E(subgraph)$weight + 0.1)
  } else {
    igraph::E(subgraph)$edge_width <- 1
  }

  ggraph::ggraph(subgraph, layout = "manual", x = layout_df$x, y = layout_df$y) +
    ggraph::geom_edge_arc(
      ggplot2::aes(circular = TRUE, edge_width = edge_width),
      color = "gray",
      alpha = 0.7
    ) +
    ggraph::geom_node_point(ggplot2::aes(color = community, size = 6), show.legend = FALSE) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_size_identity() +
    ggraph::geom_node_text(
      ggplot2::aes(
        label = format_names(name),
        hjust = ifelse(layout_df$label_degree > pi/2 | layout_df$label_degree < -pi/2, 1.2, -0.2)
      ),
      size = 3,
      color = "black",
      show.legend = FALSE
    ) +
    ggplot2::theme_void() +
    ggplot2::guides(size = "none", edge_width = "none") +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 0, r = 100, b = 0, l = 100)
    ) +
    ggplot2::coord_cartesian(clip = "off")
}


#' @export
get_palette <- function(...) UseMethod("get_palette")
#' @export
get_palette.default <- function(alpha = 1) {
  grDevices::adjustcolor(c(
    "#1E90FF", "#E31A1C", "#008000", "#6A3D9A", "#FF7F00",
    "#FFD700", "#87CEEB", "#FB9A99", "#98FB98", "#CAB2D6",
    "#FDBF6F", "#B3B3B3", "#F0E68C", "#800000", "#DA70D6",
    "#FF1493", "#0000FF", "#000000", "#4682B4", "#00CED1",
    "#00FF00", "#808000", "#FFFF00", "#FF8C00", "#A52A2A"
  ), alpha.f = alpha)
}
#' @export
get_palette.graph <- function(graph, attr = "community", alpha = 1) {
  attr_vals <- igraph::vertex_attr(graph$graph, attr)
  if (is.factor(attr_vals)) {
    n <- nlevels(attr_vals)
    levs <- levels(attr_vals)
  } else {
    levs <- sort(unique(attr_vals))
    n <- length(levs)
  }
  colors <- get_palette(alpha = alpha)[seq_len(n)]
  names(colors) <- levs
  colors
}
