#' @export
plot <- function(graph, ...) UseMethod("plot")
#' @export
plot.graph <- function(
  graph,
  vertex_color = NULL,
  vertex_size = NULL,
  edge_color = NULL,
  edge_width = "weight",
  log_edge_width = FALSE,
  vertex_order = NULL,
  vertex_palette = NULL
) {
  comps <- igraph::components(graph$graph)
  main_comp_vids <- which(comps$membership == which.max(comps$csize))
  plot_graph <- igraph::induced_subgraph(graph$graph, vids = main_comp_vids)

  plot_graph <- plot_graph |>
    set_vertex_color(vertex_color = vertex_color, custom_palette = vertex_palette, custom_order = vertex_order) |>
    set_vertex_size(vertex_size = vertex_size) |>
    set_edge_color(edge_color = edge_color, custom_palette = NULL) |>
    set_edge_width(edge_width = edge_width, log_edge_width = log_edge_width)

  ggraph::ggraph(plot_graph,
    layout = "centrality",
    centrality = tidygraph::centrality_degree()
  ) +
    ggraph::geom_edge_link(ggplot2::aes(edge_width = width, edge_color = color),
                           edge_alpha = 0.7, show.legend = FALSE) +
    ggraph::geom_node_point(ggplot2::aes(color = color, size = size),
                            show.legend = FALSE) +
    ggraph::geom_node_text(
      ggplot2::aes(
        label = ifelse(isna, NA, name)
      ),
      size = 6,
      repel = TRUE
    ) +
    ggplot2::scale_size_identity() +
    ggplot2::coord_fixed() +
    add_legend(graph$graph, vertex_color, edge_color) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0)
    ) +
    ggplot2::coord_cartesian(clip = "off")
}

#' @export
plot_legend_only <- function(graph, ...) UseMethod("plot_legend_only")

#' @export
plot_legend_only <- function(
  graph,
  vertex_color = NULL,
  vertex_order = NULL,
  vertex_palette = NULL,
  edge_color = NULL
) {
  comps <- igraph::components(graph$graph)
  main_comp_vids <- which(comps$membership == which.max(comps$csize))
  graph$graph <- igraph::induced_subgraph(graph$graph, vids = main_comp_vids)

  graph <- graph |>
    set_vertex_color(vertex_color = vertex_color, custom_palette = vertex_palette, custom_order = vertex_order) |>
    set_edge_color(edge_color = edge_color, custom_palette = NULL)

  vertex_labels <- unique(igraph::V(graph$graph)$vertex_name)
  if (!is.null(vertex_order)) {
    vertex_labels <- intersect(vertex_order, vertex_labels)
    vertex_legend_df <- data.frame(
      label = factor(vertex_labels, levels = vertex_order),
      color = igraph::V(graph$graph)$color[match(vertex_labels, igraph::V(graph$graph)$vertex_name)]
    )
  } else {
    vertex_legend_df <- data.frame(
      label = vertex_labels,
      color = igraph::V(graph$graph)$color[match(vertex_labels, igraph::V(graph$graph)$vertex_name)]
    )
  }

  edge_legend_df <- data.frame(
    label = unique(igraph::E(graph$graph)$edge_name),
    color = unique(igraph::E(graph$graph)$color)
  )

  vertex_legend_plot <- ggplot2::ggplot(vertex_legend_df, ggplot2::aes(x = 1, y = label, color = label)) +
    ggplot2::geom_point(size = 6) +
    ggplot2::scale_color_manual(values = setNames(vertex_legend_df$color, vertex_legend_df$label), name = vertex_color) +
    ggplot2::theme_void()

  edge_legend_plot <- ggplot2::ggplot(edge_legend_df, ggplot2::aes(x = 1, y = label, color = label)) +
    ggplot2::geom_segment(ggplot2::aes(xend = 2, yend = label), size = 2) +
    ggplot2::scale_color_manual(values = setNames(edge_legend_df$color, edge_legend_df$label), name = edge_color) +
    ggplot2::theme_void()

  cowplot::ggdraw(
    cowplot::plot_grid(
      cowplot::get_legend(vertex_legend_plot),
      cowplot::get_legend(edge_legend_plot),
      ncol = 2,
      rel_heights = c(1, 1)
    )
  ) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 0, r = 40, b = 0, l = 40)
    ) +
    ggplot2::coord_cartesian(clip = "off")
}

#' @export
plot_top_vertices <- function(graph, ...) UseMethod("plot_top_vertices")
#' @export
plot_top_vertices.graph <- function(
  graph,
  n = 10,
  edge_color = NULL,
  edge_width = "weight",
  vertex_color = NULL,
  vertex_size = NULL,
  log_edge_width = FALSE,
  vertex_palette = NULL,
  vertex_order = NULL,
  centrality_method = "degree"
) {
  comps <- igraph::components(graph$graph)
  main_comp_vids <- which(comps$membership == which.max(comps$csize))
  graph$graph <- igraph::induced_subgraph(graph$graph, vids = main_comp_vids)

  graph <- graph |>
    set_edge_width(edge_width = edge_width, log_edge_width = log_edge_width) |>
    set_edge_color(edge_color = edge_color) |>
    set_vertex_color(vertex_color = vertex_color, custom_palette = vertex_palette, custom_order = vertex_order) |>
    set_vertex_size(vertex_size = vertex_size)

  centrality <- get_centrality(graph, method = centrality_method)
  if (length(centrality) < n) {
    n <- length(centrality)
  }
  top_vertices <- order(centrality, decreasing = TRUE)[1:n]

  subgraph <- igraph::induced_subgraph(graph$graph, vids = top_vertices)

  layout_df <- get_circle_layout(subgraph)

  ggraph::ggraph(subgraph, layout = "manual", x = layout_df$x, y = layout_df$y) +
    ggraph::geom_edge_arc(
      ggplot2::aes(circular = TRUE,
                   edge_width = width,
                   color = color),
      alpha = 0.7,
      show.legend = TRUE
    ) +
    ggraph::geom_node_point(ggplot2::aes(color = color, size = size), show.legend = TRUE) +
    ggraph::geom_node_text(
      ggplot2::aes(
        label = format_names(name),
        angle = layout_df$label_angle,
        hjust = ifelse(layout_df$label_degree > pi / 2 | layout_df$label_degree < -pi / 2, 1.2, -0.2)
      ),
      size = 3,
      color = "black",
      show.legend = FALSE
    ) +
    add_legend(graph = subgraph, vertex_color = vertex_color, edge_color = edge_color) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 50, r = 400, b = 50, l = 100),
      legend.position = "inside",
      legend.position.inside = c(1.5, 0.5)
    ) +
    ggplot2::coord_cartesian(clip = "off")

}


add_legend <- function(graph, vertex_color = NULL, edge_color = NULL) {
  list(
    ggplot2::scale_color_identity(
      guide = "legend",
      name = vertex_color,
      labels = igraph::V(graph)$vertex_name,
      breaks = igraph::V(graph)$color
    ),
    ggraph::scale_edge_color_identity(
      guide = "legend",
      name = edge_color,
      labels = igraph::E(graph)$edge_name,
      breaks = igraph::E(graph)$color
    ),
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(size = 6, edge_width = 0)
      ),
      edge_color = ggplot2::guide_legend(
        override.aes = list(size = 0, stroke = 0, edge_width = 3)
      ),
      size = "none",
      edge_width = "none"
    )
  )
}


#' @export
get_palette <- function(...) UseMethod("get_palette")
#' @export
get_palette.default <- function(alpha = 1, n = NULL) {
  base_cols <- grDevices::adjustcolor(c(
    "#1E90FF", "#008000", "#E31A1C", "#6A3D9A", "#FF7F00",
    "#FFD700", "#87CEEB", "#FB9A99", "#98FB98", "#CAB2D6",
    "#FDBF6F", "#F0E68C", "#800000", "#DA70D6", "#00BFFF",
    "#FF1493", "#0000FF", "#000000", "#4682B4", "#00CED1",
    "#00FF00", "#808000", "#FFFF00", "#FF8C00", "#A52A2A",
    "#B22222", "#20B2AA", "#FF6347", "#7B68EE", "#3CB371",
    "#B8860B", "#8B008B", "#556B2F", "#FF4500", "#2E8B57",
    "#D2691E", "#8FBC8F", "#9932CC", "#BDB76B", "#4169E1",
    "#DC143C", "#00FA9A", "#FFDAB9", "#8B0000", "#E9967A",
    "#483D8B", "#ADFF2F", "#FFB6C1", "#CD5C5C", "#40E0D0"
  ), alpha.f = alpha)
  if (!is.null(n) && n > length(base_cols)) {
    return(grDevices::rainbow(n, s = 0.8, v = 0.9, alpha = alpha))
  }
  base_cols[seq_len(n)]
}
#' @export
get_palette.graph <- function(graph, vertex_attr = NULL, edge_attr = NULL, alpha = 1) {
  get_palette(graph = graph$graph, vertex_attr = vertex_attr, edge_attr = edge_attr, alpha = alpha)
}
#' @export
get_palette.igraph <- function(graph, vertex_attr = NULL, edge_attr = NULL, alpha = 1) {
  if (!is.null(vertex_attr) && !is.null(edge_attr)) {
    cli::cli_abort(c(
      "x" = "You can only pass one of `vertex_attr` or `edge_attr` to get_palette, not both.",
      "i" = "Please choose one to get the corresponding palette."
    ))
  } else if (!is.null(vertex_attr)) {
    attr_vals <- igraph::vertex_attr(graph, vertex_attr)
  } else if (!is.null(edge_attr)) {
    attr_vals <- igraph::edge_attr(graph, edge_attr)
  } else {
    return(get_palette(alpha = alpha))
  }
  if (is.factor(attr_vals)) {
    attr_vals <- droplevels(attr_vals)
    n <- nlevels(attr_vals)
    levs <- levels(attr_vals)
  } else {
    levs <- sort(unique(attr_vals))
    n <- length(levs)
  }
  colors <- get_palette(alpha = alpha, n = n)
  names(colors) <- levs
  colors
}


#' @export
set_vertex_color <- function(graph, vertex_color = NULL, custom_palette = NULL, custom_order = NULL) UseMethod("set_vertex_color")
#' @export
set_vertex_color.graph <- function(graph, vertex_color = NULL, custom_palette = NULL, custom_order = NULL) {
  graph$graph <- set_vertex_color.igraph(graph$graph, vertex_color = vertex_color, custom_palette = custom_palette, custom_order = custom_order)
  graph
}
#' @export
set_vertex_color.igraph <- function(graph, vertex_color = NULL, custom_palette = NULL, custom_order = NULL) {
  vertex_isna <- rep(FALSE, igraph::vcount(graph))
  if (!is.null(vertex_color) && vertex_color %in% igraph::vertex_attr_names(graph)) {
    vertex_vals <- igraph::vertex_attr(graph, vertex_color)
    vertex_names <- igraph::vertex_attr(graph, vertex_color)
    if (!is.null(custom_order)) {
      vertex_vals <- factor(vertex_vals, levels = custom_order)
    } else {
      custom_order <- unique(as.character(vertex_vals))
    }
    if (is.numeric(vertex_vals)) {
      ord <- order(vertex_vals)
      vertex_vals <- vertex_vals[ord]
      vertex_names <- vertex_names[ord]
      vertex_colors <- scales::col_numeric("Blues", domain = NULL)(vertex_vals)
    } else {
      if (is.null(custom_palette)) {
        pal <- get_palette.igraph(graph, vertex_attr = vertex_color, alpha = 0.8)
        vertex_colors <- pal[as.character(vertex_vals)]
        vertex_isna <- is.na(vertex_colors)
        vertex_colors[vertex_isna] <- grDevices::adjustcolor("gray", alpha.f = 0.4)
      } else {
        check_palette_length(custom_palette, custom_order)
        pal <- setNames(unlist(custom_palette), custom_order)
        vertex_colors <- pal[as.character(vertex_vals)]
        vertex_isna <- is.na(vertex_colors)
        vertex_colors[vertex_isna] <- grDevices::adjustcolor("gray", alpha.f = 0.4)
      }
    }
  } else {
    vertex_colors <- rep(grDevices::adjustcolor("gray", alpha.f = 0.4), igraph::vcount(graph))
    vertex_names <- rep("Vertex", igraph::vcount(graph))
  }
  igraph::V(graph)$color <- vertex_colors
  igraph::V(graph)$vertex_name <- vertex_names
  igraph::V(graph)$isna <- vertex_isna
  graph
}


#' @export
set_vertex_size <- function(graph, vertex_size = NULL) UseMethod("set_vertex_size")
#' @export
set_vertex_size.graph <- function(graph, vertex_size = NULL) {
  graph$graph <- set_vertex_size.igraph(graph$graph, vertex_size = vertex_size)
  graph
}
#' @export
set_vertex_size.igraph <- function(graph, vertex_size = NULL) {
  if (!is.null(vertex_size) && vertex_size %in% igraph::vertex_attr_names(graph)) {
    vertex_vals <- igraph::vertex_attr(graph, vertex_size)
    if (is.numeric(vertex_vals)) {
      vertex_vals <- (vertex_vals - min(vertex_vals)) / (max(vertex_vals) - min(vertex_vals)) * (6 - 1) + 1
      igraph::V(graph)$size <- vertex_vals
    } else {
      cli::cli_abort(c(
        "x" = "The `vertex_size` attribute must be numeric.",
        "i" = "Please provide a numeric attribute for vertex sizes."
      ))
    }
  } else {
    igraph::V(graph)$size <- 5
  }
  graph
}


#' @export
set_edge_color <- function(graph, edge_color = NULL, custom_palette = NULL) UseMethod("set_edge_color")
#' @export
set_edge_color.graph <- function(graph, edge_color = NULL, custom_palette = NULL) {
  graph$graph <- set_edge_color.igraph(graph$graph, edge_color = edge_color, custom_palette = custom_palette)
  graph
}
#' @export
set_edge_color.igraph <- function(graph, edge_color = NULL, custom_palette = NULL) {
  if (!is.null(edge_color) && edge_color %in% igraph::edge_attr_names(graph)) {
    edge_vals <- igraph::edge_attr(graph, edge_color)
    edge_names <- igraph::edge_attr(graph, edge_color)
    if (is.numeric(edge_vals)) {
      ord <- order(edge_vals)
      edge_vals <- edge_vals[ord]
      edge_names <- edge_names[ord]
      edge_colors <- scales::col_numeric("Blues", domain = NULL)(edge_vals)
    } else {
      if (is.null(custom_palette)) {
        pal <- get_palette.igraph(graph, edge_attr = edge_color, alpha = 0.8)
        edge_colors <- pal[as.character(edge_vals)]
        edge_colors[is.na(edge_colors)] <- "gray"
      } else {
        pal <- unlist(custom_palette)
        edge_colors <- pal[as.character(edge_vals)]
        edge_colors[is.na(edge_colors)] <- "gray"
      }
    }
  } else {
    edge_colors <- rep("gray", igraph::ecount(graph))
    edge_names <- rep("Edge", igraph::ecount(graph))
  }
  igraph::E(graph)$color <- edge_colors
  igraph::E(graph)$edge_name <- edge_names
  graph
}


#' @export
set_edge_width <- function(graph, edge_width = "weight", log_edge_width = FALSE) UseMethod("set_edge_width")
#' @export
set_edge_width.graph <- function(graph, edge_width = "weight", log_edge_width = FALSE) {
  graph$graph <- set_edge_width.igraph(graph$graph, edge_width = edge_width, log_edge_width = log_edge_width)
  graph
}
#' @export
set_edge_width.igraph <- function(graph, edge_width = "weight", log_edge_width = FALSE) {
  if (!is.null(edge_width) && edge_width %in% igraph::edge_attr_names(graph)) {
    edge_vals <- igraph::edge_attr(graph, edge_width)
    if (is.numeric(edge_vals)) {
      if (log_edge_width) {
        igraph::E(graph)$width <- log2(edge_vals + 0.1)
      } else {
        edge_vals <- edge_vals / max(edge_vals) * 5
        igraph::E(graph)$width <- edge_vals
      }
    } else {
      cli::cli_abort(c(
        "x" = "The `edge_width` attribute must be numeric.",
        "i" = "Please provide a numeric attribute for edge widths."
      ))
    }
  } else {
    igraph::E(graph)$width <- 1
  }
  graph
}


#' @export
get_circle_layout <- function(graph) UseMethod("get_circle_layout")
#' @export
get_circle_layout.igraph <- function(graph) {
  if (is.null(igraph::V(graph)$color)) {
    vertex_order <- igraph::V(graph)$name
  } else {
    vertex_order <- igraph::V(graph)$name[order(igraph::V(graph)$color)]
  }
  layout_coords <- igraph::layout_in_circle(graph, order = vertex_order)
  colnames(layout_coords) <- c("x", "y")
  rownames(layout_coords) <- igraph::V(graph)$name

  layout_df <- as.data.frame(layout_coords)
  layout_df$name <- rownames(layout_coords)
  layout_df$label_degree <- -atan2(layout_coords[, 2], layout_coords[, 1])
  layout_df$label_dist <- 1 + abs(cos(layout_df$label_degree)) * 2
  layout_df$label_angle <- ifelse(
    (cos(layout_df$label_degree) > 0 & sin(layout_df$label_degree) > 0) |
      (cos(layout_df$label_degree) < 0 & sin(layout_df$label_degree) < 0),
    -45,
    45
  )
  layout_df
}
