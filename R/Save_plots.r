save_plots <- function(graph, config, paths, date_range) {
  if (!is.null(config$node_properties$file_path)) {
    vertex_color <- config$node_properties$color
  } else {
    vertex_color <- "community"
  }

                                        # Legend
  p <- plot_legend_only(graph,
                   vertex_color = vertex_color,
                   vertex_order = config$node_properties$order,
                   vertex_palette = config$node_properties$palette,
                   layout = config$plot$layout)
  ggplot2::ggsave(
             filename = paste0(paths$plots, "/Legend_", date_range, ".png"),
             plot = p,
             bg = "white"
           )

                                        # Degree centrality plot
  graph <- set_centrality(graph, method = "degree")
  p <- plot(graph,
            vertex_color = vertex_color,
            vertex_size = "centrality",
            vertex_order = config$node_properties$order,
            vertex_palette = config$node_properties$palette,
            layout = config$plot$layout)
  ggplot2::ggsave(
             filename = paste0(paths$plots, "/Network_degree_", date_range, ".png"),
             plot = p,
             bg = "white"
           )

                                        # Top nodes
  p <- plot_top_vertices(graph,
                    vertex_color = vertex_color,
                    n = 30,
                    vertex_order = config$node_properties$order,
                    vertex_palette = config$node_properties$palette)
  ggplot2::ggsave(
             filename = paste0(paths$plots, "/Top_authors_", date_range, ".png"),
             plot = p,
             bg = "white",
             width = 14,
             height = 8
           )

                                        # Cutpoints
  graph <- set_cutpoints(graph)
  p <- plot(graph, vertex_color = "cutpoint", layout = config$plot$layout)
  ggplot2::ggsave(
             filename = paste0(paths$plots, "/Cutpoints_", date_range, ".png"),
             plot = p,
             bg = "white"
           )

                                        # Betweenness centrality
  graph <- set_centrality(graph, method = "betweenness")
  p <- plot(graph,
            vertex_color = vertex_color,
            vertex_size = "centrality",
            vertex_order = config$node_properties$order,
            vertex_palette = config$node_properties$palette,
            layout = config$plot$layout)
    ggplot2::ggsave(
             filename = paste0(paths$plots, "/Network_betweenness_", date_range, ".png"),
             plot = p,
             bg = "white"
           )

                                        # Harmonic centrality
  graph <- set_centrality(graph, method = "harmonic")
  p <- plot(graph,
       vertex_color = vertex_color,
       vertex_size = "centrality",
       vertex_order = config$node_properties$order,
       vertex_palette = config$node_properties$palette,
       layout = config$plot$layout)
  ggplot2::ggsave(
             filename = paste0(paths$plots, "/Network_harmonic_", date_range, ".png"),
             plot = p,
             bg = "white"
           )

                                        # No centrality
  p <- plot(graph,
       vertex_color = vertex_color,
       vertex_size = NULL,
       vertex_order = config$node_properties$order,
       vertex_palette = config$node_properties$palette,
       layout = config$plot$layout)
  ggplot2::ggsave(
             filename = paste0(paths$plots, "/Network_no_centrality_", date_range, ".png"),
             plot = p,
             bg = "white"
           )

                                        # No names
  graph <- set_centrality(graph, method = "degree")
  p <- plot(graph,
            vertex_color = vertex_color,
            vertex_size = "centrality",
            vertex_order = config$node_properties$order,
            vertex_palette = config$node_properties$palette,
            display_names = FALSE,
            layout = config$plot$layout)
  ggplot2::ggsave(
             filename = paste0(paths$plots, "/Network_no_names_", date_range, ".png"),
             plot = p,
             bg = "white"
           )
}
