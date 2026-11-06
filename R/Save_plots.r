save_plot <- function(plot, path, title, date_range, formats, width = NA, height = NA) {
  for (fmt in formats) {
    ggplot2::ggsave(
      filename = paste0(path, "/", title, "_", date_range, ".", fmt),
      plot = plot,
      bg = "white",
      width = width,
      height = height
    )
  }
}

save_report_plots <- function(graph, config, paths, date_range) {
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
  save_plot(p, path = paths$plots, title = "Legend", date_range = date_range, formats = config$plot$formats)

  # Degree centrality plot
  graph <- set_centrality(graph, method = "degree")
  p <- plot(graph,
            vertex_color = vertex_color,
            vertex_size = "centrality",
            vertex_order = config$node_properties$order,
            vertex_palette = config$node_properties$palette,
            layout = config$plot$layout)
  save_plot(p, path = paths$plots, title = "Network_degree", date_range = date_range, formats = config$plot$formats)

  # Top nodes
  p <- plot_top_vertices(graph,
                         vertex_color = vertex_color,
                         n = 30,
                         vertex_order = config$node_properties$order,
                         vertex_palette = config$node_properties$palette)
  save_plot(p, path = paths$plots, title = "Top_authors", date_range = date_range, formats = config$plot$formats, width = 14, height = 8)

  # Cutpoints
  graph <- set_cutpoints(graph)
  p <- plot(graph, vertex_color = "cutpoint", layout = config$plot$layout)
  save_plot(p, path = paths$plots, title = "Cutpoints", date_range = date_range, formats = config$plot$formats)

  # Betweenness centrality
  graph <- set_centrality(graph, method = "betweenness")
  p <- plot(graph,
            vertex_color = vertex_color,
            vertex_size = "centrality",
            vertex_order = config$node_properties$order,
            vertex_palette = config$node_properties$palette,
            layout = config$plot$layout)
  save_plot(p, path = paths$plots, title = "Network_betweenness", date_range = date_range, formats = config$plot$formats)

  # Harmonic centrality
  graph <- set_centrality(graph, method = "harmonic")
  p <- plot(graph,
            vertex_color = vertex_color,
            vertex_size = "centrality",
            vertex_order = config$node_properties$order,
            vertex_palette = config$node_properties$palette,
            layout = config$plot$layout)
  save_plot(p, path = paths$plots, title = "Network_harmonic", date_range = date_range, formats = config$plot$formats)

  # No centrality
  p <- plot(graph,
            vertex_color = vertex_color,
            vertex_size = NULL,
            vertex_order = config$node_properties$order,
            vertex_palette = config$node_properties$palette,
            layout = config$plot$layout)
  save_plot(p, path = paths$plots, title = "Network_no_centrality", date_range = date_range, formats = config$plot$formats)

  # No names
  graph <- set_centrality(graph, method = "degree")
  p <- plot(graph,
            vertex_color = vertex_color,
            vertex_size = "centrality",
            vertex_order = config$node_properties$order,
            vertex_palette = config$node_properties$palette,
            display_names = FALSE,
            layout = config$plot$layout)
  save_plot(p, path = paths$plots, title = "Network_no_names", date_range = date_range, formats = config$plot$formats)
}
