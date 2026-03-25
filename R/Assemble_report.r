#' @export
assemble_report <- function(config_file) {
  summary_stats <- data.frame()

  config <- read_config(config_file)
  paths <- Paths(config)
  file.copy(config_file, paste0(paths$dataset))

  df <- utils::read.csv(paths$input_file, stringsAsFactor = FALSE) |>
    unnest_vertex_column(config$data$node_id, config$data$node_delimiter) |>
    dplyr::distinct(.data[[config$data$edge_id]], .data[[config$data$node_id]], .keep_all = TRUE) |>
    apply_filters(config$data$filters)

  if (!is.null(config$node_properties$file_path)) {
    node_props <- utils::read.csv(config$node_properties$file_path, stringsAsFactor = FALSE) |>
      apply_filters(config$node_properties$filters)

    check_column(node_props, config$node_properties$node_id)
    check_column(node_props, config$node_properties$color)

    if (config$node_properties$remove_NA) {
      df <- subset_df(df, config$data$node_id, node_props, config$node_properties$node_id)
    }
  }

  years <- get_years_from_to(df, config)

  for (i in seq_along(years$from)) {
    # Set the current year range
    from_year <- years$from[i]
    to_year <- years$to[i]
    date_range <- paste0(from_year, "-", to_year)
    current_df <- filter_by_year(df, config$data$year_column, from_year, to_year)
    print(paste0("Creating report for the period ", from_year, " to ", to_year))

    # Build network data
    network <- network(current_df,
                       vertex_column = config$data$node_id,
                       vertex_delimiter = config$data$node_delimiter,
                       edge_id = config$data$edge_id,
                       year_column = config$data$year_column)

    # Filter the network data
    network <- filter_single_vertices(network)
    if (!is.null(config$data$max_authors_per_paper)) {
      network <- filter_by_vertex_occurrences(network, config$data$max_authors_per_paper)
    }
    if (!is.null(config$data$min_papers_per_author)) {
      network <- filter_infrequent_vertices(network, config$data$min_papers_per_author)
    }

    # Build the graph
    graph <- graph(network$filtered, directed = FALSE) |>
      build(vertices = network$vertex_column, edges = network$edge_id)

    # If node data is available, add it as a vertex attribute
    # Otherwise, cluster nodes with igraph's "cluster_louvain" method (communities)
    if (!is.null(config$node_properties$file_path)) {
      graph <- set_vertex_attr(graph,
                               name = config$node_properties$color,
                               keys = node_props[[config$node_properties$node_id]],
                               values = node_props[[config$node_properties$color]])
      if (all(is.na(igraph::vertex_attr(graph$graph, config$node_properties$color)))) {
        cli::cli_alert(c("!" = "No match found for node colour.",
                         "i" = "Check that the name format in the node colour table matches that in the main data."))
      }
    } else {
      graph <- set_communities(graph)
    }

    # Save raw and filtered data
    utils::write.csv(network$raw, paste0(paths$data, "/Raw_data_", date_range, ".csv"), row.names = FALSE)
    utils::write.csv(network$filtered, paste0(paths$data, "/Filtered_data_", date_range, ".csv"), row.names = FALSE)

    # Save edges per vertex (before and after filtering)
    save_edges_per_vertex(
      network,
      output_file = paste0(paths$papers_per_author, date_range, ".csv")
    )

    # Save centrality data as csv
    save_centrality_data(graph, paste0(paths$centrality_data, date_range, ".csv"))

    # Save cutpoint names as csv
    save_cutpoint_names(
      graph,
      output_file = paste0(paths$cutpoints, date_range, ".csv")
    )

    # Save vertex attributes as csv
    save_vertex_attributes(
      graph,
      output_file = paste0(paths$vertex_attributes, date_range, ".csv")
    )

    # Compute summary statistics and update summary_stats dataframe
    author_stats <- get_summary_stats(
      graph = graph,
      network = network
    )
    summary_stats <- rbind(summary_stats, author_stats[author_stats$Data == "filtered", ])

    # Generate the pdf report
    print("Exporting PDF...")
    rmarkdown::render(
      paste0(paths$templates, "/Report_template.Rmd"),
      output_file = paste0(paths$dataset, "/Report_", date_range, ".pdf"),
      output_format = "pdf_document",
      quiet = TRUE,
      params = list(
        config = config,
        metadata = config$metadata,
        date_range = date_range,
        network = network,
        graph = graph,
        author_stats = author_stats
      )
    )
    print("PDF exported successfully!")

  }

  # Save summary statistics to a CSV file
  utils::write.table(summary_stats, paths$summary_table, sep = ",", row.names = FALSE, col.names = TRUE)

  # Format the summary statistics for the report
  summary_stats <- format_summary_stats(summary_stats)

  # Export the summary statistics as pdf
  print("Exporting Summary Table...")
  rmarkdown::render(
    paste0(paths$templates, "/Summary_template.Rmd"),
    output_file = paste0(paths$dataset, "/Summary.pdf"),
    output_format = "pdf_document",
    quiet = TRUE,
    params = list(summary_stats = summary_stats, config = config)
  )
  print("Summary Table exported successfully!")
}
