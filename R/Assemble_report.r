#' @export
assemble_report <- function(config, metadata) {
  summary_stats <- data.frame()

  config <- read_config(config)
  metadata <- read_metadata(metadata)

  paths <- Paths(config)

  json_config <- jsonlite::toJSON(config, pretty = TRUE, auto_unbox = TRUE, null = "null")
  write(json_config, file = paste0(paths$dataset, "/config.json"))

  df <- utils::read.csv(paths$input_file, stringsAsFactor = FALSE) |>
    unnest_vertex_column(config$author_column_name, config$author_delimiter)

  years <- get_years_from_to(df, config)

  for (i in seq_along(years$from)) {
    # Set the current year range
    from_year <- years$from[i]
    to_year <- years$to[i]
    date_range <- paste0(from_year, "-", to_year)
    current_df <- filter_by_year(df, config$year_column_name, from_year, to_year)
    print(paste0("Creating report for the period ", from_year, " to ", to_year))

    # Build network data
    network <- network(current_df,
                       vertex_column = config$author_column_name,
                       vertex_delimiter = config$author_delimiter,
                       edge_id = config$edge_id,
                       year_column = config$year_column_name)

    # Filter the network data
    network <- filter_single_vertices(network)
    if (!is.null(config$max_authors_per_paper)) {
      network <- filter_by_vertex_occurrences(network, config$max_authors_per_paper)
    }
    if (!is.null(config$min_papers_per_author)) {
      network <- filter_infrequent_vertices(network, config$min_papers_per_author)
    }

    # Build the graph
    graph <- graph(network$filtered, directed = FALSE) |>
      build(vertices = network$vertex_column, edges = network$edge_id)

    # If affiliation data is available, add it as a vertex attribute
    # Otherwise, cluster nodes with igraph's "cluster_louvain" method (communities)
    if (!is.null(config$node_properties_file_path)) {
      node_props <- utils::read.csv(config$node_properties_file_path, stringsAsFactor = FALSE)
      graph <- set_vertex_attr(graph,
                               name = config$node_color,
                               keys = node_props[[config$node_name]],
                               values = node_props[[config$node_color]])
      if (all(is.na(igraph::vertex_attr(graph$graph, config$node_color)))) {
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
        metadata = metadata,
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
