#' @export
get_authors_per_paper <- function(data, edge_id) {
  check_column(data, edge_id)
  summary <- data |>
    dplyr::group_by(.data[[edge_id]]) |>
    dplyr::summarise(Authors = dplyr::n(), .groups = "drop")

  list(
    average = mean(summary$Authors),
    median = stats::median(summary$Authors),
    min = min(summary$Authors),
    max = max(summary$Authors)
  )
}

#' @export
compute_metrics <- function(network, graph = NULL, raw = FALSE) {
  if (raw) {
    data <- network$raw
  } else {
    data <- network$filtered
  }
  check_column(data, network$year_column)
  check_column(data, network$vertex_column)
  author_stats <- get_authors_per_paper(data, edge_id = network$edge_id)
  metrics <- list(
    Start_year = min(data[[network$year_column]]),
    End_year = max(data[[network$year_column]]),
    Total_Papers = length(unique(data[[network$edge_id]])),
    Total_Authors = length(unique(data[[network$vertex_column]])),
    Average_Authors_per_Paper = author_stats$average,
    Median_Authors_per_Paper = author_stats$median,
    Min_Authors_per_Paper = author_stats$min,
    Max_Authors_per_Paper = author_stats$max
  )

  if (!is.null(graph)) {
    cutpoints <- get_cutpoints(graph)
    metrics <- c(metrics, list(
      Density = get_density(graph),
      Transitivity = get_transitivity(graph),
      Mean_degree_centrality = round(mean(get_centrality(graph, method = "degree")), 3),
      Mean_betweenness_centrality = round(mean(get_centrality(graph, method = "betweenness")), 3),
      Mean_harmonic_centrality = round(mean(get_centrality(graph, method = "harmonic")), 3),
      Mean_shortest_path = get_average_shortest_path(graph),
      Number_of_cutpoints = length(cutpoints),
      Cutpoint_authors = paste(names(cutpoints), collapse = "; ")
    ))
  }
  metrics
}

#' @export
get_summary_stats <- function(network, graph) {
  raw_metrics <- compute_metrics(network, raw = TRUE)
  filtered_metrics <- compute_metrics(network, graph, raw = FALSE)

  stats_df <- dplyr::bind_rows(
    cbind(Data = "raw", as.data.frame(raw_metrics, stringsAsFactors = FALSE)),
    cbind(Data = "filtered", as.data.frame(filtered_metrics, stringsAsFactors = FALSE))
  )
  rownames(stats_df) <- NULL
  stats_df
}
