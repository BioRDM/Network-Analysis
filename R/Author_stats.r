#' @export
get_authors_per_paper <- function(data, edge_id) {
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
get_summary_stats <- function(network, graph) {
  check_column(network$filtered, network$year_column)
  check_column(network$filtered, network$edge_id)
  check_column(network$filtered, network$vertex_column)

  author_stats <- get_authors_per_paper(network$filtered, edge_id = network$edge_id)
  cutpoints <- get_cutpoints(graph)

  data.frame(
    "Start_year" = min(network$filtered[[network$year_column]]),
    "End_year" = max(network$filtered[[network$year_column]]),
    "Total_Papers" = length(unique(network$filtered[[network$edge_id]])),
    "Total_Authors" = length(unique(network$filtered[[network$author_column_name]])),
    "Average_Authors_per_Paper" = author_stats$average,
    "Median_Authors_per_Paper" = author_stats$median,
    "Min_Authors_per_Paper" = author_stats$min,
    "Max_Authors_per_Paper" = author_stats$max,
    "Density" = get_density(graph),
    "Transitivity" = get_transitivity(graph),
    "Mean_degree_centrality" = round(mean(get_centrality(graph, method = "degree")), digits = 3),
    "Mean_betweenness_centrality" = round(mean(get_centrality(graph, method = "betweenness")), digits = 3),
    "Mean_harmonic_centrality" = round(mean(get_centrality(graph, method = "harmonic")), digits = 3),
    "Mean_shortest_path" = get_average_shortest_path(graph),
    "Number_of_cutpoints" = length(cutpoints),
    "Cutpoint_authors" = paste(names(cutpoints), collapse = "; ")
  )
}
