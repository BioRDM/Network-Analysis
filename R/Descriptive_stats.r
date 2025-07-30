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

get_papers_per_author <- function(data, author_column_name = "Author") {
  data |>
    dplyr::group_by(.data[[author_column_name]]) |>
    dplyr::summarise(Papers = dplyr::n(), .groups = "drop")
}

#' @export
get_summary_stats <- function(graph, data, edge_id, author_column_name, from_year, to_year) {
  author_stats <- get_authors_per_paper(data, edge_id = edge_id)
  centrality <- get_centrality(graph)
  cutpoints <- igraph::articulation_points(graph)

  data.frame(
    "Start_year" = from_year,
    "End_year" = to_year,
    "Total_Papers" = nrow(data) |> dplyr::group_by(!!rlang::sym(author_column_name)),
    "Total_Authors" = length(unique(data[[author_column_name]])),
    "Average_Authors_per_Paper" = author_stats$average,
    "Median_Authors_per_Paper" = author_stats$median,
    "Min_Authors_per_Paper" = author_stats$min,
    "Max_Authors_per_Paper" = author_stats$max,
    "Density" = igraph::edge_density(graph),
    "Transitivity" = igraph::transitivity(graph, type = "global"),
    "Mean_degree_centrality" = round(mean(centrality$degree), digits = 3),
    "Mean_betweenness_centrality" = round(mean(centrality$betweenness), digits = 3),
    "Mean_harmonic_centrality" = round(mean(centrality$harmonic), digits = 3),
    "Mean_shortest_path" = igraph::mean_distance(graph, directed = igraph::is_directed(graph), unconnected = TRUE),
    "Number_of_cutpoints" = length(cutpoints),
    "Cutpoint_authors" = paste(cutpoints, collapse = "; ")
  )
}
