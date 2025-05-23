#' @export
Interactions <- function(data,
                         author_delimiter = ";",
                         author_column_name = "Author",
                         year_column_name = "Year",
                         max_authors_per_paper = 50,
                         min_papers_per_author = 1,
                         directed = FALSE,
                         from_year = NULL,
                         to_year = NULL) {

  initial_papers <- nrow(data)

  result <- filter_papers_by_authors(data,
                                     column_name = author_column_name,
                                     delimiter = author_delimiter,
                                     max_authors = max_authors_per_paper)
  data <- result[[1]]
  papers_removed <- result[[2]]
  n_papers <- nrow(data)

  result <- filter_infrequent_authors(data,
                                      column_name = author_column_name,
                                      delimiter = author_delimiter,
                                      min_occurrences = min_papers_per_author)
  data <- result[[1]]
  authors_removed <- result[[2]]

  graph <- make_graph_from_df(data,
                              delimiter = author_delimiter,
                              column_name = author_column_name,
                              directed = directed)
  if (is.null(graph)) {
    return(NULL)
  }

  network <- intergraph::asNetwork(graph)
  layout_coords <- get_graph_coords(graph)

  if (!directed) {
    communities <- get_communities(graph)
  } else {
    communities <- NULL
  }

  interactions <- list(data = data,
                       author_column_name = author_column_name,
                       author_delimiter = author_delimiter,
                       year_column_name = year_column_name,
                       graph = graph,
                       network = network,
                       initial_papers = initial_papers,
                       n_papers = n_papers,
                       max_authors = max_authors_per_paper,
                       min_papers = min_papers_per_author,
                       papers_removed = papers_removed,
                       authors_removed = authors_removed,
                       directed = directed,
                       from_year = from_year,
                       to_year = to_year,
                       layout_coords = layout_coords,
                       communities = communities)

  class(interactions) <- "Interactions"

  interactions
}

#' @export
generate_network_metrics.Interactions <- function(interactions) {
  interactions$weighted <- igraph::is_weighted(interactions$graph)
  interactions$cohesion <- get_cohesion(interactions)
  interactions$density <- get_density(interactions)
  interactions$transitivity <- get_transitivity(interactions)
  interactions$centrality <- get_centrality(interactions)
  interactions$diameter <- get_diameter(interactions)
  interactions$unreachable_percentage <- get_reachability(interactions) * 100
  interactions$cutpoints <- get_cutpoints(interactions)
  interactions$top_authors_degree <- get_most_central_authors(interactions, centrality = "degree", n = 15)
  interactions$top_authors_betweenness <- get_most_central_authors(interactions, centrality = "betweenness", n = 15)
  interactions$top_authors_harmonic <- get_most_central_authors(interactions, centrality = "harmonic", n = 15)
  interactions
}

generate_network_metrics <- function(interactions) {
  UseMethod("generate_network_metrics", interactions)
}

#' @export
generate_figures.Interactions <- function(interactions, paths, date_range) {
  fig1 <- plot_graph(interactions, output_file = paste0(paths$figures, "/graph_", date_range, ".png"))
  fig2 <- plot_top_authors(interactions, n = 15, output_file = paste0(paths$figures, "/top_authors_", date_range, ".png"))
  fig3 <- plot_cutpoints(interactions, output_file = paste0(paths$figures, "/cutpoints_", date_range, ".png"))
  fig4 <- plot_graph(interactions, centrality = "betweenness", output_file = paste0(paths$figures, "/graph_betweenness_", date_range, ".png"))
  fig5 <- plot_graph(interactions, centrality = "harmonic", output_file = paste0(paths$figures, "/graph_harmonic_", date_range, ".png"))
  fig6 <- plot_graph(interactions, centrality = "none", output_file = paste0(paths$figures, "/graph_no_centrality_", date_range, ".png"))
  list(fig1 = fig1, fig2 = fig2, fig3 = fig3, fig4 = fig4, fig5 = fig5, fig6 = fig6)
}

generate_figures <- function(interactions, paths, date_range) {
  UseMethod("generate_figures", interactions)
}

#' @export
get_cohesion.Interactions <- function(interactions) {
  dyadcount <- network::network.dyadcount(interactions$network)
  edgecount <- network::network.edgecount(interactions$network)
  size <- network::network.size(interactions$network)
  list(dyadcount = dyadcount, edgecount = edgecount, size = size)
}

get_cohesion <- function(interactions) {
  UseMethod("get_cohesion", interactions)
}

#' @export
get_density.Interactions <- function(interactions) {
  sna::gden(interactions$network)
}

get_density <- function(interactions) {
  UseMethod("get_density", interactions)
}

#' @export
get_transitivity.Interactions <- function(interactions) {
  sna::gtrans(interactions$network)
}

get_transitivity <- function(interactions) {
  UseMethod("get_transitivity", interactions)
}

#' @export
get_centrality.Interactions <- function(interactions) {
  degree <- igraph::degree(interactions$graph)
  harmonic <- igraph::harmonic_centrality(interactions$graph)
  betweenness <- igraph::betweenness(interactions$graph)
  list(degree = degree, harmonic = harmonic, betweenness = betweenness)
}

get_centrality <- function(interactions) {
  UseMethod("get_centrality", interactions)
}

#' @export
get_diameter.Interactions <- function(interactions) {
  diameter <- igraph::diameter(interactions$graph, directed = interactions$directed, weights = NULL)
  shortest_paths <- igraph::distances(interactions$graph)
  average_shortest_path <- mean(shortest_paths[shortest_paths != Inf])
  list(diameter = diameter, average_shortest_path = average_shortest_path)
}

get_diameter <- function(interactions) {
  UseMethod("get_diameter", interactions)
}

#' @export
get_reachability.Interactions <- function(interactions) {
  reachability_matrix <- sna::reachability(interactions$network)
  num_nodes <- nrow(reachability_matrix)
  total_pairs <- num_nodes * (num_nodes - 1)
  (sum(reachability_matrix == 0) - num_nodes) / total_pairs
}

get_reachability <- function(interactions) {
  UseMethod("get_reachability", interactions)
}

#' @export
get_cutpoints.Interactions <- function(interactions) {
  cutpoints <- sna::cutpoints(interactions$network, mode = "graph", return.indicator = TRUE)
  cutpoint_names <- network::network.vertex.names(interactions$network)[which(cutpoints == TRUE)]

  if (length(cutpoint_names) > 0) {
    cutpoint_names |>
      format_names() |>
      sort()
  } else {
    cutpoint_names
  }
}

get_cutpoints <- function(interactions) {
  UseMethod("get_cutpoints", interactions)
}

#' @export
get_most_central_authors.Interactions <- function(interactions, centrality = "degree", n = 10) {
  centrality_metric <- switch(centrality,
                              "degree" = get_centrality(interactions)$degree,
                              "betweenness" = get_centrality(interactions)$betweenness,
                              "harmonic" = get_centrality(interactions)$harmonic)
  format_names(igraph::V(interactions$graph)$name[order(-centrality_metric)[1:n]])
}

get_most_central_authors <- function(interactions, centrality, n) {
  UseMethod("get_most_central_authors", interactions)
}

#' @export
get_most_central_per_community.Interactions <- function(interactions, centrality = "degree") {
  comm <- interactions$communities
  centrality_metric <- switch(centrality,
                              "degree" = get_centrality(interactions)$degree,
                              "betweenness" = get_centrality(interactions)$betweenness,
                              "harmonic" = get_centrality(interactions)$harmonic,
                              "none" = get_centrality(interactions)$degree)  # If no centrality is specified, use degree
  sapply(unique(comm$membership), function(group) {
    group_vertices <- which(comm$membership == group)
    group_centrality <- centrality_metric[group_vertices]
    most_central_vertex <- group_vertices[which.max(group_centrality)]
    igraph::V(interactions$graph)$name[most_central_vertex]
  })
}

get_most_central_per_community <- function(interactions, centrality) {
  UseMethod("get_most_central_per_community", interactions)
}

#' @export
save_centrality_data.Interactions <- function(interactions, output_file = "output/centrality_data.csv") {
  centrality <- get_centrality(interactions)
  output_data <- data.frame(
    degree = centrality$degree,
    harmonic = centrality$harmonic,
    betweenness = centrality$betweenness
  )
  rownames(output_data) <- format_names(igraph::V(interactions$graph)$name)
  output_data <- output_data[statnet.common::order(-output_data$degree), ]
  utils::write.csv(output_data, output_file, row.names = TRUE)
}

save_centrality_data <- function(interactions, output_file) {
  UseMethod("save_centrality_data", interactions)
}

#' @export
get_communities <- function(graph) {
  igraph::cluster_louvain(graph)
}
