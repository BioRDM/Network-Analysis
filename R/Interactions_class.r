library(igraph)
library(intergraph)
library(statnet)

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

  # Filter data by year if specified
  if (!is.null(from_year) || !is.null(to_year)) {
    check_year_column(data, year_column_name)
    data <- filter_by_year(data, year_column_name, from_year, to_year)
  }

  # Filter papers by number of authors
  result <- filter_papers_by_authors(data,
                                     column_name = author_column_name,
                                     delimiter = author_delimiter,
                                     max_authors = max_authors_per_paper)
  data <- result[[1]]
  papers_removed <- result[[2]]
  n_papers <- nrow(data)

  # Remove authors with few papers from data
  result <- filter_infrequent_authors(data,
                                      column_name = author_column_name,
                                      delimiter = author_delimiter,
                                      min_occurrences = min_papers_per_author)
  data <- result[[1]]
  authors_removed <- result[[2]]

  # Create graph
  graph <- make_graph_from_df(data,
                              delimiter = author_delimiter,
                              column_name = author_column_name,
                              directed = directed)

  # Create network object from the graph
  network <- intergraph::asNetwork(graph)

  interactions <- list(data = data,
                       graph = graph,
                       network = network,
                       n_papers = n_papers,
                       max_authors = max_authors_per_paper,
                       min_papers = min_papers_per_author,
                       papers_removed = papers_removed,
                       authors_removed = authors_removed,
                       directed = directed)

  # Assign the class name
  class(interactions) <- "Interactions"

  return(interactions)
}

#' @export
get_cohesion.Interactions <- function(interactions) {
  interactions$dyadcount <- network::network.dyadcount(interactions$network)
  interactions$edgecount <- network::network.edgecount(interactions$network)
  interactions$size <- network::network.size(interactions$network)
  return(interactions)
}

get_cohesion <- function(interactions) {
  UseMethod("get_cohesion", interactions)
}

#' @export
get_density.Interactions <- function(interactions) {
  return(sna::gden(interactions$network))
}

get_density <- function(interactions) {
  UseMethod("get_density", interactions)
}

#' @export
get_transitivity.Interactions <- function(interactions) {
  return(sna::gtrans(interactions$network))
}

get_transitivity <- function(interactions) {
  UseMethod("get_transitivity", interactions)
}

#' @export
get_centrality.Interactions <- function(interactions) {
  degree <- igraph::degree(interactions$graph)
  closeness <- igraph::closeness(interactions$graph)
  betweenness <- igraph::betweenness(interactions$graph)
  return(list(degree = degree, closeness = closeness, betweenness = betweenness))
}

get_centrality <- function(interactions) {
  UseMethod("get_centrality", interactions)
}

#' @export
get_diameter.Interactions <- function(interactions) {
  diameter <- igraph::diameter(interactions$graph, directed = interactions$directed, weights = NULL)
  shortest_paths <- distances(interactions$graph)
  average_shortest_path <- mean(shortest_paths[shortest_paths != Inf])
  return(list(diameter = diameter, average_shortest_path = average_shortest_path))
}

get_diameter <- function(interactions) {
  UseMethod("get_diameter", interactions)
}

#' @export
get_communities.Interactions <- function(interactions) {
  return(igraph::cluster_louvain(interactions$graph))
}

get_communities <- function(interactions) {
  UseMethod("get_communities", interactions)
}

#' @export
get_most_central_per_community.Interactions <- function(interactions) {
  comm <- get_communities(interactions)
  centrality <- get_centrality(interactions)$degree
  most_central_authors <- sapply(unique(comm$membership), function(group) {
    group_vertices <- which(comm$membership == group)
    group_centrality <- centrality[group_vertices]
    most_central_vertex <- group_vertices[which.max(group_centrality)]
    igraph::V(interactions$graph)$name[most_central_vertex]
  })
  return(most_central_authors)
}

get_most_central_per_community <- function(interactions) {
  UseMethod("get_most_central_per_community", interactions)
}

#' @export
save_centrality_data.Interactions <- function(interactions, output_file = "output/centrality_data.csv") {
  centrality <- get_centrality(interactions)
  output_data <- data.frame(
    degree = centrality$degree,
    closeness = centrality$closeness,
    betweenness = centrality$betweenness
  )
  output_data <- output_data[statnet.common::order(-output_data$degree), ]
  utils::write.csv(output_data, output_file, row.names = TRUE)
}

save_centrality_data <- function(interactions, output_file) {
  UseMethod("save_centrality_data", interactions)
}
